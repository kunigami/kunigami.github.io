// Minimal end-to-end Velox execution example:
//
//   1. Generate an in-memory dataset and write it to a CSV (text) file (the "input").
//   2. Build a plan that reads the input file, filters rows, and writes the
//      surviving rows to a second CSV (text) file (the "output"), then read the
//      output back and print it to verify the filter took effect.
//   3. Run the SAME plan in parallel mode and verify it the same way.
//
// Plan of interest:  TableScan -> Filter -> TableWrite
//
// The serial and parallel runs share the same plan; they differ only in the
// Task's execution mode (kSerial vs. kParallel) and how the Task is driven
// (next() drain loop vs. start()/taskCompletionFuture()).
//
// Modeled on velox/examples/ScanAndSort.cpp.

#include "velox/common/base/Fs.h"
#include "velox/common/file/FileSystems.h"
#include "velox/common/memory/Memory.h"
#include "velox/common/testutil/TempDirectoryPath.h"
#include "velox/connectors/ConnectorRegistry.h"
#include "velox/connectors/hive/HiveConnector.h"
#include "velox/connectors/hive/HiveConnectorSplit.h"
#include "velox/common/future/VeloxPromise.h"
#include "velox/dwio/common/FileSink.h"
#include "velox/dwio/text/RegisterTextReader.h"
#include "velox/dwio/text/RegisterTextWriter.h"
#include "velox/exec/Driver.h"
#include "velox/exec/Task.h"
#include "velox/exec/tests/utils/PlanBuilder.h"
#include "velox/functions/prestosql/registration/RegistrationFunctions.h"
#include "velox/parse/TypeResolver.h"
#include "velox/type/Type.h"
#include "velox/vector/FlatVector.h"

#include <folly/init/Init.h>
#include <folly/system/HardwareConcurrency.h>

#include <fstream>
#include <iostream>

using namespace facebook::velox;
using namespace facebook::velox::common::testutil;

namespace {

// Connector id shared by the registered connector and every scan/write/split.
const std::string kFileConnectorId = "test-hive";

// Field delimiter of the hand-written input CSV (writeCsv emits commas).
constexpr char kInputDelim = ',';
// Field delimiter the Velox TEXT writer actually produces. The writer ignores
// the field.delim serde parameter (TextWriterFactory::createWriter never plumbs
// it to TextWriter), so TableWrite output is always Hive's default ^A (0x01).
constexpr char kTextWriterDelim = '\x01';

// Builds a Hive split for a single local TEXT file, splitting fields on
// 'fieldDelim'. The "file:" prefix selects the local filesystem. Hive text
// defaults to ^A (0x01); our input CSV uses ',', while the TableWrite output is
// ^A-delimited (the TEXT writer ignores the field.delim serde param and always
// emits ^A), so callers must pass the delimiter that matches the file at hand.
std::shared_ptr<connector::hive::HiveConnectorSplit> buildSplit(
    const std::string& filePath,
    char fieldDelim) {
  return connector::hive::HiveConnectorSplitBuilder("file:" + filePath)
      .connectorId(kFileConnectorId)
      .fileFormat(dwio::common::FileFormat::TEXT)
      .serdeParameters({{"field.delim", std::string(1, fieldDelim)}})
      .build();
}

// Adds every file in 'dir' to 'task' as a Hive split feeding 'scanNodeId',
// reading each with 'fieldDelim' as the field separator.
void addSplits(
    const std::shared_ptr<exec::Task>& task,
    const core::PlanNodeId& scanNodeId,
    const std::string& dir,
    char fieldDelim) {
  for (auto& entry : fs::directory_iterator(dir)) {
    task->addSplit(
        scanNodeId,
        exec::Split{buildSplit(entry.path().string(), fieldDelim)});
  }
  task->noMoreSplits(scanNodeId);
}

// Drains a task that produces no output (e.g. a write fragment).
void runToCompletion(const std::shared_ptr<exec::Task>& task) {
  while (auto result = task->next()) {
  }
}

// Builds the plan of interest: TableScan -> Filter (class = 'Mammal') ->
// TableWrite, writing comma-delimited TEXT to 'outputDir'. The scan's node id
// is returned via 'scanNodeId' so the caller can attach splits to it.
core::PlanFragment buildScanFilterWritePlan(
    const RowTypePtr& schema,
    const std::string& outputDir,
    core::PlanNodeId& scanNodeId) {
  return exec::test::PlanBuilder()
      .startTableScan()
      .connectorId(kFileConnectorId)
      .outputType(schema)
      // TEXT/CSV files carry no embedded schema, so the reader needs it
      // supplied explicitly via dataColumns.
      .dataColumns(schema)
      .endTableScan()
      .capturePlanNodeId(scanNodeId)
      .filter("\"class\" = 'Mammal'")
      .startTableWriter()
      .connectorId(kFileConnectorId)
      .outputDirectoryPath(outputDir)
      .fileFormat(dwio::common::FileFormat::TEXT)
      .endTableWriter()
      .planFragment();
}

// Builds the animal dataset as an in-memory Velox RowVector with two VARCHAR
// columns: species and class. Strings are copied into the vector's own buffers.
RowVectorPtr makeData(memory::MemoryPool* pool) {
  const std::vector<std::pair<std::string, std::string>> animals = {
      {"Axolotl", "Amphibian"},
      {"Fennec Fox", "Mammal"},
      {"Aardvark", "Mammal"},
      {"Shoebill", "Bird"},
      {"Leafy Seadragon", "Fish"},
      {"Quokka", "Mammal"},
  };
  const vector_size_t size = animals.size();

  auto species =
      BaseVector::create<FlatVector<StringView>>(VARCHAR(), size, pool);
  auto klass =
      BaseVector::create<FlatVector<StringView>>(VARCHAR(), size, pool);
  for (vector_size_t i = 0; i < size; ++i) {
    species->set(i, StringView(animals[i].first));
    klass->set(i, StringView(animals[i].second));
  }

  return std::make_shared<RowVector>(
      pool,
      ROW({{"species", VARCHAR()}, {"class", VARCHAR()}}),
      /*nulls=*/nullptr,
      size,
      std::vector<VectorPtr>{species, klass});
}

// Writes 'data' as a plain comma-delimited CSV (one row per line, no header) to
// 'path'. Because Hive text defaults to a ^A (0x01) field delimiter, the scan
// passes field.delim="," so the TEXT reader splits these rows on commas.
// Assumes every column is VARCHAR.
void writeCsv(const RowVectorPtr& data, const std::string& path) {
  std::ofstream csv(path);
  for (vector_size_t row = 0; row < data->size(); ++row) {
    for (size_t col = 0; col < data->childrenSize(); ++col) {
      if (col > 0) {
        csv << ",";
      }
      csv << data->childAt(col)->asFlatVector<StringView>()->valueAt(row).str();
    }
    csv << "\n";
  }
}

// Registers all global components the plans below depend on: the Hive
// connector, the local filesystem, the TEXT reader/writer factories, the
// Presto scalar functions used by the filter, and the SQL type resolver.
void registerDependencies() {
  connector::hive::HiveConnectorFactory factory;
  auto connectorConfig = std::make_shared<config::ConfigBase>(
      std::unordered_map<std::string, std::string>());
  std::shared_ptr<connector::Connector> hiveConnector =
      factory.newConnector(kFileConnectorId, connectorConfig);
  connector::ConnectorRegistry::global().insert(
      hiveConnector->connectorId(), hiveConnector);

  filesystems::registerLocalFileSystem();
  text::registerTextReaderFactory();

  dwio::common::registerFileSinks();
  text::registerTextWriterFactory();

  // The filter ("class" = 'Mammal') is parsed from SQL, so we need the scalar
  // functions it references and a type resolver hook to type the expression.
  functions::prestosql::registerAllScalarFunctions();
  parse::registerTypeResolver();
}

// Creates a serial-execution Task for 'plan'. Serial mode runs the drivers on
// the caller's thread inside Task::next(), so no executor is needed.
std::shared_ptr<exec::Task> makeTask(
    const std::string& id,
    const core::PlanFragment& plan) {
  return exec::Task::create(
      id,
      plan,
      /*destination=*/0,
      core::QueryCtx::create(),
      exec::Task::ExecutionMode::kSerial,
      exec::Consumer{});
}

// Creates a parallel-execution Task for 'plan'. Parallel mode runs the Drivers
// on 'executor', so a (non-inline) executor is required.
//
// Unlike serial mode (which pulls output via next()), a parallel Driver may not
// let the last operator's output fall through unconsumed -- it must go to a
// callback or another operator. TableWrite is not a no-output sink: it emits
// commit metadata (row counts, written file fragments). So we pass a Consumer
// that simply drains and ignores that metadata.
std::shared_ptr<exec::Task> makeParallelTask(
    folly::Executor* executor,
    const std::string& id,
    const core::PlanFragment& plan) {
  auto drain = [](RowVectorPtr /*data*/,
                  bool /*drained*/,
                  ContinueFuture* /*future*/) {
    return exec::BlockingReason::kNotBlocked;
  };
  return exec::Task::create(
      id,
      plan,
      /*destination=*/0,
      core::QueryCtx::create(executor),
      exec::Task::ExecutionMode::kParallel,
      std::move(drain));
}

// Drives a parallel write task to completion. Mirrors the serial sequence
// (add splits, drain) but with the parallel ordering: start the Drivers first,
// then feed splits, then block on taskCompletionFuture(). 'maxDrivers' caps the
// number of Drivers (pipeline instances), which are cooperatively scheduled
// onto the executor's thread pool -- they're not dedicated OS threads.
void runToCompletionParallel(
    const std::shared_ptr<exec::Task>& task,
    const core::PlanNodeId& scanNodeId,
    const std::string& inputDir) {
  task->start(/*maxDrivers=*/4);
  addSplits(task, scanNodeId, inputDir, kInputDelim);
  task->taskCompletionFuture().wait();
}

// Reads every file in 'dir' back with a serial scan-only plan and prints each
// row under 'label'. Shared by the serial and parallel examples to verify their
// written output the same way.
void readAndPrint(
    const RowTypePtr& schema,
    const std::string& dir,
    const std::string& label,
    char fieldDelim) {
  core::PlanNodeId scanId;
  auto plan = exec::test::PlanBuilder()
                  .startTableScan()
                  .connectorId(kFileConnectorId)
                  .outputType(schema)
                  .dataColumns(schema) // TEXT needs an explicit file schema
                  .endTableScan()
                  .capturePlanNodeId(scanId)
                  .planFragment();
  auto task = makeTask("read-output", plan);
  addSplits(task, scanId, dir, fieldDelim);

  std::cout << label << "\n";
  while (auto result = task->next()) {
    for (vector_size_t i = 0; i < result->size(); ++i) {
      std::cout << "  " << result->toString(i) << "\n";
    }
  }
}

} // namespace

int main(int argc, char** argv) {
  // Velox Tasks/Operators run on folly's async framework; initialize it first.
  folly::Init init{&argc, &argv};

  memory::MemoryManager::initialize(memory::MemoryManager::Options{});

  // ---- Build the dataset as a Velox vector; derive the schema from it -------
  auto pool = memory::memoryManager()->addLeafPool();
  auto data = makeData(pool.get());
  auto schema = asRowType(data->type());

  // ---- Register connector, filesystem, and TEXT read/write factories ------
  registerDependencies();

  // Temp dirs: the input file, the serial output, and the parallel output.
  auto inputDir = TempDirectoryPath::create();
  auto serialOutputDir = TempDirectoryPath::create();
  auto parallelOutputDir = TempDirectoryPath::create();

  // ---- Step 1: write the dataset out as a plain comma-CSV file -------------
  const std::string inputCsvPath = inputDir->getPath() + "/animals.csv";
  writeCsv(data, inputCsvPath);

  // ---- Step 2 (SERIAL): scan -> filter -> write ---------------------------
  // Build the plan, create a serial task, feed splits, drain via next().
  core::PlanNodeId serialScanId;
  auto serialPlan =
      buildScanFilterWritePlan(schema, serialOutputDir->getPath(), serialScanId);
  auto serialTask = makeTask("serial-scan-filter-write", serialPlan);
  addSplits(serialTask, serialScanId, inputDir->getPath(), kInputDelim);
  runToCompletion(serialTask);
  readAndPrint(
      schema,
      serialOutputDir->getPath(),
      "Serial filtered output (class = 'Mammal'):",
      kTextWriterDelim);

  // ---- Same plan, executed in PARALLEL (kParallel) ------------------------
  // Identical plan; the only differences are the Task's execution mode (and its
  // executor) and how it's driven: start() the Drivers, feed splits, then wait.
  auto executor = std::make_shared<folly::CPUThreadPoolExecutor>(
      folly::available_concurrency());
  core::PlanNodeId parallelScanId;
  auto parallelPlan = buildScanFilterWritePlan(
      schema, parallelOutputDir->getPath(), parallelScanId);
  auto parallelTask =
      makeParallelTask(executor.get(), "parallel-scan-filter-write", parallelPlan);
  runToCompletionParallel(parallelTask, parallelScanId, inputDir->getPath());
  readAndPrint(
      schema,
      parallelOutputDir->getPath(),
      "Parallel filtered output (class = 'Mammal'):",
      kTextWriterDelim);
  std::cout << "Finished!" << std::endl;
  return 0;
}
