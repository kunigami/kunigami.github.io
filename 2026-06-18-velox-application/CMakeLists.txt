# This setup is for MacOS
# After changes to this file: cmake -S . -B build -G Ninja
# To build: cmake --build build -j8
# To run: build/./velox_execution

cmake_minimum_required(VERSION 3.24)
project(velox_sample CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

set(CMAKE_POLICY_VERSION_MINIMUM 3.5)

add_compile_options(-w)

# ICU location. Override with -DICU_ROOT=... or $ICU_ROOT; otherwise derive it
# from the Homebrew prefix ($HOMEBREW_PREFIX, else `brew --prefix`).
set(ICU_ROOT "$ENV{ICU_ROOT}" CACHE PATH "Path to the icu4c@78 install")
if(NOT ICU_ROOT)
  if(DEFINED ENV{HOMEBREW_PREFIX})
    set(_brew_prefix "$ENV{HOMEBREW_PREFIX}")
  else()
    execute_process(
      COMMAND brew --prefix
      OUTPUT_VARIABLE _brew_prefix
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
  set(ICU_ROOT "${_brew_prefix}/opt/icu4c@78")
endif()
list(PREPEND CMAKE_PREFIX_PATH "${ICU_ROOT}")
include_directories(SYSTEM "${ICU_ROOT}/include")
link_directories("${ICU_ROOT}/lib")

# Velox checkout. Override with -DVELOX_ROOT=... or $VELOX_ROOT; otherwise assume
# it sits alongside this repo at ../../github/velox.
set(VELOX_ROOT "$ENV{VELOX_ROOT}" CACHE PATH "Path to the Velox checkout")
if(NOT VELOX_ROOT)
  set(VELOX_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../../github/velox")
endif()
get_filename_component(VELOX_ROOT "${VELOX_ROOT}" ABSOLUTE)
# execution.cpp needs the exec engine, Hive connector, DWRF reader/writer, and
# the PlanBuilder test helper (lives in velox_exec_test_lib), so we build the
# full (non-minimal) library with testing libs enabled. Test *executables* are
# not built because the subdirectory is added EXCLUDE_FROM_ALL — only the
# library targets our binaries depend on get compiled.
set(VELOX_BUILD_TESTING ON CACHE BOOL "" FORCE)
set(VELOX_BUILD_MINIMAL OFF CACHE BOOL "" FORCE)
set(VELOX_DEPENDENCY_SOURCE BUNDLED CACHE STRING "" FORCE)
set(ICU_SOURCE SYSTEM CACHE STRING "" FORCE)

add_subdirectory(${VELOX_ROOT} ${CMAKE_BINARY_DIR}/velox EXCLUDE_FROM_ALL)

add_executable(velox_sample main.cpp)

target_link_libraries(
  velox_sample
  PRIVATE
    velox_vector
)

add_executable(velox_execution execution.cpp)

target_link_libraries(
  velox_execution
  PRIVATE
    velox_exec
    velox_exec_test_lib
    velox_hive_connector
    velox_dwio_dwrf_reader
    velox_dwio_dwrf_writer
    velox_type
    velox_vector
    velox_memory
    velox_vector_test_lib
)

add_executable(velox_reducer reducer.cpp)

target_link_libraries(
  velox_reducer
  PRIVATE
    velox_exec
    velox_exec_test_lib
    velox_presto_serializer
    velox_type
    velox_vector
    velox_memory
    velox_vector_test_lib
)

add_custom_target(
  run
  COMMAND velox_sample
  DEPENDS velox_sample
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
