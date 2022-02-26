#include <AudioToolbox/AudioToolbox.h>

#include <fstream>
#include <iostream>

struct Options {
  double bufferSizeInSecs;
  int bufferCount;
};

// generic error handler - if error is nonzero, prints error message and exits
// program.
static void CheckError(OSStatus error, const char *operation) {
  if (error == noErr) return;

  char errorString[20];
  // see if it appears to be a 4-char-code
  *(UInt32 *)(errorString + 1) = CFSwapInt32HostToBig(error);
  if (isprint(errorString[1]) && isprint(errorString[2]) &&
      isprint(errorString[3]) && isprint(errorString[4])) {
    errorString[0] = errorString[5] = '\'';
    errorString[6] = '\0';
  } else
    // no, format it as an integer
    sprintf(errorString, "%d", (int)error);

  fprintf(stderr, "Error: %s (%s)\n", operation, errorString);

  exit(1);
}

/**
 * Abstraction over AudioQueue for getting PCM data (via the OnReceiveData()
 * callback). Needs to be implemented by children class.
 */
class PCMQueue {
 private:
  // Whether the audio queue is running
  bool running_ = FALSE;
  // Audio format the queue is working with
  AudioStreamBasicDescription format_ = {0};
  AudioQueueRef queue_ = {0};

  void CallbackWithThis(AudioQueueRef inQueue, AudioQueueBufferRef inBuffer,
                        UInt32 inNumPackets) {
    OnReceiveData(inBuffer, inNumPackets);

    // if we're not stopping, re-enqueue the buffer so that it gets filled again
    if (running_)
      CheckError(AudioQueueEnqueueBuffer(inQueue, inBuffer, 0, NULL),
                 "AudioQueueEnqueueBuffer failed");
  }

  static void CallbackWithoutThis(
      void *inUserData, AudioQueueRef inQueue, AudioQueueBufferRef inBuffer,
      const AudioTimeStamp *inStartTime, UInt32 inNumPackets,
      const AudioStreamPacketDescription *inPacketDesc) {
    static_cast<PCMQueue *>(inUserData)
        ->CallbackWithThis(inQueue, inBuffer, inNumPackets);
  }

  void RegisterCallback() {
    CheckError(AudioQueueNewInput(&format_, *PCMQueue::CallbackWithoutThis,
                                  (void *)this, NULL, NULL, 0, &queue_),
               "AudioQueueNewInput failed");
  }

  void AddBuffer(int sizeInBytes) {
    AudioQueueBufferRef buffer;
    CheckError(AudioQueueAllocateBuffer(queue_, sizeInBytes, &buffer),
               "AudioQueueAllocateBuffer failed");
    CheckError(AudioQueueEnqueueBuffer(queue_, buffer, 0, NULL),
               "AudioQueueEnqueueBuffer failed");
  }

  void InitializePCMFormat(AudioStreamBasicDescription *format) {
    memset(format, 0, sizeof(*format));
    format->mFormatID = kAudioFormatLinearPCM;
    format->mFormatFlags = kAudioFormatFlagIsSignedInteger;
    format->mChannelsPerFrame = 1;
    format->mFramesPerPacket = 1;
    format->mBitsPerChannel = 32;
    format->mBytesPerFrame = 4;
    format->mBytesPerPacket = 4;
    format->mReserved = 0;

    AudioDeviceID deviceID = GetDefaultInputDeviceID();
    format->mSampleRate = GetDeviceSampleRate(deviceID);
  }

  AudioDeviceID GetDefaultInputDeviceID() {
    AudioDeviceID deviceID = 0;

    AudioObjectPropertyAddress propertyAddress = {
        .mSelector = kAudioHardwarePropertyDefaultInputDevice,
        .mScope = kAudioObjectPropertyScopeGlobal,
        .mElement = 0,
    };
    UInt32 propertySize = sizeof(AudioDeviceID);
    OSStatus error =
        AudioObjectGetPropertyData(kAudioObjectSystemObject, &propertyAddress,
                                   0, NULL, &propertySize, &deviceID);
    if (error) throw error;

    return deviceID;
  }

  Float64 GetDeviceSampleRate(AudioDeviceID deviceID) {
    Float64 outSampleRate = 0;
    AudioObjectPropertyAddress propertyAddress = {
        .mSelector = kAudioDevicePropertyNominalSampleRate,
        .mScope = kAudioObjectPropertyScopeGlobal,
        .mElement = 0,
    };
    UInt32 propertySize = sizeof(Float64);
    OSStatus error = AudioObjectGetPropertyData(
        deviceID, &propertyAddress, 0, NULL, &propertySize, &outSampleRate);
    if (error) throw error;

    return outSampleRate;
  }

  void InitializePCMFormatWithQueue() {
    UInt32 size = sizeof(format_);

    CheckError(AudioQueueGetProperty(
                   queue_, kAudioConverterCurrentOutputStreamDescription,
                   &format_, &size),
               "couldn't get queue's format");
  }

  void AddBuffers(const Options *options) {
    // How many samples we'll have to store in the queue
    int bufferByteSize =
        (int)ceil(options->bufferSizeInSecs * format_.mSampleRate);

    for (int bufferIndex = 0; bufferIndex < options->bufferCount;
         ++bufferIndex) {
      AddBuffer(bufferByteSize);
    }
  }

 protected:
  virtual void OnReceiveData(AudioQueueBufferRef inBuffer,
                             UInt32 inNumPackets) = 0;

 public:
  PCMQueue(const Options *options) {
    InitializePCMFormat(&format_);

    RegisterCallback();

    // According to the Learning Core Audio Book (Adamson, Avila), some fields
    // of the format (ASBD) can only be filled when the queue is has registered
    // its input.
    InitializePCMFormatWithQueue();
    AddBuffers(options);
  }

  ~PCMQueue() {
    running_ = FALSE;
    AudioQueueDispose(queue_, TRUE);
  }

  void Start() {
    running_ = TRUE;
    CheckError(AudioQueueStart(queue_, NULL), "AudioQueueStart failed");
  }

  void Stop() {
    running_ = FALSE;
    CheckError(AudioQueueStop(queue_, TRUE), "AudioQueueStop failed");
  }
};

/**
 * Implementation of the PCMQueue that writes the PCM data to disk.
 */
class PCMWriter : public PCMQueue {
 private:
  std::ofstream samplesFile;

 public:
  PCMWriter(const Options *options) : PCMQueue(options) {
    samplesFile = std::ofstream("samples.csv");
  }

  void OnReceiveData(AudioQueueBufferRef inBuffer, UInt32 inNumPackets) {
    if (inNumPackets <= 0 || !samplesFile.is_open()) return;

    int *amplitudes = (int *)inBuffer->mAudioData;
    for (int i = 0; i < inNumPackets; i++) {
      samplesFile << amplitudes[i] << std::endl;
    }
  }
};

int main(int argc, const char *argv[]) {
  const Options options = {.bufferSizeInSecs = 0.5, .bufferCount = 3};
  auto queue = new PCMWriter(&options);

  queue->Start();

  printf("Recording, press <return> to stop:\n");
  getchar();

  queue->Stop();

  delete queue;

  return 0;
}
