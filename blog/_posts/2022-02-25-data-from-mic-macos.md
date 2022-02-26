---
layout: post
title: "Audio Data from Microphone in MacOS"
tags: [lean]
excerpt_separator: <!--more-->
vanity: "2022-02-25-data-from-mic-macos"

---
{% include blog_vars.html %}

In this post we'll learn how to obtain audio data from a microphone in MacOS, using Audio Queues.

<!--more-->

This is heavily based on the chapter *Recording* from *Learning Core Audio* book by Adamson and Avila [1], which describes how to record audio to an AAC (Advanced Audio Coding format).

The main difference is that in our post we are interested in intercepting the actual audio samples instead of writing straight to disk.

To that end, we'll use the PCM (Pulse-code modulation) audio format instead of AAC, since the former is uncompressed, which is needed if we are to have access to the raw samples.

Finally, we'll use C++ classes in our code, while [1] uses plain C.

## Terminology

We'll rely on a few key concepts in this post which is worth defining.

**Sample.** At a high-level, sample corresponds to a discrete snapshot of a signal at a specific point in time. More concretely we can see it as the signal’s amplitude(s) + the timestamp.

**Sample Rate.** Frequency in which Samples are obtained from analog signal (given in Hz).

**Channel.** Analogous to channel in images (RGB), digital audio can have channels for the left and right speakers, while more advanced 5.1 surround-sound formats might have 6.

**Frame.** Analogous to a pixel in image, it contains the amplitudes of each channel of a sample.

**Packet.** In compressed formats it might groups several frames together in a packet. Different packets might have different number of frames. For uncompressed formats it’s assume the number of frame per packet is 1.

For a more comprehensive terminology list, check the [Core Audio cheat sheet]({{site.url}}/docs/macos/core_audio.html).

## Audio Format

Apple's Audio APIs can work with a variety of file formats. The metadata about formats is stored in the structure `AudioStreamBasicDescription` (sometimes abbreviated as ASBD). It can represent uncompressed or compressed, lossless or lossy formats.

For the compressed formats it might need additional metadata since it often groups samples in non-uniform packet sizes. But in this post we'll use PCM, which simplifies the metadata a bit.

To get started, let's define a function to fill-in a `AudioStreamBasicDescription` with PCM settings:

{% highlight c++ %}
void InitializePCMFormat(AudioStreamBasicDescription *format) {
  memset(format, 0, sizeof(*format));
  format->mFormatID = kAudioFormatLinearPCM;
  format->mFormatFlags = kAudioFormatFlagIsSignedInteger;
  format->mFramesPerPacket = 1;
  format->mChannelsPerFrame = 1;
  format->mBitsPerChannel = 32;
  format->mBytesPerFrame = 4;
  format->mBytesPerPacket = 4;
  format->mReserved = 0;

  AudioDeviceID deviceID = GetDefaultInputDeviceID();
  format->mSampleRate = GetDeviceSampleRate(deviceID);
}
{% endhighlight %}

Let's discuss some of the fields. `mFormatID = kAudioFormatLinearPCM` indicates this metadata is describing the PCM format. In PCM there's no compression so each packet has exactly one frame, so `mFramesPerPacket = 1`. The microphone I'm using only has one channel, so `mChannelsPerFrame = 1`.

We'll use a 32-bit integer to represent the amplitude of the sample, so `mFormatFlags = kAudioFormatFlagIsSignedInteger` and `mBitsPerChannel = 32`.

In the case of PCM some properties are derived, such as `mBytesPerFrame = mBitsPerChannel / 8 * mChannelsPerFrame` and `mBytesPerPacket = mBytesPerFrame * mFramesPerPacket`.

The sample rate is set based on the input device. First we get the ID of the default input microphone using the `AudioObjectGetPropertyData` API:

{% highlight c++ %}
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
{% endhighlight %}

Once we have the device ID, we can use `AudioObjectGetPropertyData` again to get its sample rate:

{% highlight c++ %}
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
{% endhighlight %}


## The Audio Queue

The `AudioQueue` is a data structure from Apple's Audio API. It implements a message queue that is useful to connect a source (in our case a microphone) with a destination (in our case our callback).

The high-level operation of the queue is that it will receive samples from the microphone and accumulate them in buffers. Once the buffer fills up, it will invoke a callback. The major reason for this is debouncing: we don't have to invoke the callback on every sample received, which could be expensive.

The `AudioQueue` keeps a queue of $n$ buffers (configurable) so that it can send the buffer to the callback as soon as it fills up, and start working on the next buffer.

In theory 2 buffers would suffice but having more helps with temporary variability in throughput. For example, suppose the callback became temporarily slow due to some I/O issue. If we only had two buffers, the input might fill up its own buffer, but if there were more buffers it could start working on it.

An alternative would be to increase the buffer size but then the callback would be called less often which could be disruptive if it's a real-time application (e.g. playback).

### PCM Queue Class

As mentioned earlier, we'll create a class to represent a PCM queue wrapping the Audio APIs. The overall structure of our class is:

{% highlight c++ %}
class PCMQueue {
 private:
  // Whether the audio queue is running
  bool running_ = FALSE;
  // Audio format the queue is working with
  AudioStreamBasicDescription format_ = {0};
  AudioQueueRef queue_ = {0};

  ...
};
{% endhighlight %}

### Callback

The first thing we'll do is to register the callback via `AudioQueueNewInput()`. Among other things, it takes the audio format structure (`format`), a pointer to the callback (`PCMQueue::CallbackWithoutThis`), the additional input for it (`this`) and the queue itself (`queue_`, a member variable).

{% highlight c++ %}
void RegisterCallback() {
  CheckError(AudioQueueNewInput(format_, *PCMQueue::CallbackWithoutThis,
                                (void *)this, NULL, NULL, 0, &queue_),
              "AudioQueueNewInput failed");
}
{% endhighlight %}

We can't pass instance methods as function pointers, but we pass `this` as additional input and then invoke a method inside `CallbackWithoutThis()`:

{% highlight c++ %}
void CallbackWithoutThis(
    void *inUserData, AudioQueueRef inQueue, AudioQueueBufferRef inBuffer,
    ..., UInt32 inNumPackets, ...) {
  static_cast<PCMQueue *>(inUserData)
      ->CallbackWithThis(inQueue, inBuffer, inNumPackets);
}
{% endhighlight %}

This helps converting from a plain-C API into a more OOP version. In `CallbackWithThis()` we call a virtual method `OnReceiveData()` to be implemented by a derived class, so that we can keep this as a generic PCM queue without specific business logic.

This function also re-enqueues the buffer once it's consumed so that the source can pick it up, hiding this detail from the derived class.

{% highlight c++ %}
 ...
 protected:
  virtual void OnReceiveData(AudioQueueBufferRef inBuffer,
                             UInt32 inNumPackets) = 0;

 public:
  ...
  void CallbackWithThis(AudioQueueRef inQueue, AudioQueueBufferRef inBuffer,
                      UInt32 inNumPackets) {
    OnReceiveData(inBuffer, inNumPackets);

    // Re-enqueue the buffer so that it gets filled again
    if (running_) {
      CheckError(AudioQueueEnqueueBuffer(inQueue, inBuffer, 0, NULL),
                  "AudioQueueEnqueueBuffer failed");
    }
  }
  ...
{% endhighlight %}

### Completing Audio Format Initialization

Once the queue is setup, we can set some extra metadata in the `AudioStreamBasicDescription`. I have no idea what this is doing exactly but if I omit this the recorded audio is a lot more noisy.

In [1] the authors mention it might be setting some codecs, but from a quick inspection of the fields of `format_` I couldn't find what changed.

{% highlight c++ %}
void InitializePCMFormatWithQueue() {
  UInt32 size = sizeof(format_);

  CheckError(AudioQueueGetProperty(
                  queue_, kAudioConverterCurrentOutputStreamDescription,
                  &format_, &size),
              "couldn't get queue's format");
}
{% endhighlight %}


### Buffer Allocation

We need to add the buffers to the queue. We parametrize (via `Options`) this function with the number of buffers and the corresponding amount of time it can hold data for.

{% highlight c++ %}
struct Options {
  double bufferSizeInSecs;
  int bufferCount;
};

// ...

void AddBuffers(const Options *options) {
  // How many samples we'll have to store in the queue
  int bufferByteSize =
      (int)ceil(options->bufferSizeInSecs * format_.mSampleRate);

  for (int bufferIndex = 0; bufferIndex < options->bufferCount;
        ++bufferIndex) {
    AddBuffer(bufferByteSize);
  }
}
{% endhighlight %}

The function `AddBuffer()` creates and enqueues a buffer of given size using the `AudioQueue` API.

{% highlight c++ %}
void AddBuffer(int sizeInBytes) {
  AudioQueueBufferRef buffer;
  CheckError(AudioQueueAllocateBuffer(queue_, sizeInBytes, &buffer),
              "AudioQueueAllocateBuffer failed");
  CheckError(AudioQueueEnqueueBuffer(queue_, buffer, 0, NULL),
              "AudioQueueEnqueueBuffer failed");
}
{% endhighlight %}


### Queue Lifecycle

Creating, starting, stopping and destroying the queue is simple given the methods defined above and other `AudioQueue` API:

{% highlight c++ %}
class PCMQueue {
  ...
 public:
  PCMQueue(const Options *options) {
    InitializePCMFormat(&format_);
    RegisterCallback();
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
{% endhighlight %}

## Writing To File

We can subclass `PCMQueue` to provide an implementation for the `OnReceiveData()` which appends the samples to a file.

{% highlight c++ %}
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
{% endhighlight %}

## Application

To put everything together we define a `main()` function and block the thread until *Enter* is typed.

{% highlight c++ %}
int main(int argc, const char *argv[]) {
  const Options options = {.bufferSizeInSecs = 0.5, .bufferCount = 3};
  auto queue = new PCMWriter(&options);
  queue->Start();

  printf("Listening to mic, press <return> to stop:\n");
  getchar();

  queue->Stop();
  delete queue;
  return 0;
}
{% endhighlight %}

## Visualizing

To verify the samples we collected make sense, we can convert those amplitudes to `wav` format using Python. We only need to take note of the sample rate when running the C++ code.

{% highlight python %}
from scipy.io import wavfile

# Must match the one from the mic
sample_rate = 48000
filename = "samples.csv"
with open(filename, "r") as file:
    samples = [float(sample) for sample in file.readlines()]

samples = np.array(samples)
samples /= np.max(np.abs(samples))
wavfile.write("samples.wav", sample_rate, samples)
{% endhighlight %}

## Code

The full C++ code is available on [Github]({{github}}/) and can be build using `make build` (you probably need the MacOS SDK that ships with XCode).

## Conclusion

In this post we learned how to "intercept" samples from a microphone in MacOS.

For this particular case we wrote the data to file, which is not much different from chapter *Recording* in [1], but being able to get the raw audio samples in real-time is a key ingredient for doing processing, such as speech recognition, on the fly.

## References

* [1] Learning Core Audio, C. Adamson and K. Avila
