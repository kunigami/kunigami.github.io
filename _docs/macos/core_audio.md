---
layout: doc
title: "Core Audio"
---

# Index
{:.no_toc}

1. TOC
{:toc}


## High-Level Concepts

Alphabetical order.

### Audio Format

Corresponds to how samples are structured in memory/disk. Examples include FLAC, MP3 and PCM.

* **Lossiness**: lossy formats might simplify data in order to save space, e.g. MP3 vs. FLAC (lossless)
* **Compression**: compressed formats might group frames together to save space, e.g. FLAC vs. PCM (noncompressed)

### Audio Queue

Responsible for getting buffers from an **Buffer Queue** and supplying it to a registered callback.

### Buffer

Includes a segment of memory holding audio data and metadata about this segment.

### Buffer Queue

Literally a queue (FIFO) of **Buffer**s.

### Channel

Analogous to channel in images (RGB), digital audio can have channels for the left and right speakers, while more advanced 5.1 surround-sound formats might have 6.

### Frame

Analogous to a pixel in image. It contains the amplitudes of each **Channel** of a **Sample**.

### Packets

Compressed formats might groups several **Frame**s together in a packet. Different packets might have different number of frames.

For uncompressed formats it's assume the number of frame per packet is 1.

### Sample

At a high-level, sample corresponds to a discrete snapshot of a signal at a specific point in time. More concretely we can see it as the signal's amplitude(s) + the timestamp.

### Sample Rate

Frequency in which **Sample**s are obtained from analog signal (given in Hz).

## Code Conventions

It helps to be aware of code conventions, especially naming, since they provide hints about the functionality.

* `k.*` - indicates c(k)onstants
* `m.*` - indicates a member (e.g. from a struct)

## Glossary

* `AudioStreamBasicDescription` (ASBD) - describes a format (e.g. MP3)

## Data Structures

### AudioObjectPropertyAddress

Selected Fields:

* `mSelector: AudioObjectPropertySelector` - enum representing the type of property
    * Examples:
        * `kAudioHardwarePropertyDefaultInputDevice`: default input device, e.g. mic
        * `kAudioDevicePropertyNominalSampleRate`: sample rate of the input device


### AudioQueueRef

See *Audio Queue*.

### AudioStreamBasicDescription

Selected Fields:

* `mFormatID: AudioFormatID` - enum representing the format
    * Examples:
        * `kAudioFormatMPEG4AAC` (aac)
        * `kAudioFormatMPEGLayer3` (.mp3)
* `mChannelsPerFrame: int` - see *Channel*
* `mSampleRate: float` - see *Sample Rate*

## API

### AudioFormatGetProperty

{% highlight cpp %}
#include <CoreAudio/AudioFormat.h>

OSStatus AudioFormatGetProperty(
    AudioFormatPropertyID inPropertyID,
    UInt32 inSpecifierSize,
    const void * __nullable inSpecifier,
    UInt32 * __nullable ioPropertyDataSize,
    void * __nullable outPropertyData);
{% endhighlight %}

**Example 1:** complete the information about an ASBD when the format is set

{% highlight cpp %}
AudioStreamBasicDescription recordFormat = {0};
recordFormat.mFormatID = kAudioFormatMPEG4AAC;
// ... set other information ...

UInt32 propSize = sizeof(recordFormat);
AudioFormatGetProperty(
    kAudioFormatProperty_FormatInfo,
    0,
    NULL,
    &propSize,
    &recordFormat);
{% endhighlight %}

### AudioHardwareServiceGetPropertyData (deprecated)

Deprecated in [10.5](https://developer.apple.com/library/archive/technotes/tn2223/_index.html). See `AudioObjectGetPropertyData`.

### AudioObjectGetPropertyData

This is a generic getter for some specific property that is related to the hardware. We can think of this as calling the getter of a method from an object.

So if we have `value = my_object.get_property()`, then `inObjectID = my_object`, `inAddress = get_property`, `outData = value`.

Note we need to know the size of the property value upfront (`ioDataSize`).

{% highlight cpp %}
#include <CoreAudio/AudioHardware.h>

OSStatus AudioObjectGetPropertyData(
    AudioObjectID inObjectID,
    const AudioObjectPropertyAddress *inAddress,
    UInt32 inQualifierDataSize,
    const void *inQualifierData,
    UInt32 *ioDataSize,
    void *outData,
);
{% endhighlight %}

**Example 1:** Get a reference to the default input device object (microphone)

{% highlight cpp %}
AudioDeviceID deviceID = 0;

AudioObjectPropertyAddress propertyAddress;
propertyAddress.mSelector = kAudioHardwarePropertyDefaultInputDevice;
propertyAddress.mScope = kAudioObjectPropertyScopeGlobal;
propertyAddress.mElement = 0;

UInt32 propertySize = sizeof(AudioDeviceID);

error = AudioObjectGetPropertyData(
    kAudioObjectSystemObject,
    &propertyAddress,
    0,
    NULL,
    &propertySize,
    &deviceID);
{% endhighlight %}

**Example 2:** Get the sample rate from a device object. `deviceID` was populated from the call in *Example 1*.

{% highlight cpp %}
AudioObjectPropertyAddress propertyAddress;
propertyAddress.mSelector = kAudioDevicePropertyNominalSampleRate;
propertyAddress.mScope = kAudioObjectPropertyScopeGlobal;
propertyAddress.mElement = 0;

UInt32 propertySize = sizeof(Float64);

error = AudioObjectGetPropertyData(
    deviceID,
    &propertyAddress,
    0,
    NULL,
    &propertySize,
    outSampleRate);
{% endhighlight %}

### AudioQueueGetProperty

We can pass a ASBD and this function will fill the structure with information that can only be known once the queue is created.

{% highlight cpp %}
OSStatus AudioQueueGetProperty(
    AudioQueueRef inAQ,
    AudioQueuePropertyID inID,
    void * outData,
    UInt32 * ioDataSize
)
{% endhighlight %}


### AudioQueueNewInput

Creates a new audio queue for recording audio data. The `inFormat` corresponds to the audio format which is going to be used.

The `inCallbackProc` is the function which will be invoked when a audio is ready to be processed. `inUserData` is a parameter we can send to our callback.

`outAQ` is the reference to the queue, which will be initialized.

{% highlight cpp %}
OSStatus AudioQueueNewInput(
    const AudioStreamBasicDescription *inFormat,
    AudioQueueInputCallback inCallbackProc,
    void * __nullable inUserData,
    CFRunLoopRef __nullable inCallbackRunLoop,
    CFStringRef __nullable inCallbackRunLoopMode,
    UInt32 inFlags,
    AudioQueueRef __nullable * __nonnull outAQ)
{% endhighlight %}
