---
layout: post
title: "A Simple Key Logger for MacOS"
tags: [c++]
vanity: "2021-11-16-a-simple-key-logger"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In this post we'll build a simple key logger for MacOS. I consists of two parts: a daemon (backend) and a UI (frontend). The daemon is basically a C++ binary that is always running on the background using launchctl. It intercepts key presses via MacOS's APIs and logs a histogram in a plain text file. The UI is a Electron-based application that displays the file's data using Plotly.

We'll be primarily focusing on the backend. The objective of building this simple application is to learn about some MacOS APIs by creating a simple end-to-end application.

<!--more-->

<figure class="center_children">
    <img src="{{resources_path}}/keyboard.jpg" alt="Mac keyboard"/>
    <figcaption>Credit: <a href="https://unsplash.com/photos/1RVWGnPR2i4">Mitchel Lensink</a></figcaption>
</figure>

The Github project is [here](https://github.com/kunigami/keylogger), in case you want to follow along with the code. The majority of the daemon logic is in `daemon/main.cpp`.

## Concepts

### Core Foundation and Core Graphics

[Core Foundation](https://developer.apple.com/documentation/corefoundation) is a set of libraries containing some general utils for common operations but also access to some Operating System operations. The functions names from Core Foundation are prefixed with `CF`.

[Core Graphics](https://developer.apple.com/documentation/coregraphics) is a set of libraries containing utils related to user interaction, be it UI or handling of input events like keyboards. The functions names from Core Graphics are prefixed with `CG`.

There are also libraries for dealing with accessibility, which are prefixed with `AX`.

### Mach Ports

[Mach](https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/KernelProgramming/Mach/Mach.html) is the name of MacOS's kernel. It uses the concept of *ports*, which are abstraction representing uni-directional communication channels and [2] says is the equivalent to Linux's pipes.

A *port* is often used by callers to request objects from a source. The data structure representing a port is the `CFMachPort`.

### CFRunLoop

One of the key parts of the application is the `CFRunLoop`. According to [1], the `CFRunLoop` object monitors sources of input to a task and dispatches control when they become ready for processing.

One of the sources that can be monitored is `CFRunLoopSource` which is associated with an input source (like keyboard or mouse event).

There is exactly one run loop per thread. The function `CFRunLoopRun()` starts the `CFRunLoop` for the current thread.

`CFRunLoopGetMain()` and `CFRunLoopGetCurrent()` get the `CFRunLoop` of the main thread or the current thread, respectively. Note these can be called before the `CFRunLoop` has started running.

## Application

We now describe the main parts of the application, roughly in the order that they're executed.

### Accessibility

First we need to check for accessibility privileges. By default binaries need explicit permission to access input APIs.

We call the `AXIsProcessTrustedWithOptions()` function which returns true if the application has accessibility privileges. We can pass a flag so this API also prompts the OS to ask for permission in case it does not.

It doesn't seem to be possible to pre-grant accessibility to binaries [3], so we'll need to run it first and wait for the prompt to grant privileges via the UI.

### Listener

This part involves registering a callback that fires on every keystroke.

We first create a wrapper for our callback via `CGEventTapCreate()`, by passing our own function, which we call `on_keystroke` and has this signature:

{% highlight cpp %}
CGEventRef on_keystroke(
    CGEventTapProxy proxy,
    CGEventType type,
    CGEventRef event,
    void *data
);
{% endhighlight %}

The last argument is a generic pointer which we can use to send our own data. The call to `CGEventTapCreate()` looks like:

{% highlight cpp %}
CFMachPortRef handle = CGEventTapCreate(
    ...,
    mask,
    on_keystroke,
    &custom_data
);
{% endhighlight %}

`mask` serves as a filter for what type of events we're interested in. We can set it to:

{% highlight cpp %}
mask = (1 << kCGEventKeyDown);
{% endhighlight %}

The last argument is a pointer to some data which will be forwarded to `on_keystroke`. The return type is `CFMachPortRef` which is a reference to `CFMachPort`, which will act as the channel from which we'll receive the events.

We then start a loop that keeps listening to events and calls our wrapped callback `handle`:

{% highlight cpp %}
CFRunLoopSourceRef runloop_source = CFMachPortCreateRunLoopSource(
    ...
    handle,
    ...
);
{% endhighlight %}

The return type is `CFRunLoopSourceRef` a reference to `CFRunLoopSource`, which represents a subscription to input events.

Finally, we register the `CFRunLoopSource` with the loop running in the main loop:

{% highlight cpp %}
CFRunLoopAddSource(
    CFRunLoopGetMain(),
    runloop_source,
    ...
);
{% endhighlight %}

### Formatting Key

This is basically te job of `on_keystroke()`. Recall the signature of this callback is:

{% highlight cpp %}
CGEventRef on_keystroke(
    CGEventTapProxy proxy,
    CGEventType type,
    CGEventRef event,
    void *data
);
{% endhighlight %}

The `CGEventType` is represents the type of the input. One of them is `kCGEventKeyDown` which, as the name suggests, correspond to the event when a key is pressed. There are other inputs for events like `kCGEventMouseMoved` but we won't receive them because we filtered the events via the mask:

{% highlight cpp %}
mask = (1 << kCGEventKeyDown);
{% endhighlight %}

To extract a code associated with the specific key being pressed, we can call:

{% highlight cpp %}
auto key_code = CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
{% endhighlight %}

There doesn't seem to be an obvious pattern or existing mapping function from `key_code` to the actual character being pressed, so we need to build an explicit mapping, which I copied from [4]. Here are the first few keycodes:

{% highlight cpp %}
std::string key_code_to_str(int key_code, bool shift, bool caps) {
    switch (key_code) {
        case 0:   return shift ? "A" : "a";
        case 1:   return shift ? "S" : "s";
        case 2:   return shift ? "D" : "d";
        case 3:   return shift ? "F" : "f";
        case 4:   return shift ? "H" : "h";
        case 5:   return shift ? "G" : "g";
        case 6:   return shift ? "Z" : "z";
        case 7:   return shift ? "X" : "x";
        case 8:   return shift ? "C" : "c";
        case 9:   return shift ? "V" : "v";
        case 11:  return shift ? "B" : "b";
        case 12:  return shift ? "Q" : "q";
        case 13:  return shift ? "W" : "w";
        case 14:  return shift ? "E" : "e";
        case 15:  return shift ? "R" : "r";
        case 16:  return shift ? "Y" : "y";
        case 17:  return shift ? "T" : "t";
        ...
        case 123: return "[left]";
        case 124: return "[right]";
        case 125: return "[down]";
        case 126: return "[up]";
    }
}
{% endhighlight %}

We can extract the metakeys via:

{% highlight cpp %}
auto flag = CGEventGetFlags(event);
bool is_shift = !!(flag & kCGEventFlagMaskShift);
bool is_cmd = !!(flag & kCGEventFlagMaskCommand);
bool is_ctrl !!(flag & kCGEventFlagMaskControl);
{% endhighlight %}

We can use this information to format the key press as:

{% highlight cpp %}
auto key_str = key_code_to_str(key_code, is_shift, false);
std::string prefix = "";
if (is_cmd) {
    key_str = "[cmd] + " + key_str;
}

if (is_ctrl) {
    key_str = "[ctrl] + " + key_str;
}
{% endhighlight %}

and log this to a file.

### Logging to File

There are definitely better ways to implement this, but we chose to use a file for our simple application to:

* Reduce dependencies - we can using C++ standard libraries to write to files instead of having to pick some database.
* Snapshotting - by writing data to disk we avoid (most) data loss if our application crashes
* Communication channel - we can use the file as an asynchronous communication mechanism between backend and frontend.

We keep the data on the file aggregated (key value + counts) and also log the data on a different file every day. The reason for the latter is so we can do time range queries (e.g. total keystrokes on the past 7 days) and also so it is fairly easy to delete old data.

However, writing to disk is expensive and we can't afford to do it on every keystroke. Instead we buffer the raw data in a simple vector and from time to time we flush to disk.

To implement this we start a new thread that runs on an infinite loop and but only executes every 5 seconds:

{% highlight cpp %}
std::thread([&env, &event_tap]() {
    while (true) {
        auto next_ts = std::chrono::steady_clock::now() +
            std::chrono::milliseconds(5000);
        std::this_thread::sleep_until(next_ts);
        log_current_keystrokes(env, event_tap);
    }
}).detach();
{% endhighlight %}

The `log_current_keystrokes()` performs a few operations:

* Aggregate data from our raw logs by building a histogram keyed on the formatted key value.
* Read the (aggregated) data from an existing file into memory
* Merge that data into the histogram
* Write to a new file
* Replace the new file with the old file.

We write to a separate file to avoid read-locking the existing file while updating it, in case the frontend is reading from it, but in practice the updates are fast and infrequent and so are the frontend reads.

## Daemon

We want to make sure our binary runs all times, including when the computer is restarted. We can use [launchd](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html) for this.

First we need to register it via a configuration file. The configuration will have an ID, for example `com.joe.keylogger`, which is also going to be the name of the file, with extension `.plist`. We'll add this file in under our user folder, `/Users/joe/Library/LaunchAgents/com.joe.keylogger.plist`:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>

    <key>Label</key>
    <string>com.joe.keylogger</string>

    <key>ProcessType</key>
    <string>Background</string>

    <key>ProgramArguments</key>
    <array>
        <string>/Users/joe/bin/keylogger/main</string>
        <string>-o</string>
        <string>/Users/joe/var/keylogger/data/</string>
    </array>

    <key>KeepAlive</key>
    <true/>

    <key>WorkingDirectory</key>
    <string>/Users/joe/bin/keylogger</string>

    <key>StandardOutPath</key>
    <string>/Users/joe/var/keylogger/stdout.log</string>

    <key>StandardErrorPath</key>
    <string>/Users/joe/var/keylogger/stderr.log</string>

</dict>
</plist>
{% endhighlight %}

As we can see, the `plist` is just an XML format configuring a dictionary. A few comments in some of the keys:

* `ProgramArguments`: the command that will be executed together with its arguments
* `KeepAlive`: by setting it to `true` it means launchd will keep trying to start it until it succeeds.
* `WorkingDirectory`: the effective directory the binary will run (default is '/')
* `StandardOutPath`: the file were stdout is logged to
* `StandardErrorPath`: the file were stderr is logged to

Logging to files is very handy for this situation since we won't have access to stdout/stderr directly.

Here are some useful commands. Start a daemon job:

    launchctl load /Users/joe/Library/LaunchAgents/com.joe.keylogger.plist

Check if it's running:

    launchctl list | grep keylogger

Stop a job:

    launchctl unload /Users/joe/Library/LaunchAgents/com.joe.keylogger.plist


## Frontend

Once we start getting the aggregated data into files we can display them in a nicer format. We could serve them using a webserver and display in the browser or even run a Jupyter notebook locally.

I opted to use [Electron](https://www.electronjs.org/) because I plan to use it for later applications.

The overall pattern is very similar to how we would do a webpage: we have a `index.html` with some divs with specific IDs which we'll access in JavaScript.

We then execute the JavaScript code using Node.js once the DOM content has loaded. He have access to the DOM elements from the page just like in a web application. The major difference is that we have access to the local filesystem since this process is not running in a sandboxed environment like in the browser.

So we read the data, transform and use Plotly to display the results, as in Figure 1.

<figure class="center_children">
    <img src="{{resources_path}}/chart.png" alt="see caption"/>
    <figcaption>Figure 1: Histogram of keys pressed during a day.</figcaption>
</figure>

We then use `setTimeout()` to re-execute the code every few minutes.

## Conclusion

This was a fun project to work on. It's the first time I write a MacOS application and use Electron.

Apple's documentation was not very helpful, especially for API documentation. One example was trying to figure out which constant in `CGEventType` mapped to 14, which was an unhandled value I was seeing in runtime. Neither the web nor XCode's documentation helped me and after some reverse engineering it turned out 14 is not part of `CGEventType`.

Luckily there are tons of examples on Github that helped me tremendously. I later also realized that we can at least look at the header files, which are described in more details in the *Appendix*.

My next small project is to log words instead of keys, but that's trickier because words might come from auto-complete or Cmd-C + Cmd-V, so a new strategy is needed.

## Appendix

Suppose we want to find what 14 represents in `CGEventType`. We can start by looking for the `CoreGraphics.h` header:

    > sudo find /Library -name CoreGraphics.h
    /Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/System/Library/Frameworks/CoreGraphics.framework/Versions/A/Headers/CoreGraphics.h

There we see the line:

{% highlight cpp %}
#include <CoreGraphics/CGEventTypes.h>
{% endhighlight %}

which is a promising candidate, so we repeat:

    > sudo find /Library -name CGEventTypes.h
    /Library/Developer/CommandLineTools/SDKs/MacOSX11.1.sdk/System/Library/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h

and find the definition:

{% highlight cpp %}
/* Constants that specify the different types of input events. */
typedef CF_ENUM(uint32_t, CGEventType) {
    /* The null event. */
    kCGEventNull = NX_NULLEVENT,

    /* Mouse events. */
    kCGEventLeftMouseDown = NX_LMOUSEDOWN,
    kCGEventLeftMouseUp = NX_LMOUSEUP,
    ...
}
{% endhighlight %}

Now, one of these constants could be our 14, but now it's unclear which library this is importing from, so we do a content search:

    > grep -rnw  /Library/Developer/CommandLineTools/SDKs/MacOSX11.1.sdk/System/Library/Frameworks/ -e 'NX_LMOUSEDOWN'

which is more noisy, but we eventually find `IOLLEvent.h`. There we do have the

{% highlight cpp %}
#define NX_SYSDEFINED 14 /* system-defined event */
{% endhighlight %}

though that doesn't seem an explicit entry of `CGEventType`.

## References

* [[1](https://developer.apple.com/documentation/corefoundation/cfrunloop-rht)] Core Foundation: CFRunLoop
* [[2](https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/KernelProgramming/Mach/Mach.html)] Mach Overview
* [[3](https://apple.stackexchange.com/questions/178313/change-accessibility-setting-on-mac-using-terminal)] Ask Different: Change accessibility setting on Mac using terminal
* [[4](https://github.com/caseyscarborough/keylogger)] Github: keylogger
