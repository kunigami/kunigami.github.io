---
layout: post
title: "A Simple Key Logger for MacOS"
tags: [c++]
vanity: "2021-10-30-a-simple-key-logger"
excerpt_separator: <!--more-->
---

In this post we'll build a simple key logger for MacOS. I consists of two parts: a daemon (backend) and a UI (frontend). The daemon is basically a C++ binary that is always running on the background using launchctl. It intercepts key presses via MacOS's APIs and logs a histogram in a plain text file. The UI is a Electron-based application that displays the data using Plotly.

The objective of this exercise is to learn about some MacOS APIs by creating a simple end-to-end application.

<!--more-->

## Backend

## Accessibility

First we need to check for accessibility privileges. By default binaries need explicit permission to access input APIs.

We call the `AXIsProcessTrustedWithOptions()` function which returns true if the application has accessibility privileges. We can pass a flag so this API also prompts the OS to ask for permission in case it does not.

### CFRunLoop

Let's discuss the `CFRunLoop` before continuing. According to [1], the `CFRunLoop` object monitors sources of input to a task and dispatches control when they become ready for processing.

One of the sources that can be monitored is `CFRunLoopSource` which is associated with an input source (like keyboard or mouse event).

There is exactly one run loop per thread. The function `CFRunLoopRun()`.

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
handle = CGEventTapCreate(
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

The last argument is a pointer to some data which will be forwarded to `on_keystroke`.

We then start a loop that keeps listening to events and calls our wrapped callback `handle`:

{% highlight cpp %}
runloop_source = CFMachPortCreateRunLoopSource(
    ...
    handle,
    ...
);
CFRunLoopAddSource(
    CFRunLoopGetMain(),
    runloop_source,
    ...
);
{% endhighlight %}

The `CFRunLoopGetMain()` returns the loop

## Frontend

* Electron



## Accessibility
https://apple.stackexchange.com/questions/178313/change-accessibility-setting-on-mac-using-terminal
https://developer.apple.com/documentation/apple-silicon/building-a-universal-macos-binary

how to do it programmatically

## Daemon

https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html

https://www.launchd.info/

Add plist file to `/Users/kunigami/Library/LaunchAgents`:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>

    <key>Label</key>
    <string>com.kunigami.keylogger</string>

    <key>ProcessType</key>
    <string>Background</string>

    <key>Program</key>
    <string>/Users/kunigami/bin/keylogger/main</string>

    <key>KeepAlive</key>
    <true/>

    <key>WorkingDirectory</key>
    <string>/Users/kunigami/bin/keylogger</string>

    <key>StandardOutPath</key>
    <string>/Users/kunigami/var/keylogger/stdout.log</string>

    <key>StandardErrorPath</key>
    <string>/Users/kunigami/var/keylogger/stderr.log</string>

</dict>
</plist>
{% endhighlight %}

Start a daemon job:

    launchctl start Library/LaunchAgents/com.kunigami.keylogger.plist

Check if it's running:

    launchctl list | grep keylogger

Stop a daemon job:

   launchctl stop com.kunigami.keylogger.plist

Status:

https://stackoverflow.com/questions/16706847/how-to-check-if-launchd-has-started-the-script

https://developer.apple.com/documentation/corefoundation/cfrunloop-rht
