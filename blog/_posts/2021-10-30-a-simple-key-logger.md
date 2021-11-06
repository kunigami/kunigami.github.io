---
layout: post
title: "A Simple Key Logger for MacOS"
tags: [c++]
vanity: "2021-10-30-a-simple-key-logger"
excerpt_separator: <!--more-->
---

In this post we'll build a simple key logger for MacOS. I consists of two parts: a daemon and a UI. The daemon is basically a C++ binary that is always running on the background. It intercepts key presses via MacOS's APIs and logs a histogram in a plain text file. The UI is a Electron-based application that displays the data using Plotly.

The objective of this exercise is to learn about APIs.

<!--more-->

## Frontend

* Electron

## Backend

* C++


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
