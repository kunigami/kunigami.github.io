---
layout: post
title: "A Simple Key Logger for MacOS"
tags: [c++]
vanity: "2021-10-30-a-simple-key-logger"
excerpt_separator: <!--more-->
---

In this post we'll build a simple key logger for MacOS. I consists of two parts: a daemon and a UI. The daemon is basically a C++ binary that is always running on the background. It intercepts key presses via MacOS's APIs and logs a histogram in a plain text file. The UI is a Electron-based application that displays the data using Plotly.

The objective of this exercise is to learn about APIs.

## Accessibility
https://apple.stackexchange.com/questions/178313/change-accessibility-setting-on-mac-using-terminal
https://developer.apple.com/documentation/apple-silicon/building-a-universal-macos-binary

how to do it programmatically

## Daemon

https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html

https://www.launchd.info/

Start a daemon job:

    launchctl start Library/LaunchAgents/com.kunigami.keylogger.plist
