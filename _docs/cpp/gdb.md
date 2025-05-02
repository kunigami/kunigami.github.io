---
layout: doc
title: "GDB"
---

Note: make sure to compile with `-g`.

# Commands

| Command | Description |
| `b <file>:<line>`      | Breakpoint at file / line  |
| `bt`                   | Show callstack |
| `fin`                  | Step out |
| `c`                    | Continue    |
| `l`                    | Show lines around |
| `n`                    | Next line   |
| `r`                    | Start program |
| `s`                    | Step in |
| `tbreak`               | One-time breakpoint |

## Breakpoint

Formats:

* `b <file>:<line>`

Adds a breakpoint. Use `tbreak` for a one-time only break.

## Continue

`c`. Go to the next breakpoint.

## Next line

`n` or `next`.

## Run

`r`. Starts the binary.

## Show call stack

`bt` or `backtrace`. Show the current call stack.

## Show next lines

`l` or `list`. Shows 10 lines around the current position.

## Skip functions

It's possible to always skip certain functions, such as `std::move()`.

`skip function move`

## Step in

`s` or `step`. Enters the function.

## Step out

`fin` or `finish`. Exits the current function and goes back to the immediate caller.
