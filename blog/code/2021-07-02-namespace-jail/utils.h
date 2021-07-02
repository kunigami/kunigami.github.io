#ifndef UTILS_H
#define UTILS_H

#include <iostream>

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

using namespace std;

// prints action to sterr + string representation of errno
void error_action(string action);

// error_action + exit program with failure
void fatal_action(string action);

// format string using printf() syntax
string format(const char *format, ...);

#endif

