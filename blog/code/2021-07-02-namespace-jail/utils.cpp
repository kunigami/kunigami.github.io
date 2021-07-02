#include "utils.h"

using namespace std;

void error_action(string action) {
  cerr << "Error when " << action << ": " <<  strerror(errno) << endl;
}

void fatal_action(string action) {
  error_action(action);
  exit(EXIT_FAILURE); 
}

static string format_list(const char *format, va_list argp) {
  char *buffer;
  vasprintf(&buffer, format, argp);
  string formatted(buffer);
  return formatted;
}

string format(const char *format, ...) {
	va_list argp;
	va_start(argp, format);
	string ret = format_list(format, argp);
	va_end(argp);
  return ret;
}
