#ifndef LOG_H_
#define LOG_H_

#include "def.h"

#include <stdlib.h>

void log_fatal(orcstr_t format, ...);

#endif

#ifdef LOG_IMPLEMENTATION

void log_fatal(orcstr_t format, ...) {
    va_list args;
    va_start(args, format);

    printf("[FATAL] ");
    vprintf(format, args);
    printf("\n");

    va_end(args);
}

#undef LOG_IMPLEMENTATION
#endif
