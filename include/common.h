#ifndef COMMON_H_
#define COMMON_H_

#include "def.h"

#define UINT8_COUNT (UINT8_MAX + 1)

typedef enum OrsoErrorType {
    ORSO_ERROR_COMPILE,
    ORSO_ERROR_RUNTIME,
} OrsoErrorType;

typedef void (*OrsoErrorFunction)(OrsoErrorType error, i32 line, const char* message);
typedef void (*OrsoWriteFunction)(const char* chars);

#endif
