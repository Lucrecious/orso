#ifndef COMMON_H_
#define COMMON_H_

#include "def.h"

struct OrsoError;

#define UINT8_COUNT (UINT8_MAX + 1)
#define MAX_PARAMETERS 100

typedef void (*OrsoErrorFunction)(struct OrsoError error);
typedef void (*OrsoWriteFunction)(const char* chars);

#endif
