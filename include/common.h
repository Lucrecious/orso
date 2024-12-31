#ifndef COMMON_H_
#define COMMON_H_

#include "def.h"

struct error_t;

#define UINT8_COUNT (UINT8_MAX + 1)
#define MAX_PARAMETERS 100

typedef void (*error_function_t)(struct error_t error);
typedef void (*write_function_t)(const char* chars);

#endif
