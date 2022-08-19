#ifndef SAVINE_H_
#define SAVINE_H_

#include "error_codes.h"

typedef struct Savine_Context {
    SavineError error;
} Savine_Context;

void savine_init(Savine_Context* context);
void savine_run_code(Savine_Context* context, char* code);
void savine_get_i64(Savine_Context* context, char* idenifier, long* value);

#endif
