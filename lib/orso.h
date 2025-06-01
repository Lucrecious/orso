#ifndef ORSO_H_
#define ORSO_H_

#include "stringt.h"

typedef struct orso_compiler_t orso_compiler_t;
struct orso_compiler_t {
    string_t root_source;
    string_t build_dir;
    string_t output_name;
};

int orso_build(orso_compiler_t *compiler);

#endif
