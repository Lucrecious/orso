#ifndef ORSO_H_
#define ORSO_H_

#include "intrinsics.h"

typedef struct orso_compiler_t orso_compiler_t;
struct orso_compiler_t {
    orstring_t root_source;
    orstring_t build_dir;
    orstring_t output_name;
};

bool ororso_build(orso_compiler_t *compiler);

#endif
