#ifndef ORSO_H_
#define ORSO_H_

#include "intrinsics.h"

typedef struct orso_compiler_t orso_compiler_t;
struct orso_compiler_t {
    orstring_t root_source;
    orstring_t build_dir;
    orstring_t output_name;

    orstring_t *cflags;
    size_t cflags_count;
};

bool orbuild(void *compiler);
bool orinterpret(orstring_t input_file_path);

void *orrealloc(void *ptr, size_t old_size, size_t new_size);
void orprintint(orint num);

bool orshell_run(void *cmds, size_t count);



#endif
