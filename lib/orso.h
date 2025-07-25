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

void *orrealloc(void *ptr, size_t new_size);
void orprintint(orint num);
void orprintln(orcstr_t s);

bool orshell_run(void *cmds, size_t count);

void *orreserve(size_t size);
bool ormarkro(void *addr, size_t size);
bool ormarkrw(void *addr, size_t size);
bool orfree(void *addr, size_t size);
size_t orpagesize(void);

#endif
