#ifndef ORSO_H_
#define ORSO_H_

#include "intrinsics.h"

typedef struct orstr8s_t orstr8s_t;
struct orstr8s_t {
    orstring_t *items;
    orsint count;
    orsint capacity;
};

typedef struct orso_compiler_t orso_compiler_t;
struct orso_compiler_t {
    orstring_t src;
    orstring_t build_dir;
    orstring_t output_name;

    orstr8s_t cflags;
    orstr8s_t linker_flags;
};

bool orbuild(orso_compiler_t *compiler);
bool orinterpret(orstring_t input_file_path);
bool ordebug(orstring_t input_file_path);

void *orrealloc(void *ptr, size_t new_size);
void orprintint(orint num);
void orprintln(orstring_t s);

orf32 orsinf(orf32);
orf32 orcosf(orf32);
orf32 oratan2f(orf32, orf32);
orf32 orsqrtf(orf32);
orf32 orrandf();

bool orshell_run(void *cmds, size_t count);

void *orreserve(size_t size);
bool ormarkro(void *addr, size_t size);
bool ormarkrw(void *addr, size_t size);
bool orfree(void *addr, size_t size);
size_t orpagesize(void);

#endif
