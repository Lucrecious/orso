#ifndef DYNLIB_H_
#define DYNLIB_H_

#include "stringt.h"

typedef struct dynlib_t dynlib_t;
struct dynlib_t {
    void *handle;
};

dynlib_t dynlib_load(string_t path);
void dynlib_unload(dynlib_t dl);
void *dynlib_symbol(dynlib_t dl, string_t symbol_name);

#endif

#ifdef DYNLIB_IMPLEMENTATION

#include <dlfcn.h>

dynlib_t dynlib_load(string_t path) {
    dynlib_t dl;
    dl.handle = dlopen(path.cstr, RTLD_LOCAL);
    return dl;
}

void dynlib_unload(dynlib_t dl) {
    // dlclose(dl.handle);
    UNUSED(dl);
}

void *dynlib_symbol(dynlib_t dl, string_t symbol_name) {
    return dlsym(dl.handle, symbol_name.cstr);
}

#undef DYNLIB_IMPLEMENTATION
#endif
