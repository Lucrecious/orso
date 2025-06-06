#define NOB_IMPLEMENTATION
#include "nob.h"

#define ARENA_IMPLEMENTATION
#include "include/arena.h"

#include <stdlib.h>
#include "lib/intrinsics.h"


const char* SOURCES[] = {
    "./src/lexer.c",
    "./src/parser.c",
    "./src/static_analyzer.c",
    "./src/type.c",
    "./src/type_set.c",
    "./src/debugger.c",
    "./src/error.c",
    "./lib/core.c",
};

typedef enum cb_std_t cb_std_t;
enum cb_std_t {
    cb_std_c99
};

typedef struct c_builder_t c_builder_t;
struct c_builder_t {
    arena_t *allocator;
    cb_std_t std;
    orcstr_t output;

    struct {
        orcstr_t *items;
        size_t count;
        size_t capacity;
    } flags;

    struct {
        orcstr_t *items;
        size_t count;
        size_t capacity;
    } sources;

    struct {
        orcstr_t *items;
        size_t count;
        size_t capacity;
    } includes;

    struct {
        orcstr_t *items;
        size_t count;
        size_t capacity;
    } library_dirs;

    struct {
        orcstr_t *items;
        size_t count;
        size_t capacity;
    } libraries;
};

void cb_flags(c_builder_t *cb, size_t n, ...) {
    va_list args;
    va_start(args, n);
    
    for (size_t i = 0; i < n; ++i) {
        orcstr_t flag = va_arg(args, orcstr_t);
        flag = arena_strdup(cb->allocator, flag);
        nob_da_append(&cb->flags, flag);
    }

    va_end(args);
}

void cb_source(c_builder_t *cb, orcstr_t src) {
    src = arena_strdup(cb->allocator, src);
    nob_da_append(&cb->sources, src);
}

void cb_include(c_builder_t *cb, orcstr_t include_path) {
    include_path = arena_strdup(cb->allocator, include_path);
    nob_da_append(&cb->includes, include_path);
}

void cb_library_dir(c_builder_t *cb, orcstr_t library_dir) {
    library_dir = arena_strdup(cb->allocator, library_dir);
    nob_da_append(&cb->library_dirs, library_dir);
}

void cb_library(c_builder_t *cb, orcstr_t library) {
    library = arena_strdup(cb->allocator, library);
    nob_da_append(&cb->libraries, library);
}


bool cb_build(c_builder_t *cb) {
    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "cc");

    nob_cmd_append(&cmd, "-o", cb->output);

    for (size_t i = 0; i < cb->flags.count; ++i) {
        nob_cmd_append(&cmd, cb->flags.items[i]);
    }

    for (size_t i = 0; i < cb->includes.count; ++i) {
        nob_cmd_append(&cmd, nob_temp_sprintf("-I%s", cb->includes.items[i]));
    }

    for (size_t i = 0; i < cb->library_dirs.count; ++i) {
        nob_cmd_append(&cmd, nob_temp_sprintf("-L%s", cb->library_dirs.items[i]));
    }

    for (size_t i = 0; i < cb->libraries.count; ++i) {
        nob_cmd_append(&cmd, nob_temp_sprintf("-l%s", cb->libraries.items[i]));
    }

    for (size_t i = 0; i < cb->sources.count; ++i) {
        orcstr_t src = cb->sources.items[i];
        nob_cmd_append(&cmd, src);
    }

    bool success = nob_cmd_run_sync(cmd);

    nob_cmd_free(cmd);

    return success;
}

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    arena_t allocator = {0};

    nob_mkdir_if_not_exists("./bin");

    // build test
    {
        c_builder_t cb = {.allocator=&allocator};
        cb.std = cb_std_c99;
        cb_flags(&cb, 3, "-Wall", "-Wextra", "-fsanitize=address");

        cb_source(&cb, "./tests/test.c");
        cb.output = nob_temp_sprintf("./bin/test");

        cb_build(&cb);
    }

    c_builder_t cb = {.allocator=&allocator};
    cb.std = cb_std_c99;
    cb_flags(&cb, 3, "-Wall", "-Wextra", "-Wconversion", "-fsanitize=address");

    cb_include(&cb, "./include");
    cb_include(&cb, "./lib");

    cb_flags(&cb, 2, "-ggdb", "-DDEBUG");

    cb.output = nob_temp_sprintf("./bin/orso");

    for (size_t i = 0; i < sizeof(SOURCES) / sizeof(SOURCES[0]); i++) {
        cb_source(&cb, SOURCES[i]);
    }

    cb_source(&cb, "./lib/orso.c");
    cb_source(&cb, "./src/main.c");

    cb_build(&cb);

    nob_copy_file("./lib/core.or", "./bin/core.or");

    arena_free(&allocator);

}