#ifndef CC_H_
#define CC_H_

#include "stringt.h"
#include "../nob.h"

typedef enum cc_cc_t cc_cc_t;
enum cc_cc_t {
    CC_GCC,
};

typedef enum cc_output_type_t cc_output_type_t;
enum cc_output_type_t {
    CC_STATIC,
    CC_DYNAMIC,
    CC_EXE,
};

typedef struct cc_t cc_t;
struct cc_t {
    arena_t *arena;
    cc_cc_t cc;
    string_t output_path;
    cc_output_type_t output_type;
    strings_t sources;
    strings_t include_dirs;
    strings_t no_warnings;
};

cc_t cc_make(cc_cc_t cc_cc, arena_t *arena);
void cc_source(cc_t *cc, string_t mem_source);
void cc_include_dir(cc_t *cc, string_t dir);
void cc_no_warning(cc_t *cc, string_t warning);
bool cc_build(cc_t *cc);


#endif

#ifdef CC_IMPLEMENTATION
cc_t cc_make(cc_cc_t cc_cc, arena_t *arena) {
    return (cc_t){
        .cc = cc_cc,
        .arena = arena, 
        .sources = {.allocator=arena},
        .include_dirs = {.allocator=arena},
        .no_warnings = {.allocator=arena},
        .output_path = lit2str("a.out"),
        .output_type = CC_EXE
    };
}

void cc_source(cc_t *cc, string_t source_path) {
    array_push(&cc->sources, string_copy(source_path, cc->arena));
}

void cc_include_dir(cc_t *cc, string_t dir) {
    array_push(&cc->include_dirs, dir);
}

void cc_no_warning(cc_t *cc, string_t warning) {
    array_push(&cc->no_warnings, warning);
}

static string_t cc_name(cc_cc_t cc) {
    switch (cc) {
        case CC_GCC: return lit2str("gcc");
    }

    UNREACHABLE();
}

bool cc_build(cc_t *cc) {
    strings_t source_paths = {.allocator=cc->arena};
    strings_t source_out_paths = {.allocator=cc->arena};

    for (size_t i = 0; i < cc->sources.count; ++i) {
        string_t path = cc->sources.items[i];
        array_push(&source_paths, path);

        string_t patho = string_format("%s%llu.o", cc->arena, path.cstr, i);
        array_push(&source_out_paths, patho);
    }

    Cmd cmd = {0};
    string_t cc_name_ = cc_name(cc->cc);

    for (size_t i = 0; i < source_paths.count; ++i) {
        cmd_append(&cmd, cc_name_.cstr);

        cmd_append(&cmd, "-c");

        string_t source_path = source_paths.items[i];
        cmd_append(&cmd, source_path.cstr);

        for (size_t i = 0; i < cc->include_dirs.count; ++i) {
            string_t include_dir = cc->include_dirs.items[i];
            string_t include_dir_flag = string_format("-I%s", cc->arena, include_dir.cstr);
            cmd_append(&cmd, include_dir_flag.cstr);
        }

        for (size_t i = 0; i < cc->no_warnings.count; ++i) {
            string_t warning = cc->no_warnings.items[i];
            string_t nowarning_flag = string_format("-Wno-%s", cc->arena, warning.cstr);
            cmd_append(&cmd, nowarning_flag.cstr);
        }

        cmd_append(&cmd, "-o");
        
        string_t source_out_path = source_out_paths.items[i];
        cmd_append(&cmd, source_out_path.cstr);

        bool success = cmd_run_sync_and_reset(&cmd);
        if (!success) return false;
    }

    string_t outfile = string_format("%s", cc->arena, cc->output_path.cstr);

    switch (cc->output_type) {
        case CC_DYNAMIC: {
            cmd_append(&cmd, cc_name_.cstr);

            cmd_append(&cmd, "-shared", "-o");

            cmd_append(&cmd, outfile.cstr);

            for (size_t i = 0; i < source_out_paths.count; ++i) {
                string_t source_out_path = source_out_paths.items[i];
                cmd_append(&cmd, source_out_path.cstr);
            }

            bool success = cmd_run_sync_and_reset(&cmd);
            if (!success) return false;

            break;
        }

        case CC_EXE: {
            cmd_append(&cmd, cc_name_.cstr);

            cmd_append(&cmd, "-o");

            cmd_append(&cmd, outfile.cstr);

            for (size_t i = 0; i < source_out_paths.count; ++i) {
                string_t source_out_path = source_out_paths.items[i];
                cmd_append(&cmd, source_out_path.cstr);
            }

            bool success = cmd_run_sync_and_reset(&cmd);
            if (!success) return false;

            break;
        }
        case CC_STATIC: UNREACHABLE(); break;
    }

    return true;
}

#undef CC_IMPLEMENTATION
#endif


