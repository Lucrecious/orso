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
    string_t output_name;
    string_t build_dir;
    cc_output_type_t output_type;
    strings_t mem_sources;
    strings_t include_dirs;

};

cc_t cc_make(cc_cc_t cc_cc, arena_t *arena);
void cc_mem_source(cc_t *cc, string_t mem_source);
void cc_include_dir(cc_t *cc, string_t dir);
bool cc_build(cc_t *cc);


#endif

#ifdef CC_IMPLEMENTATION
cc_t cc_make(cc_cc_t cc_cc, arena_t *arena) {
    return (cc_t){
        .cc = cc_cc,
        .arena = arena, 
        .mem_sources = {.allocator=arena},
        .include_dirs = {.allocator=arena},
        .output_name = lit2str("a.out"),
        .build_dir = lit2str("./build"),
        .output_type = CC_EXE
    };
}

void cc_mem_source(cc_t *cc, string_t mem_source) {
    array_push(&cc->mem_sources, string_copy(mem_source, cc->arena));
}

void cc_include_dir(cc_t *cc, string_t dir) {
    array_push(&cc->include_dirs, dir);
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

    bool success = mkdir_if_not_exists(cc->build_dir.cstr);
    if (!success) return false;

    // write mem sources
    for (size_t i = 0; i < cc->mem_sources.count; ++i) {
        string_t path = string_format("%s/%s%llu.c", cc->arena, cc->build_dir.cstr, "tmp", i);
        array_push(&source_paths, path);

        string_t mem_source = cc->mem_sources.items[i];
        bool success = write_entire_file(path.cstr, mem_source.cstr, mem_source.length);
        if (!success) return false;

        string_t patho = string_format("%s/%s%llu.o", cc->arena, cc->build_dir.cstr, "tmp", i);
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

        cmd_append(&cmd, "-o");
        
        string_t source_out_path = source_out_paths.items[i];
        cmd_append(&cmd, source_out_path.cstr);

        success = cmd_run_sync_and_reset(&cmd);
        if (!success) return false;
    }

    string_t outfile = string_format("%s/%s", cc->arena, cc->build_dir.cstr, cc->output_name.cstr);

    switch (cc->output_type) {
        case CC_DYNAMIC: {
            cmd_append(&cmd, cc_name_.cstr);

            cmd_append(&cmd, "-shared", "-o");

            cmd_append(&cmd, outfile.cstr);

            for (size_t i = 0; i < source_out_paths.count; ++i) {
                string_t source_out_path = source_out_paths.items[i];
                cmd_append(&cmd, source_out_path.cstr);
            }

            success = cmd_run_sync_and_reset(&cmd);
            if (!success) return false;

            break;
        }

        case CC_EXE:
        case CC_STATIC: UNREACHABLE(); break;
    }

    return success;
}

#undef CC_IMPLEMENTATION
#endif


