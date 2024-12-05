#define NOB_IMPLEMENTATION
#include "nob.h"


const char* SOURCES[] = {
    "./src/lexer.c",
    "./src/object.c",
    "./src/parser.c",
    "./src/slot.c",
    "./src/static_analyzer.c",
    "./src/symbol_table.c",
    "./src/type.c",
    "./src/type_set.c",
};

typedef enum {
    BUILD_MODE_NONE = 0x0,
    BUILD_MODE_DEBUG = 0x1,
    BUILD_MODE_RELEASE = 0x2,
} build_mode_t;

void print_usage() {
    nob_log(NOB_INFO, "Usage: nob [option]");
    nob_log(NOB_INFO, "       --debug (-d)           compiles debug build");
    nob_log(NOB_INFO, "       --release (-r)         compiles release build");
    nob_log(NOB_INFO, "       --debug-trace (-dt)    compiles debug-trace build");
    nob_log(NOB_INFO, "       --test                 compiles the test suite");
}


bool build_program(build_mode_t build_mode, const char* output_name) {
    Nob_Cmd cmd = {0};

    nob_mkdir_if_not_exists("./bin");

    nob_cmd_append(&cmd, "cc");
    nob_cmd_append(&cmd, "-Wall", "-Wextra");
    nob_cmd_append(&cmd, "-fsanitize=address");
    nob_cmd_append(&cmd, "-std=c99");

    nob_cmd_append(&cmd, "-I./include");

    if (build_mode & BUILD_MODE_DEBUG) {
        nob_cmd_append(&cmd, "-ggdb");
        nob_cmd_append(&cmd, "-DDEBUG");
        nob_cmd_append(&cmd, "-o", nob_temp_sprintf("./bin/%s", output_name));
    } else if (build_mode & BUILD_MODE_RELEASE) {
        nob_cmd_append(&cmd, "-O3");
        nob_cmd_append(&cmd, "-o", nob_temp_sprintf("./bin/%s", output_name));
    } else {
        NOB_ASSERT(false && "Unreachable");
        return 1;
    }

    for (size_t i = 0; i < sizeof(SOURCES) / sizeof(SOURCES[0]); i++) {
        nob_cmd_append(&cmd, SOURCES[i]);
    }

    nob_cmd_append(&cmd, "./src/main.c");

    return nob_cmd_run_sync(cmd);
}

int main(int argc, char** argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    nob_shift_args(&argc, &argv);

    build_mode_t mode = BUILD_MODE_NONE;

    const char* output_name = "dorso";

    if (argc == 0) {
        mode = BUILD_MODE_DEBUG;
    } else {
        while (argc > 0) {
            const char* option = nob_shift_args(&argc, &argv);

            if (strcmp(option, "--release") == 0 || strcmp(option, "-r") == 0) {
                mode |= BUILD_MODE_RELEASE;
                output_name = "orso";
            } else if (strcmp(option, "--debug") == 0 || strcmp(option, "-d") == 0) {
                mode |= BUILD_MODE_DEBUG;
                output_name = "dorso";
            } else {
                nob_log(NOB_ERROR, nob_temp_sprintf("unknown option: %s", option));
                print_usage();
                return 1;
            }
        }
    }

    nob_log(NOB_INFO, "building compiler");

    if (!build_program(mode, output_name)) {
        nob_log(NOB_ERROR, "unable to compile compiler");
        return 1;
    }

    return 0;
}