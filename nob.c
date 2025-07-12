#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"
#undef UNREACHABLE
#undef UNUSED
#undef sv_eq
#undef NOB_IMPLEMENTATION

#define ARENA_IMPLEMENTATION
#include "include/arena.h"

#include <stdlib.h>

#define INTRINSICS_IMPLEMENTION
#include "lib/intrinsics.h"

#define CC_IMPLEMENTATION
#include "lib/cc.h"

#define STRINGT_IMPLEMENTATION
#include "include/stringt.h"

#define TMP_IMPLEMENTATION
#include "include/tmp.h"


orstring_t SOURCES[] = {
    lit2str("./src/lexer.c"),
    lit2str("./src/parser.c"),
    lit2str("./src/static_analyzer.c"),
    lit2str("./src/type.c"),
    lit2str("./src/type_set.c"),
    lit2str("./src/debugger.c"),
    lit2str("./src/error.c"),
    lit2str("./lib/core.c"),
};

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    arena_t allocator = {0};

    nob_mkdir_if_not_exists("./bin");

    // build test
    {
        cc_t cc = cc_make(CC_GCC, &allocator);
        cc.output_type = CC_EXE;
        cc_flag(&cc, lit2str("-std=c99"));
        cc_flag(&cc, lit2str("-Wall"));
        cc_flag(&cc, lit2str("-Wextra"));
        // cc_flag(&cc, lit2str("-fsanitize=address"));

        cc_source(&cc, lit2str("./tests/test.c"));
        cc.output_path = lit2str("./bin/test");

        cc_build(&cc);
    }
    
    // build liborso
    {
        cc_t cc = cc_make(CC_GCC, &allocator);
        cc.output_type = CC_EXE;

        cc_flag(&cc, lit2str("-std=c99"));
        cc_flag(&cc, lit2str("-Wall"));
        cc_flag(&cc, lit2str("-Wextra"));
        cc_flag(&cc, lit2str("-Wconversion"));
        // cc_flag(&cc, lit2str("-fsanitize=address"));

        cc_include_dir(&cc, lit2str("./include"));
        cc_include_dir(&cc, lit2str("./lib"));

        cc_flag(&cc, lit2str("-g"));
        cc_flag(&cc, lit2str("-ggdb"));
        cc_flag(&cc, lit2str("-DDEBUG"));

        for (size_t i = 0; i < sizeof(SOURCES) / sizeof(SOURCES[0]); i++) {
            cc_source(&cc, SOURCES[i]);
        }

        // build as a library first
        {
            cc.output_path = lit2str("./bin/liborso.a");
            cc.output_type = CC_STATIC;
            cc_source(&cc, lit2str("./lib/orso.c"));
            cc_build(&cc);
        }

        // then compiler
        {
            cc.output_path = lit2str("./bin/orso");
            cc.output_type = CC_EXE;
            cc_source(&cc, lit2str("./src/main.c"));
            cc_build(&cc);
        }

    }

    nob_copy_file("./lib/core.or", "./bin/core.or");

    arena_free(&allocator);

}