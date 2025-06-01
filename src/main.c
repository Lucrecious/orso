
#define NOB_STRIP_PREFIX
#include "../nob.h"
#undef UNREACHABLE
#undef UNUSED
#undef sv_eq

#include "orso.h"

#include "vm.h"


static void print_usage() {
    // println("usage:");
    // println("  orso <filepath> [...]        - runs the given file in the orso interpreter and the optional program arguments");
    // println("  orso debug <filepath>        - runs the given file in the orso interpreter debugger");
    // println("  orso build <filepath>        - creates a build.odl file with template code for building orso projects natively");
    // println("  orso hello <filepath>        - generates a small sample demo of orso into the given file path");
    // println("");
    println("usage:");
    println("  orso                         - prints this usage text");
    println("  orso run <filepath>          - runs the interpreter on given <filepath>");
    println("  orso debug <filepath>        - runs the given <filepath> in the orso interpreter debugger");
    println("  orso build <in> <out>        - creates a native build of the given <in> into an exe <out>");
    println("  orso ast <in>                - outputs ast for orso file for debugging");
    println("");
}

int main(int argc, char **argv) {
    shift(argv, argc);

    arena_t arena = {0};

    if (argc) {
        cstr_t option = shift(argv, argc);
        if (strncmp(option, "build", 5) == 0) {
            cstr_t output = "a.out";
            if (argc > 2) {
                fprintf(stderr, "build option requires at least an input and optional output odl file\n");
                exit(1);
            }

            cstr_t input = shift(argv, argc);
            if (argc) {
                output = shift(argv, argc);
            }

            string_t sinput = cstr2string(input, &arena);
            string_t soutput = cstr2string(output, &arena);

            {
                orso_compiler_t compiler = {0};
                compiler.build_dir = lit2str("./build/");
                compiler.root_source = sinput;
                compiler.output_name = soutput;
                orso_build(&compiler);
            }

        }
        // } else if (strncmp(option, "run", 3) == 0) {
        //     if (argc != 1) {
        //         fprintf(stderr, "run option requires input odl file\n");
        //         exit(1);
        //     }

        //     cstr_t filename = shift(argv, argc);
        //     string_t file = {.cstr=filename, .length=strlen(filename)};
        //     interpret(file);
        // } else if (strncmp(option, "dbg", 3) == 0) {
        //     if (argc != 1) {
        //         fprintf(stderr, "dbg option requires input odl file\n");
        //         exit(1);
        //     }

        //     cstr_t filename = shift(argv, argc);
        //     string_t file = {.cstr=filename, .length=strlen(filename)};
        //     debug(file);
        // } else if (strncmp(option, "ast", 4) == 0) {
        //     if (argc > 2) {
        //         fprintf(stderr, "requires file to resolve");
        //         exit(1);
        //     }

        //     cstr_t input = shift(argv, argc);

        //     string_t sinput = cstr2string(input, &arena);

        //     print_ast(sinput);
        // }
    } else {
        print_usage();
        exit(0);
    }
}

