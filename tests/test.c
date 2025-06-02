#define NOB_IMPLEMENTATION
#include "../nob.h"

typedef const char *orcstr_t;

void print_usage(void) {
    printf("usage :\n");
    printf("  test              - prints this usage text\n");
    printf("  test <testpath>   - <testpath> is the relative path to the odl file to test, paths ending in / or \\\\ will test the directory recursively\n");
    printf("\n");
}

Nob_String_View sv_filename(Nob_String_View sv) {
    for (size_t i = sv.count; i > 0; --i) {
        if (sv.data[i-1] == '\\' || sv.data[i-1] == '/') {
            return (Nob_String_View){
                .count = sv.count - (i),
                .data = sv.data+(i),
            };
        }
    }

    return sv;
}

static bool test(orcstr_t test_file_path) {
    Nob_String_View test_file_path_sv = nob_sv_from_cstr(test_file_path);
    Nob_String_View filename = sv_filename(test_file_path_sv);

    orcstr_t native_results_path = nob_temp_sprintf(".tmp/%.*s.result.native", filename.count, filename.data);
    orcstr_t interp_results_path = nob_temp_sprintf(".tmp/%.*s.result.interp", filename.count, filename.data);
    orcstr_t executable_file_path = nob_temp_sprintf(".tmp/%.*s.out", filename.count, filename.data);
    
    Nob_Fd nativefd = nob_fd_open_for_write(native_results_path);
    Nob_Fd interpfd = nob_fd_open_for_write(interp_results_path);

    int result = true;

    if (nativefd == NOB_INVALID_FD || interpfd == NOB_INVALID_FD) nob_return_defer(1);

    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "./bin/orso", "build", test_file_path, executable_file_path);

    if (!nob_cmd_run_sync_and_reset(&cmd)) {
        nob_log(NOB_ERROR, "Failed to build test %s", test_file_path);
        nob_return_defer(false);
    }

    nob_log(NOB_INFO, "---- Executable path: %s", executable_file_path);

    orcstr_t call_test = nob_temp_sprintf("./%s", executable_file_path);
    nob_cmd_append(&cmd, call_test);
    if (!nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout = &nativefd,
    })) {
        nob_log(NOB_ERROR, "Failed to run native program %s", executable_file_path);
        nob_return_defer(false);
    }

    nob_log(NOB_INFO, "---- Native output file: %s", native_results_path);

    nob_cmd_append(&cmd, "./bin/orso", "run", test_file_path);

    if (!nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout=&interpfd,
    })) {
        nob_log(NOB_ERROR, "Failed to run interpreter program %s", executable_file_path);
        nob_return_defer(false);
    }

    nob_log(NOB_INFO, "---- Interpreter output file: %s", interp_results_path);

    nob_cmd_append(&cmd, "diff", native_results_path, interp_results_path);

    if (!nob_cmd_run_sync_and_reset(&cmd)) {
        nob_log(NOB_ERROR, "diff failed.", executable_file_path);
        nob_return_defer(false);
    }

defer:
    nob_fd_close(nativefd);
    nob_fd_close(interpfd);
    return result;
}

int main(int argc, char *argv[]) {
    nob_shift(argv, argc);

    Nob_String_View test_file_path;
    orcstr_t ctest_file_path;
    if (argc) {
        ctest_file_path = argv[0];
        test_file_path = nob_sv_from_cstr(ctest_file_path);
    } else {
        print_usage();
        return 0;
    }

    nob_mkdir_if_not_exists(".tmp");

    bool is_dir = false;
    char last_char = test_file_path.data[test_file_path.count-1];
    if (last_char == '/' || last_char == '\\') {
        is_dir = true;
    }

    if (is_dir) {
        Nob_File_Paths files = {0};
        size_t checkpoint = nob_temp_save();
        if (nob_read_entire_dir(ctest_file_path, &files)) {
            for (size_t i = 0; i < files.count; ++i) {
                orcstr_t filename = files.items[i];

                if (!nob_sv_end_with(nob_sv_from_cstr(filename), ".odl")) {
                    continue;
                }

                orcstr_t fullpath = nob_temp_sprintf("%s%s", ctest_file_path, filename);

                bool success = test(fullpath);
                if (!success) break;

                nob_temp_rewind(checkpoint);
            }

            nob_da_free(files);
        }
    } else {
        test(ctest_file_path);
    }
}