#define NOB_IMPLEMENTATION
#include "../nob.h"

typedef const char *cstr_t;

void print_usage(void) {
    printf("usage :\n");
    printf("  test              - prints this usage text\n");
    printf("  test <testpath>   - <testpath> is the relative path to the odl file to test\n");
    printf("\n");
}

int main(int argc, char *argv[]) {
    nob_shift(argv, argc);

    cstr_t test_file_path;
    if (argc) {
        test_file_path = argv[0];
    } else {
        print_usage();
        return 0;
    }

    cstr_t native_results_path = nob_temp_sprintf("%s.result.native", test_file_path);
    cstr_t interp_results_path = nob_temp_sprintf("%s.result.interp", test_file_path);
    cstr_t executable_file_path = nob_temp_sprintf("%s.out", test_file_path);
    
    Nob_Fd nativefd = nob_fd_open_for_write(native_results_path);
    Nob_Fd interpfd = nob_fd_open_for_write(interp_results_path);

    int result = 0;

    if (nativefd == NOB_INVALID_FD || interpfd == NOB_INVALID_FD) nob_return_defer(1);

    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "./bin/orso", "build", test_file_path, executable_file_path);

    if (!nob_cmd_run_sync_and_reset(&cmd)) exit(1);

    cstr_t call_test = nob_temp_sprintf("./%s", executable_file_path);
    nob_cmd_append(&cmd, call_test);
    if (!nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout = &nativefd,
    })) nob_return_defer(1);

    nob_cmd_append(&cmd, "./bin/orso", "run", test_file_path);

    if (!nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout=&interpfd,
    })) nob_return_defer(1);

    nob_cmd_append(&cmd, "diff", native_results_path, interp_results_path);

    if (!nob_cmd_run_sync_and_reset(&cmd)) nob_return_defer(1);

defer:
    nob_fd_close(nativefd);
    nob_fd_close(interpfd);
    exit(result);
}