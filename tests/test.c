#include "../nob.h"
#include <stdlib.h>

#include "orso.h"
#include "parser.h"

#include <unistd.h>

// from orso.c
ast_t *orbuild_ast(orstring_t source, arena_t *arena, orstring_t file_path);
void print_errors(ast_t *ast);

void print_usage(void) {
    printf("usage :\n");
    printf("  test              - prints this usage text\n");
    printf("  test <testpath>   - <testpath> is the relative path to the odl file to test, paths ending in / or \\\\ will test the directory recursively\n");
    printf("  errors [tag]      - [tag] is the full or sub-path of the error, otherwise all are tested");
    printf("\n");
}

static bool test(cstr_t test_file_path) {
    {
        size_t len = strlen(test_file_path);

        printf("---------------");
        for (size_t i = 0; i < len; ++i) printf("-");
        printf("\n");

        printf("TEST ---- %s ----\n", test_file_path);

        printf("---------------");
        for (size_t i = 0; i < len; ++i) printf("-");
        printf("\n");
    }

    orso_compiler_t compiler = {0};
    compiler.build_dir = S("./tmp/");
    compiler.output_name = S("tmp");
    compiler.src = (orstring_t) {
        .cstr = (char*)test_file_path,
        .length = strlen(test_file_path),
    };

    bool success = orbuild(&compiler);
    if (!success) goto defer;

    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "./tmp/tmp");

    Nob_Fd fdout = nob_fd_open_for_write("./tmp/tmp.c.out");
    if (fdout == NOB_INVALID_FD) return false;

    success = nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout = &fdout,
    });
    if (!success) goto defer;

    nob_cmd_append(&cmd, "./bin/orso", "run", test_file_path);

    fdout = nob_fd_open_for_write("./tmp/tmp.or.out");
    if (fdout == NOB_INVALID_FD) goto defer;

    success = nob_cmd_run_sync_redirect_and_reset(&cmd, (Nob_Cmd_Redirect){
        .fdout = &fdout
    });
    if (!success) goto defer;

    nob_cmd_append(&cmd, "diff", "./tmp/tmp.c.out", "./tmp/tmp.or.out");
    success = nob_cmd_run_sync_and_reset(&cmd);
    if (success) {
        cstr_t compare_file = nob_temp_sprintf("%s.out", test_file_path);
        if (!nob_file_exists(compare_file)) {
            nob_cmd_append(&cmd, "cat", "./tmp/tmp.c.out");
            success = nob_cmd_run_sync_and_reset(&cmd);

            printf("Compare file for %s does not yet exist, do you want to create it from the output? y/n\n", compare_file);
            char response;
            scanf(" %c", &response);

            if (response == 'y') {
                nob_copy_file("./tmp/tmp.c.out", compare_file);
            }
        }

        if (!nob_file_exists(compare_file)) {
            printf("No output file to compare against.\n");
        } else {
            nob_cmd_append(&cmd, "diff", "./tmp/tmp.c.out", compare_file);
            success = nob_cmd_run_sync_and_reset(&cmd);
        }
    }

defer:
    printf("\n\n");
    return success;
}

typedef struct cstrs_t cstrs_t;
struct cstrs_t {
    cstr_t *items;
    size_t capacity;
    size_t count;
};

#define da_push(arr, item) do { \
    size_t new_capacity = (arr)->capacity; \
    while ((arr)->count >= new_capacity) \
        new_capacity = new_capacity == 0 ? 8 : new_capacity*2; \
    if (new_capacity != (arr)->capacity) { \
        (arr)->items = realloc((arr)->items, new_capacity*sizeof(*((arr)->items))); \
        (arr)->capacity = new_capacity; \
    } \
    (arr)->items[((arr)->count)++] = (item); \
} while (0)

static bool find_char_before_eol(cstr_t s_, char c, size_t *result_index) {
    size_t index = 0;
    char *s = (char*)s_;
    while (*s != '\0' && *s != '\n') {
        if (*s == c) {
            *result_index = index;
            return true;
        }

        ++index;
        ++s;
    }

    return false;
}

typedef struct error_tag_t error_tag_t;
struct error_tag_t {
    cstr_t file;
    size_t line;
    size_t column;
    cstr_t tag;
};

typedef struct error_tags_t error_tags_t;
struct error_tags_t {
    error_tag_t *items;
    size_t count;
    size_t capacity;
};

static error_tags_t get_all_error_tags() {
    const size_t path_count = 3;
    cstr_t src_paths[path_count] = {
        "./src/parser.c",
        "./src/static_analyzer.c",
        "./include/codegen.h"
    };

    error_tags_t tags = {0};

    const cstr_t tag_label = ".tag";
    const size_t tag_size = strlen(tag_label);

    for (size_t i = 0; i < path_count; ++i) {
        cstr_t path = src_paths[i];
        Nob_String_Builder sb = {0};
        bool success = nob_read_entire_file(path, &sb);
        NOB_ASSERT(success);

        size_t line = 0;
        size_t column = 0;

        for (size_t j = 0; j < sb.count; ++j) {
            ++column;

            if (sb.items[j] == '\n') {
                ++line;
                column = 0;
            }

            if (strncmp(tag_label, sb.items+j, tag_size) == 0) {
                size_t tag_start = 0;
                success = find_char_before_eol(sb.items+j, '\"', &tag_start);
                NOB_ASSERT(success);
                tag_start += j+1;

                size_t tag_end = 0;
                success = find_char_before_eol(sb.items+tag_start, '\"', &tag_end);
                NOB_ASSERT(success);

                tag_end += tag_start;

                size_t count = tag_end - tag_start;
                cstr_t tag = strndup(sb.items+tag_start, count);

                error_tag_t error_tag = {
                    .file = path,
                    .line = line,
                    .column = column,
                    .tag = tag,
                };

                da_push(&tags, error_tag);

                j = tag_end+1;
            }
        }

        nob_sb_free(sb);
    }

    return tags;
}

static void log_error_tag_location(error_tag_t tag, bool is_skipping) {
    nob_log(NOB_INFO, "%s%s:%zu:%zu: %s", is_skipping ? "SKIPPED -- " : "", tag.file, tag.line+1, tag.column, tag.tag);
}

static bool check_error(ast_t *ast, error_tag_t expected_tag, size_t expected_error_count, size_t check_index) {
    MUST(check_index < expected_error_count);

    if (ast->errors.count != expected_error_count) {
        nob_log(NOB_ERROR, "expected 1 error but got '%zu' instead", ast->errors.count);
        if (ast->errors.count > 0) {
            print_errors(ast);
        }
        return false;
    }

    orcstr_t actual_tag = ast->errors.items[check_index].tag;
    if (strcmp(actual_tag, expected_tag.tag) != 0) {
        nob_log(NOB_ERROR, "expected error '%s' error but got '%s' instead", expected_tag.tag, actual_tag);
        print_errors(ast);
        return false;
    }

    return true;
}

int main(int argc, char *argv[]) {
    nob_shift(argv, argc);

    if (argc == 0) {
        print_usage();
        return 1;
    }

    cstr_t command = argv[0];

    if (strcmp(command, "errors") == 0) {
        error_tags_t tags = get_all_error_tags();
        nob_log(NOB_INFO, "error tag count: %zu", tags.count);
        size_t i;
        size_t skipped = 0;
        for (i = 0; i < tags.count; ++i) {
            nob_temp_reset();

            error_tag_t tag = tags.items[i];
            
            bool is_skipping = false;
            {
                string_view_t sv = {
                    .data = tag.tag,
                    .length = strlen(tag.tag),
                };

                if (sv_ends_with(sv, "|skip")) {
                    is_skipping = true;
                    ++skipped;
                }
            }

            cstr_t filepath = nob_temp_sprintf("./tests/errors/%s.or", tag.tag);

            log_error_tag_location(tag, is_skipping);

            if (is_skipping) continue;

            Nob_String_Builder sb = {0};
            bool success = nob_read_entire_file(filepath, &sb);
            if (!success) {
                nob_log(NOB_ERROR, "could not run test for error: '%s'", tag.tag);
                nob_log(NOB_INFO, "create test file? y/n");

                static const orstring_t error_template = lit2str("main :: () -> void {\n};\n");

                char response;
                scanf(" %c", &response);
                if (response == 'y' || response == 'Y') {
                    success = nob_write_entire_file(filepath, error_template.cstr, error_template.length);
                    if (success) {
                        nob_log(NOB_INFO, "test file %s was created successfully", filepath);
                    } else {
                        nob_log(NOB_ERROR, "unable to create test file %s", filepath);
                    }
                }
                break;
            }

            orstring_t src = {
                .cstr = sb.items,
                .length = sb.count,
            };

            arena_t arena = {0};
            orstring_t filepath_ = {
                .cstr = (char*)filepath,
                .length = strlen(filepath),
            };
            ast_t *ast = orbuild_ast(src, &arena, filepath_);

            success = false;

            // exceptions
            if (strcmp(tag.tag, "sem.fficall-mismatch.return-type") == 0
            || strcmp(tag.tag, "sem.type-mismatch.fficall-arg") == 0
            || strcmp(tag.tag, "sem.arg-count-mismatch.fiicall-call") == 0) {
                success = check_error(ast, tag, 3, 2);
            } else {
                success = check_error(ast, tag, 1, 0);
            }

            arena_free(&arena);

            nob_sb_free(sb);

            if (!success) break;
        }

        nob_log(NOB_INFO, "error tag tests: %zu/%zu", i, tags.count);
        nob_log(NOB_INFO, "error tag skipped: %zu", skipped);
    } else if (strcmp(command, "test") == 0) {
        Nob_File_Paths paths = {0};
        cstr_t test_dir = "./tests/correctness/";
        bool success = nob_read_entire_dir(test_dir, &paths);
        if (!success) {
            printf("cannot read %s directory", test_dir);
            nob_da_free(paths);
            return 1;
        }

        // todo: deep copy paths into arena, so it's not on temp buffer

        size_t test_count = 0;
        size_t tests_passed = 0;
        for (size_t i = 0; i < paths.count; ++i) {
            cstr_t file = paths.items[i];
            if (sv_ends_with(cstr2sv(file), ".or")) {
                ++test_count;
                cstr_t path = nob_temp_sprintf("%s%s", test_dir, file);
                if (test(path)) {
                    ++tests_passed;
                }
            }
        }

        nob_da_free(paths);
        printf("%zu/%zu tests passed", tests_passed, test_count);
    }

    return 0;
}