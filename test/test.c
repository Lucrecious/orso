#include <stdbool.h>

#include "interpreter.h"
#include "error.h"
#include "../nob.h"

#define DEFAULT_BUFFER_SIZE 1024

typedef struct {
    i32 capacity;
    i32 count;
    OrsoError* items;
} errors_t;
errors_t errors = {0};

i32 write_buffer_count = 0;
char write_buffer[DEFAULT_BUFFER_SIZE];

void error_to_buffer(OrsoError error) {
    nob_da_append(&errors, error);
    switch (error.type) {
        case ORSO_ERROR_COMPILE: {
            switch (error.region_type) {
                case ORSO_ERROR_REGION_TYPE_TOKEN:
                case ORSO_ERROR_REGION_TYPE_RANGE:
                case ORSO_ERROR_REGION_TYPE_RANGE2: {
                    printf("test:%d: %s\n", error.region.token.line, error.message);
                    break;
                }
            }
            break;
        }
        case ORSO_ERROR_RUNTIME: UNREACHABLE();
    }
}

void write_to_buffer(const char* chars) {
    write_buffer_count += snprintf(write_buffer + write_buffer_count, 1024 - write_buffer_count, "%s", chars);
}

static size_t snprintfv(char* buffer, size_t len, const char* format, const char** args, size_t argc) {
    switch (argc) {
        case 0: return snprintf(buffer, len, "%s", format);
        case 1: return snprintf(buffer, len, format, args[0]);
        case 2: return snprintf(buffer, len, format, args[0], args[1]);
        case 3: return snprintf(buffer, len, format, args[0], args[1], args[2]);
        default: UNREACHABLE();
    }

    return 0;
}

bool test_template(
        const char* code_template,
        const char* output_template,
        const char** code_args, size_t code_arg_count,
        const char** output_args, size_t output_arg_count) {
    errors.count = 0;
    write_buffer_count = 0;
    write_buffer[0] = '\0';

    char source_buffer[DEFAULT_BUFFER_SIZE];
    snprintfv(source_buffer, DEFAULT_BUFFER_SIZE, code_template, code_args, code_arg_count);

    OrsoVM vm;
    orso_vm_init(&vm, write_to_buffer, 1000);

    orso_run_source(&vm, source_buffer, error_to_buffer);

    orso_vm_free(&vm);

    char expected_buffer[DEFAULT_BUFFER_SIZE];
    snprintfv(expected_buffer, DEFAULT_BUFFER_SIZE, output_template, output_args, output_arg_count);

    if (strcmp(expected_buffer, write_buffer) == 0) {
        return true;
    }

    printf("Test failed for source:\n    %s\n", source_buffer);
    printf("***\n");
    printf("Expected: %s\n", expected_buffer);
    printf("Got:      %s\n\n", write_buffer);
    return false;
}

bool test_variable_declarations() {
    bool success = true;
    {
        const char* source_args[] = { "i32", "i64", "f64", "f32", "bool", "string", "symbol", "void", "string|void" };
        const char* output_args[] = { 
            "i32",      "0",
            "i64",      "0",
            "f64",      "0.0",
            "f32",      "0.0",
            "bool",     "false",
            "string",   "",
            "symbol",   "''",
            "void",     "null",
            "void",     "null"
        };

        size_t count = len(source_args);

        const char* global_type_no_value = "value: %s; main :: () -> i32 { print_expr value; return 0; };";
        char* output = "value (%s) => %s\n";

        for (size_t i = 0; i < count; i++) {
            success = test_template(
                    global_type_no_value, output,
                    source_args + i, 1,
                    output_args + i * 2, 2) && success;
        }

        const char* local_type_no_value = "main :: () -> i32 { value: %s; print_expr value; return 0; };";

        for (size_t i = 0; i < count; i++) {
            success = test_template(
                    local_type_no_value, output,
                    source_args + i, 1,
                    output_args + i * 2, 2) && success;
        }
    }
    
    {
        const char* source_args[] = { "42", "42.0", "true", "\"forty-two\"", "'forty-two'", "null", "\"forty-two\"" };
        const char* output_args[] = {
            "i32",          "42",
            "f64",          "42.0",
            "bool",         "true",
            "string",       "forty-two",
            "symbol",       "'forty-two'",
            "void",         "null",
            "string",       "forty-two",
        };

        size_t count = len(source_args);

        const char* global_no_type_value = "value := %s; main :: () -> i32 { print_expr value; return 0; };";
        const char* output = "value (%s) => %s\n";

        for (size_t i = 0; i < count; i++) {
            success = test_template(
                    global_no_type_value, output,
                    source_args + i, 1,
                    output_args + i * 2, 2) && success;
        }

        const char* local_no_type_value = "main :: () -> i32 { value := %s; print_expr value; return 0; };";

        for (size_t i = 0; i < count; i++) {
            success = test_template(
                local_no_type_value, output,
                source_args + i, 1,
                output_args + i * 2, 2) && success;
        }
    }

    return success;
}

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;
    if (!test_variable_declarations()) {
        return 1;
    }

    nob_da_free(errors);
}