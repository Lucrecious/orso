#include <stdbool.h>

#include "interpreter.h"
#include "error.h"
#include "nob.h"

typedef struct {
    i32 capacity;
    i32 count;
    OrsoError* items;
} errors_t;
errors_t errors = {0};

char write_buffer[1024];

void error_to_buffer(OrsoError error) {
    nob_da_append(&errors, error);
}

void write_to_buffer(const char* chars) {
    sprintf(write_buffer, "%s", chars);
}

bool test_global_variable_declaration_explicit_type_implicit_value(const char* type, const char* printed_value, const char* printed_type) {
    errors.count = 0;
    write_buffer[0] = '\0';

    OrsoVM vm;
    orso_vm_init(&vm, write_to_buffer, 1000);

    char source_template[] = "value: %s; int main() { print_expr value; };";
    char expected_template[] = "value (%s) => %s";

    char source_buffer[1024];
    sprintf(source_buffer, source_template, type);

    orso_run_source(&vm, source_buffer, error_to_buffer);

    orso_vm_free(&vm);

    char expected_buffer[1024];
    sprintf(expected_buffer, expected_template, printed_type, printed_value);

    if (strcmp(expected_buffer, write_buffer) == 0) {
        return true;
    }

    printf("Test failed for source: %s\n", source_buffer);
    printf("Expected: %s | Got: %s\n\n", expected_buffer, write_buffer);
    return false;
}

bool test_variable_declarations() {
    {
        char* types = { "i32", "i64", "f32", "bool", "string", "symbol", "void", "string|void" };
        char* print_types = { "i32", "i64", "f32", "string", "symbol", "void", "void" };
        char* print_values = { "0", "0", "0.0", "", "''", "null", "null" };
        size_t count = sizeof(types) / sizeof(types[0]);

        for (size_t i = 0; i < count; i++) {
            char* type = types[i];
            char* print_type = print_types[i];
            char* print_value = print_values[i];
            bool error = test_global_variable_declaration_explicit_type_implicit_value(type, print_value, print_type);
            if (!error) {
                continue;
            }
            return false;
        }
    }

    return true;
}

int main(int argc, char** argv) {
    if (!test_variable_declarations()) {
        return 1;
    }

    nob_da_free(errors);
}