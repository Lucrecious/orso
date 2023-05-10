#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "def.h"
#include "virtual_machine.h"
#include "interpreter.h"
#include "error_codes.h"

char test_buffer[1024];
i32 buffer_length;

void clear_test_buffer(void) {
    buffer_length = 0;
    test_buffer[0] = '\0';
}

void write_to_test_buffer(const char* chars) {
    buffer_length += sprintf(test_buffer + buffer_length, "%s", chars);
}

void write_to_test_error_buffer(OrsoErrorType type, i32 line, const char* message) {
    char* error_type;
    switch (type) {
        case ORSO_ERROR_COMPILE: {
            error_type = "compile error";
            break;
        }
        case ORSO_ERROR_RUNTIME: {
            error_type = "runtime error";
            break;
        }
    }

    buffer_length += sprintf(test_buffer + buffer_length, "%s, %d: %s\n", error_type, line, message);
}

typedef struct Test {
    char* name;
    char* code;
    char* expected;
} Test;

typedef enum ReadingPhase {
    READING_PHASE_IDLE,
    READING_PHASE_INPUT,
    READING_PHASE_EXPECTED,
} ReadingPhase;

void parse_tests(const char* filename, Test* tests, int* numTests) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        printf("Failed to open file: %s\n", filename);
        exit(1);
    }

    char line[256];
    i32 current_test = -1;
    ReadingPhase phase = READING_PHASE_IDLE;

    while (fgets(line, sizeof(line), file)) {
        if (strncmp(line, "## ", 3) == 0) {
            current_test++;
            tests[current_test].name = strdup(line + 3);
            tests[current_test].name[strlen(tests[current_test].name) - 1] = '\0';
            tests[current_test].code = NULL;
            tests[current_test].expected = NULL;

            phase = READING_PHASE_INPUT;
        } else if (strncmp(line, "# ---", 5) == 0) {
            if (phase == READING_PHASE_INPUT) {
                phase = READING_PHASE_EXPECTED;
            } else if (phase == READING_PHASE_EXPECTED) {
                phase = READING_PHASE_IDLE;
            }
        } else if (phase == READING_PHASE_EXPECTED) {
            if (!tests[current_test].expected) {
                tests[current_test].expected = strdup(line);
            } else {
                i32 length = strlen(tests[current_test].expected);
                tests[current_test].expected = realloc(tests[current_test].expected, length + strlen(line) + 1);
                strcat(tests[current_test].expected, line);
            }
        } else if (phase == READING_PHASE_INPUT) {
            // Populate code section of current test case
            if (!tests[current_test].code) {
                tests[current_test].code = strdup(line);
            } else {
                i32 length = strlen(tests[current_test].code);
                tests[current_test].code = realloc(tests[current_test].code, length + strlen(line) + 1);
                strcat(tests[current_test].code, line);
            }
        }
    }

    fclose(file);
    *numTests = current_test + 1;
}

bool run_test(OrsoInterpreter* interpreter, Test* test) {
    orso_interpreter_init(interpreter, write_to_test_buffer, write_to_test_error_buffer);

    char* source = test->code;
    orso_interpreter_run(interpreter, source);

    orso_interpreter_free(interpreter);

    char* expected = test->expected;
    size_t expected_length = strlen(expected);
    if (expected_length != (u64)buffer_length) {
        return false;
    }

    if (memcmp(expected, test_buffer, buffer_length) != 0) {
        return false;
    }

    return true;
}

int main(int argc, char** argv) {
    Test tests[100];
    i32 test_count;

    if (argc < 2) {
        printf("Needs test file as argument.");
        exit(1);
    }

    parse_tests(argv[1], tests, &test_count);

    bool test_failed = false;

    OrsoInterpreter interpreter;
    for (i32 i = 0; i < test_count; i++) {
        clear_test_buffer();

        bool passed = run_test(&interpreter, &tests[i]);

        if (!passed) {
            printf("Test '%s' failed.\nExpected:\n%sActual:\n%.*s\n\n", tests[i].name, tests[i].expected, buffer_length, test_buffer);
            test_failed = true;
        }

        free(tests[i].name);
        free(tests[i].code);
        free(tests[i].expected);
    }

    return test_failed ? 1 : 0;
}
