#ifndef ERROR_H_
#define ERROR_H_

#include "lexer.h"

typedef enum error_type_t {
    ORSO_ERROR_COMPILE,
    ORSO_ERROR_RUNTIME
} error_type_t;

typedef enum error_region_type_t {
    ORSO_ERROR_REGION_TYPE_TOKEN,
    ORSO_ERROR_REGION_TYPE_RANGE,
    ORSO_ERROR_REGION_TYPE_RANGE2,
} error_region_type_t;

typedef struct error_t {
    char* message;
    error_type_t type;
    error_region_type_t region_type;

    union {
        token_t token;
        struct {
            token_t start;
            token_t end;
        } range;

        struct {
            token_t start1;
            token_t end1;

            token_t start2;
            token_t end2;
        } range2;

    } region;
} error_t;

#endif
