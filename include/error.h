#ifndef ERROR_H_
#define ERROR_H_

#include "lexer.h"

typedef enum OrsoErrorType {
    ORSO_ERROR_COMPILE,
    ORSO_ERROR_RUNTIME
} OrsoErrorType;

typedef enum OrsoErrorRegionType {
    ORSO_ERROR_REGION_TYPE_TOKEN,
    ORSO_ERROR_REGION_TYPE_RANGE,
    ORSO_ERROR_REGION_TYPE_RANGE2,
} OrsoErrorRegionType;

typedef struct error_t {
    char* message;
    OrsoErrorType type;
    OrsoErrorRegionType region_type;

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
