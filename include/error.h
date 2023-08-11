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

typedef struct OrsoError {
    char* message;
    OrsoErrorType type;
    OrsoErrorRegionType region_type;

    union {
        Token token;
        struct {
            Token start;
            Token end;
        } range;

        struct {
            Token start1;
            Token end1;

            Token start2;
            Token end2;
        } range2;

    } region;
} OrsoError;

#endif
