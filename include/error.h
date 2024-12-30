#ifndef ERROR_H_
#define ERROR_H_

#include "lexer.h"

typedef enum error_type_t {
    ERROR_PARSER_UNEXPECTED_TOKEN,
    ERROR_PARSER_EXPECTED_OPEN_BRACE,
    ERROR_PARSER_EXPECTED_CLOSE_BRACE,
    ERROR_PARSER_EXPECTED_DO_OR_BLOCK,
    ERROR_PARSER_EXPECTED_THEN_OR_BLOCK,
    ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS,
    ERROR_PARSER_EXPECTED_SEMICOLON,
    ERROR_PARSER_EXPECTED_TYPE,
    ERROR_PARSER_EXPECTED_DECLARATION_COLON,
    ERROR_PARSER_EXPECTED_DECLARATION,
    ERROR_PARSER_EXPECTED_EXPRESSION,
    ERROR_PARSER_EXPECTED_EOF,
    ERROR_PARSER_DOT_OPERATOR_ONLY_NAMES_FOR_NOW,
    ERROR_PARSER_TOO_MANY_PARAMETERS,

    ERROR_ANALYSIS_INVALID_UNARY_OPERAND,
    ERROR_ANALYSIS_INVALID_BINARY_OPERANDS,
    ERROR_ANALYSIS_INVALID_MEMBER_ACCESS,
    ERROR_ANALYSIS_INVALID_RETURN_TYPE,
    ERROR_ANALYSIS_EXPECTED_CONSTANT,
    ERROR_ANALYSIS_EXPECTED_LVALUE,
    ERROR_ANALYSIS_EXPECTED_CALLABLE,
    ERROR_ANALYSIS_EXPECTED_RESOLVED,
    ERROR_ANALYSIS_EXPECTED_DEFAULT_VALUE,
    ERROR_ANALYSIS_EXPECTED_TYPE,
    ERROR_ANALYSIS_TYPE_MISMATCH,
    ERROR_ANALYSIS_CANNOT_RETURN_INSIDE_RETURN,
    ERROR_ANALYSIS_CANNOT_ACCESS_MUTABLE_ON_DIFFERENT_FOLD_LEVEL,
    ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION,
    ERROR_ANALYSIS_CANNOT_FOLD_ON_ERRORED_FUNCTION_DEFINITION,
    ERROR_ANALYSIS_FOLDING_LOOP,
    ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES,
    ERROR_ANALYSIS_MEMBER_DOES_NOT_EXIST,
    ERROR_ANALYSIS_INVALID_TYPE_FOR_TYPE_INITIALIZER_LIST,
    ERROR_ANALYSIS_TOO_MANY_STRUCT_ARGUMENTS,

    ERROR_CODEGEN_NOT_ENOUGH_MEMORY,
    ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG,
    ERROR_COUNT
} error_type_t;

cstr_t const error_messages[ERROR_COUNT];
typedef enum error_region_type_t {
    ERROR_REGION_TYPE_TOKEN,
    ERROR_REGION_TYPE_RANGE,
    ERROR_REGION_TYPE_TWO_RANGES,
} error_region_type_t;

typedef struct error_t {
    error_type_t type;
    error_region_type_t region_type;
    token_t first;
    token_t first_end;
    token_t second;
    token_t second_end;
} error_t;

#endif
