#ifndef LEXER_H_
#define LEXER_H_

#include "intrinsics.h"
#include "stringt.h"

typedef enum token_type_t token_type_t;
enum token_type_t {
    TOKEN_PARENTHESIS_OPEN, TOKEN_PARENTHESIS_CLOSE,
    TOKEN_BRACE_OPEN, TOKEN_BRACE_CLOSE,
    TOKEN_BRACKET_OPEN, TOKEN_BRACKET_CLOSE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_COLON, TOKEN_SEMICOLON,

    TOKEN_MINUS, TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT, TOKEN_PERCENT_PERCENT,
    TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS, TOKEN_LESS_LESS,

    TOKEN_EQUAL, TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_SLASH_EQUAL, TOKEN_STAR_EQUAL,
    TOKEN_AND_EQUAL, TOKEN_OR_EQUAL, TOKEN_PERCENT_EQUAL, TOKEN_PERCENT_PERCENT_EQUAL, TOKEN_NOT_EQUAL,

    TOKEN_BANG, TOKEN_LESS, TOKEN_GREATER,
    TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,
    TOKEN_BAR, TOKEN_AMPERSAND, TOKEN_SQUIGGLE,
    TOKEN_NOT, TOKEN_AND, TOKEN_OR, TOKEN_AS,
    TOKEN_ARROW_RIGHT,

    TOKEN_IDENTIFIER, TOKEN_INFERRED_TYPE,
    TOKEN_STRING, TOKEN_SYMBOL, TOKEN_INTEGER, TOKEN_FLOAT, TOKEN_ANNOTATION,
    TOKEN_DIRECTIVE,

    TOKEN_STRUCT,

    TOKEN_IF, TOKEN_UNLESS, TOKEN_ELSE, TOKEN_THEN,
    TOKEN_WHILE, TOKEN_UNTIL, TOKEN_FOR, TOKEN_DO,
    TOKEN_TRUE, TOKEN_FALSE,

    TOKEN_RETURN, TOKEN_BREAK, TOKEN_CONTINUE,

    TOKEN_TYPEOF, TOKEN_SIZEOF, TOKEN_LEN,

    TOKEN_IMPLICIT,

    TOKEN_EOF,

    TOKEN_ERROR,

    TOKEN_SIZE,
};

typedef enum operator_type_t operator_type_t;
enum operator_type_t {
    OPERATOR_TYPE_NONE = 0,
    OPERATOR_TYPE_ARITHMETIC = 1,
    OPERATOR_TYPE_EQUALITY = 1 << 1,
    OPERATOR_TYPE_COMPARISON = 1 << 2,
    OPERATOR_TYPE_LOGICAL = 1 << 3,
};

typedef struct operator_t operator_t;
struct operator_t {
    operator_type_t type;
};

operator_t operators[TOKEN_SIZE];

#define operator_is_arithmetic(op) (operators[op].type == OPERATOR_TYPE_ARITHMETIC)
#define operator_is_comparing(op) (operators[op].type == OPERATOR_TYPE_COMPARISON)
#define operator_is_equating(op) (operators[op].type == OPERATOR_TYPE_EQUALITY)
#define operator_is_logical(op) (operators[op].type == OPERATOR_TYPE_LOGICAL)

typedef struct texloc_t texloc_t;
struct texloc_t {
    string_t filepath;
    size_t line;
    size_t column;
};

#define texloc(filepath_, line_, col) ((texloc_t){.filepath=(filepath_), .line=(line_), .column=(col)})

typedef struct token_t token_t;
struct token_t {
    string_view_t source;
    string_view_t view;
    texloc_t loc;
    token_type_t type;
};

#define nil_token (token_t){\
    .source = lit2sv(""),\
    .loc.line = 0,\
    .loc.column= 0,\
    .view = lit2sv(""),\
    .type = TOKEN_ERROR\
}

#define token_end_loc(token) ((texloc_t){\
    .filepath = ((token)->loc.filepath), \
    .line = ((token)->loc.line), \
    .column=(((token)->loc.column + (token)->view.length)) \
})

typedef struct lexer_t lexer_t;
struct lexer_t {
    token_t previous_token;
    string_t file_path;
    string_view_t source;
    s32 line;
    cstr_t start;
    cstr_t line_start;
    cstr_t current;
};

void lexer_init(lexer_t *state, string_t file_path, string_view_t code);
token_t lexer_next_token(lexer_t *state);

#endif
