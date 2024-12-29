#ifndef LEXER_H_
#define LEXER_H_

#include "common.h"
#include "def.h"
#include "stringt.h"

typedef enum token_type_t token_type_t;
enum token_type_t {
    // single character
    TOKEN_PARENTHESIS_OPEN, TOKEN_PARENTHESIS_CLOSE,
    TOKEN_BRACE_OPEN, TOKEN_BRACE_CLOSE,
    TOKEN_BRACKET_OPEN, TOKEN_BRACKET_CLOSE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH,
    TOKEN_COLON, TOKEN_EQUAL, TOKEN_BANG, TOKEN_LESS, TOKEN_GREATER,
    TOKEN_SEMICOLON, TOKEN_BAR, TOKEN_AMPERSAND,

    // two character
    TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
    TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,
    TOKEN_ARROW_RIGHT,

    // literals
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_SYMBOL, TOKEN_INTEGER, TOKEN_FLOAT, TOKEN_ANNOTATION,
    TOKEN_DIRECTIVE,

    // keywords
    TOKEN_STRUCT, TOKEN_NOT, TOKEN_AND, TOKEN_OR,
    TOKEN_IF, TOKEN_UNLESS, TOKEN_ELSE, TOKEN_THEN,
    TOKEN_WHILE, TOKEN_UNTIL, TOKEN_FOR, TOKEN_DO,
    TOKEN_TRUE, TOKEN_FALSE, TOKEN_NULL,
    TOKEN_RETURN,

    // builtin functions
    TOKEN_PRINT_EXPR, TOKEN_PRINT,

    TOKEN_ERROR, TOKEN_EOF, TOKEN_SIZE,
};

typedef struct text_location_t text_location_t;
struct text_location_t {
    size_t line;
    size_t column;
};

#define texloc(line_, col) ((text_location_t){.line=(line_), .column=(col)})

typedef struct token_t token_t;
struct token_t {
    string_t file_path;
    string_view_t source_view;
    text_location_t start_location;
    token_type_t type;
};

#define token_end_location(token) ((text_location_t){\
    .line = ((token)->start_location.line),\
    .column=(((token)->start_location.column + (token)->source_view.length))\
})

typedef struct lexer_t lexer_t;
struct lexer_t {
    token_t previous_token;
    error_function_t error_fn;
    string_t file_path;
    cstr_t source;
    i32 line;
    cstr_t start;
    cstr_t line_start;
    cstr_t current;
};

void lexer_init(lexer_t *state, string_t file_path, cstr_t code);
token_t lexer_next_token(lexer_t *state);

#endif
