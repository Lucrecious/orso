#ifndef LEXER_H_
#define LEXER_H_

#include "def.h"
#include "stringt.h"

typedef enum token_type_t token_type_t;
enum token_type_t {
    TOKEN_PARENTHESIS_OPEN, TOKEN_PARENTHESIS_CLOSE,
    TOKEN_BRACE_OPEN, TOKEN_BRACE_CLOSE,
    TOKEN_BRACKET_OPEN, TOKEN_BRACKET_CLOSE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_COLON, TOKEN_SEMICOLON,

    TOKEN_MINUS, TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT, TOKEN_PERCENT_PERCENT,
    TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
    TOKEN_EQUAL, TOKEN_BANG, TOKEN_LESS, TOKEN_GREATER,
    TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,
    TOKEN_BAR, TOKEN_AMPERSAND,
    TOKEN_NOT, TOKEN_AND, TOKEN_OR,
    TOKEN_ARROW_RIGHT,

    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_SYMBOL, TOKEN_INTEGER, TOKEN_FLOAT, TOKEN_ANNOTATION,
    TOKEN_DIRECTIVE,

    TOKEN_STRUCT,

    TOKEN_IF, TOKEN_UNLESS, TOKEN_ELSE, TOKEN_THEN,
    TOKEN_WHILE, TOKEN_UNTIL, TOKEN_FOR, TOKEN_DO,
    TOKEN_TRUE, TOKEN_FALSE,

    TOKEN_RETURN, TOKEN_BREAK, TOKEN_CONTINUE,

    TOKEN_IMPLICIT,

    TOKEN_EOF,

    TOKEN_ERROR,

    TOKEN_SIZE,
};

typedef struct text_location_t text_location_t;
struct text_location_t {
    size_t line;
    size_t column;
};

#define texloc(line_, col) ((text_location_t){.line=(line_), .column=(col)})

typedef struct token_t token_t;
struct token_t {
    string_view_t source;
    string_view_t view;
    text_location_t location;
    token_type_t type;
};

#define nil_token (token_t){\
    .source = lit2sv(""),\
    .location.line = 0,\
    .location.column= 0,\
    .view = lit2sv(""),\
    .type = TOKEN_ERROR\
}

#define token_end_location(token) ((text_location_t){\
    .line = ((token)->location.line),\
    .column=(((token)->location.column + (token)->view.length))\
})

typedef struct lexer_t lexer_t;
struct lexer_t {
    token_t previous_token;
    string_t file_path;
    string_view_t source;
    i32 line;
    cstr_t start;
    cstr_t line_start;
    cstr_t current;
};

void lexer_init(lexer_t *state, string_t file_path, string_view_t code);
token_t lexer_next_token(lexer_t *state);

#endif
