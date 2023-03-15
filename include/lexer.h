#ifndef LEXER_H_
#define LEXER_H_

#include "common.h"
#include "def.h"
#include "error_codes.h"

typedef enum TokenType {
    // single character
    TOKEN_PARENTHESIS_OPEN, TOKEN_PARENTHESIS_CLOSE,
    TOKEN_BRACE_OPEN, TOKEN_BRACE_CLOSE,
    TOKEN_BRACKET_OPEN, TOKEN_BRACKET_CLOSE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH,
    TOKEN_COLON, TOKEN_EQUAL, TOKEN_BANG, TOKEN_LESS, TOKEN_GREATER,
    TOKEN_SEMICOLON, TOKEN_BAR,

    // two character
    TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
    TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,

    // literals
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_SYMBOL, TOKEN_INTEGER, TOKEN_FLOAT, TOKEN_ANNOTATION,

    // keywords
    TOKEN_STRUCT, TOKEN_FUNCTION, TOKEN_NOT, TOKEN_AND, TOKEN_OR, TOKEN_IF, TOKEN_ELSE,
    TOKEN_TRUE, TOKEN_FALSE, TOKEN_NULL,

    // builtin functions
    TOKEN_PRINT_EXPR,

    TOKEN_ERROR, TOKEN_EOF, TOKEN_SIZE,
} TokenType;

typedef struct Token {
    char* start;
    i32 length;
    i32 line;
    TokenType type;
} Token;

typedef struct Lexer {
    OrsoErrorFunction error_fn;
    i32 line;
    char* start;
    char* current;
} Lexer;

void lexer_init(Lexer* state, const char* code);
Token lexer_next_token(Lexer* state);

#endif
