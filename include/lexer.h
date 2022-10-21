#ifndef LEXER_H_
#define LEXER_H_

#include "def.h"
#include "error_codes.h"

typedef enum TokenType {
    // single character
    TOKEN_PARENTHESIS_OPEN, TOKEN_PARENTHESIS_CLOSE,
    TOKEN_BRACE_OPEN, TOKEN_BRACE_CLOSE,
    TOKEN_BRACKET_OPEN, TOKEN_BRACKET_CLOSE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH,
    TOKEN_COLIN, TOKEN_EQUAL, TOKEN_BANG, TOKEN_LESS, TOKEN_GREATER,

    // two character
    TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
    TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL,

    // literals
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_INTEGER, TOKEN_DECIMAL, TOKEN_ANNOTATION,

    // keywords
    TOKEN_STRUCT, TOKEN_VAR, TOKEN_FUNCTION,

    TOKEN_ERROR, TOKEN_EOF, TOKEN_SIZE,
} TokenType;

typedef struct Token {
    char* start;
    i32 length;
    i32 line;
    TokenType type;
} Token;

typedef struct Lexer {
    i32 line;
    char* start;
    char* current;
} Lexer;

void lexer_state_init(Lexer* state, char* code);
Token lexer_next_token(Lexer* state);

#endif
