#ifndef LEXER_H_
#define LEXER_H_

#include "def.h"
#include "error_codes.h"
#include "list.h"

typedef enum TokenType {
    TokenType_INVALID,
    TokenType_IDENTIFIER,
    TokenType_COLIN,
    TokenType_EQUALS,
    TokenType_INTEGER,
    TokenType_EOF,
    TokenType_SIZE,
} TokenType;

typedef struct Token {
    char* start;
    i32 length;
    TokenType type;
} Token;

typedef struct LexerState {
    char* start;
    char* current;
} LexerState;

void lexer_state_init(LexerState* state, char* code);
Token lexer_next_token(LexerState* state);

#endif