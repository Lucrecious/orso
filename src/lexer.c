#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>

void lexer_state_init(LexerState* state, char* code) {
    state->start = code;
    state->current = code;
}

static bool is_at_end(LexerState* state) {
    return *state->current == '\0';
}

Token create_token(LexerState* state, TokenType type) {
    Token token = { 
        .type = type,
        .start = state->start,
        .length = state->current - state->start
    };

    return token;
}

char advance(LexerState* state) {
    state->current++;
    return state->current[-1];
}

char peak(LexerState* state) {
    return state->current[0];
}

Token lexer_next_token(LexerState* state) {
    // skip whitespace
    while (isspace(*state->current)) {
        state->current++;
    }

    state->start = state->current;

    if (is_at_end(state)) {
        return create_token(state, TokenType_EOF);
    }

    char c = advance(state);

    if (isalpha(c)) {
        while (isalnum(peak(state))) {
            advance(state);
        }

        return create_token(state, TokenType_IDENTIFIER);
    }

    if (isnumber(c)) {
        while(isnumber(peak(state))) {
            advance(state);
        }

        return create_token(state, TokenType_INTEGER);
    }

    switch (c) {
        case ':': return create_token(state, TokenType_COLIN);
        case '=': return create_token(state, TokenType_EQUALS);
    }

    return create_token(state, TokenType_INVALID);
}
