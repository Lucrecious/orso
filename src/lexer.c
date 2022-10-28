#include "lexer.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void lexer_init(Lexer* lexer, const char* code) {
    lexer->line = 0;
    lexer->start = (char*)code;
    lexer->current = (char*)code;
}

static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || c == '_';
}

static bool is_at_end(Lexer* lexer) {
    return *lexer->current == '\0';
}

static Token create_token(Lexer* lexer, TokenType type) {
    Token token = { 
        .type = type,
        .start = lexer->start,
        .length = lexer->current - lexer->start,
        .line = lexer->line,
    };

    return token;
}

static Token error_token(Lexer* lexer, const char* message) {
    Token token = {
        .type = TOKEN_ERROR,
        .start = (char*)message,
        .length = (i32)(strlen(message)),
        .line = lexer->line,
    };
    
    return token;
}

static char advance(Lexer* lexer) {
    lexer->current++;
    return lexer->current[-1];
}

static char FORCE_INLINE peek(Lexer* lexer) {
    return lexer->current[0];
}

static char peek_next(Lexer* lexer) {
    if (is_at_end(lexer)) {
        return '\0';
    }

    return lexer->current[1];
}

static bool match(Lexer* lexer, char expected) {
    if (is_at_end(lexer)) {
        return false;
    }

    if (*lexer->current != expected) {
        return false;
    }

    lexer->current++;
    return true;
}

static void skip_whitespace(Lexer* lexer) {
    for (;;) {
        char c = peek(lexer);
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
                advance(lexer);
                break;
            case '\n':
                lexer->line++;
                advance(lexer);
                break;
            default:
                return;
        }
    }
}

static FORCE_INLINE Token _string_symbol(Lexer* lexer, char terminator, TokenType type) {
    while (peek(lexer) != terminator && !is_at_end(lexer)) {
        if (peek(lexer) == '\n') {
            lexer->line++;
        }
        advance(lexer);
    }

    if (is_at_end(lexer)) {
        return error_token(lexer, "Unterminated string.");
    }

    advance(lexer);
    return create_token(lexer, type);
}

static Token string(Lexer* lexer) {
    return _string_symbol(lexer, '"', TOKEN_STRING);
}

static Token symbol(Lexer* lexer) {
    return _string_symbol(lexer, '\'', TOKEN_SYMBOL);
}

static Token annotation(Lexer* lexer) {
    while (is_alpha(peek(lexer)) || is_digit(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, TOKEN_ANNOTATION);
}

static Token number(Lexer* lexer) {
    while (is_digit(peek(lexer)) || peek(lexer) == '_') {
        advance(lexer);
    }

    bool is_float = false;
    if (peek(lexer) == '.' && is_digit(peek_next(lexer))) {
        is_float = true;

        advance(lexer);

        while (is_digit(peek(lexer)) || peek(lexer) == '_') {
            advance(lexer);
        }
    }

    return create_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INTEGER);
}

static TokenType check_keyword(Lexer* lexer, i32 start, i32 length,
        const char* rest, TokenType type) {
    if (lexer->current - lexer->start == start + length &&
        memcmp(lexer->start + start, rest, length)== 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static TokenType identifier_type(Lexer* lexer) {
    switch (lexer->start[0]) {
        case 'a': return check_keyword(lexer, 1, 2, "nd", TOKEN_AND);
        case 'b': return check_keyword(lexer, 1, 3, "ool", TOKEN_TYPE);
        case 'f': {
            switch (lexer->start[1]) {
                case 'a': return check_keyword(lexer, 2, 3, "lse", TOKEN_FALSE);
                case 'u': return check_keyword(lexer, 2, 2, "nc", TOKEN_FUNCTION);
                case '3': return check_keyword(lexer, 2, 1, "2", TOKEN_TYPE);
                case '6': return check_keyword(lexer, 2, 1, "4", TOKEN_TYPE);
            }
            break;
        }
        case 'i': {
            switch (lexer->start[1]) {
                case '3': return check_keyword(lexer, 2, 1, "2", TOKEN_TYPE);
                case '6': return check_keyword(lexer, 2, 1, "4", TOKEN_TYPE);
            }
            break;
        }
        case 'n': {
            switch (lexer->start[1]) {
                case 'o': return check_keyword(lexer, 2, 1, "t", TOKEN_NOT);
                case 'u': return check_keyword(lexer, 2, 2, "ll", TOKEN_NULL);
            }
            break;
        }
        case 'o': return check_keyword(lexer, 1, 1, "r", TOKEN_OR);
        case 'p': return check_keyword(lexer, 1, 9, "rint_expr", TOKEN_PRINT_EXPR);
        case 's': 
            switch (lexer->start[1]) {
                case 'y': return check_keyword(lexer, 2, 4, "mbol", TOKEN_TYPE);
                case 't': 
                    switch (lexer->start[2]) {
                        case 'r':
                            switch(lexer->start[3]) {
                                case 'u': return check_keyword(lexer, 4, 2, "ct", TOKEN_STRUCT);
                                case 'i': return check_keyword(lexer, 4, 2, "ng", TOKEN_TYPE);
                            }
                            break;
                    }
                    break;
            }
            break;
        case 't': return check_keyword(lexer, 1, 3, "rue", TOKEN_TRUE);
        case 'v': return check_keyword(lexer, 1, 3, "oid", TOKEN_TYPE);
    }

    return TOKEN_IDENTIFIER;
}

static Token identifier(Lexer* lexer) {
    while (is_alpha(peek(lexer)) || is_digit(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, identifier_type(lexer));
}

Token lexer_next_token(Lexer* lexer) {
    skip_whitespace(lexer);

    lexer->start = lexer->current;

    if (is_at_end(lexer)) {
        return create_token(lexer, TOKEN_EOF);
    }

    char c = advance(lexer);

    if (is_alpha(c)) {
        return identifier(lexer);
    }

    if (is_digit(c)) {
        return number(lexer);
    }

    switch (c) {
        case '(': return create_token(lexer, TOKEN_PARENTHESIS_OPEN);
        case ')': return create_token(lexer, TOKEN_PARENTHESIS_CLOSE);
        case '{': return create_token(lexer, TOKEN_BRACE_OPEN);
        case '}': return create_token(lexer, TOKEN_BRACE_CLOSE);
        case '[': return create_token(lexer, TOKEN_BRACKET_OPEN);
        case ']': return create_token(lexer, TOKEN_BRACKET_CLOSE);
        case ',': return create_token(lexer, TOKEN_COMMA);
        case '.': return create_token(lexer, TOKEN_DOT);
        case '-': return create_token(lexer, TOKEN_MINUS);
        case '*': return create_token(lexer, TOKEN_STAR);
        case '/': return create_token(lexer, TOKEN_SLASH);
        case ':': return create_token(lexer, TOKEN_COLIN);
        case '+': return create_token(lexer, match(lexer, '+') ? TOKEN_PLUS_PLUS : TOKEN_PLUS);
        case '=': return create_token(lexer, match(lexer, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '!': return create_token(lexer, match(lexer, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '<': return create_token(lexer, match(lexer, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>': return create_token(lexer, match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        
        case '"': return string(lexer);
        case '\'': return symbol(lexer);
        case '@': return annotation(lexer);
    }

    return error_token(lexer, "Unexpected character.");
}
