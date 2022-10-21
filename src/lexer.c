#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>

void lexer_lexer_init(Lexer* lexer, char* code) {
    lexer->line = 0;
    lexer->start = code;
    lexer->current = code;
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
        .start = message,
        .length = (i32)(strlen(message)),
        .line = lexer->line,
    };
    
    return token;
}

static char advance(Lexer* lexer) {
    lexer->current++;
    return lexer->current[-1];
}

static char FORCE_INLINE peak(Lexer* lexer) {
    return lexer->current[0];
}

static char peak_next(Lexer* lexer) {
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

static bool skip_whitespace(Lexer* lexer) {
    for (;;) {
        char c = peak(lexer);
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

static Token string(Lexer* lexer) {
    while (peek(lexer) != '"' && !is_at_end(lexer)) {
        if (peek(lexer) == '\n') {
            lexer->line++;
        }
        advance(lexer);
    }

    if (is_at_end(lexer)) {
        return error_token(lexer, "Unterminated string.");
    }

    advance(lexer);
    return create_token(lexer, TOKEN_STRING);
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

    bool is_decimal = false;
    if (peek(lexer) == '.' && is_digit(peek_next(lexer))) {
        is_decimal = true;

        advance(lexer);

        while (is_digit(peek(lexer)) || peek(lexer) == '_') {
            advance(lexer);
        }
    }

    return create_token(lexer, is_decimal ? TOKEN_DECIMAL : TOKEN_INTEGER);
}

static TokenType check_keyword(Lexer* lexer, i32 start, i32 length,
        const char* rest, TokenType type) {
    if (lexer->current - lexer->start == start + length &&
        memcpy(lexer->start + start, rest, length)== 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static TokenType identifier_type(Lexer* lexer) {
    switch (lexer->start[0]) {
        case 's': return check_keyword(lexer, 1, 5, "truct", TOKEN_STRUCT);
        case 'v': return check_keyword(lexer, 1, 2, "ar", TOKEN_VAR);
        case 'f': return check_keyword(lexer, 1, 3, "unc", TOKEN_FUNCTION);
    }

    return TOKEN_IDENTIFIER;
}

static Token identifier(Lexer* lexer) {
    while (is_alpha(peek(lexer)) || is_digit(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, idenifier_type(lexer));
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
        case '+':
            return create_token(lexer, match(lexer, '+') ? TOKEN_PLUS_PLUS : TOKEN_PLUS);
        case '=':
            return create_token(lexer, match(lexer, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '!':
            return create_token(lexer, match(lexer, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '<':
            return create_token(lexer, match(lexer, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            return create_token(lexer, match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        
        case '"':
            return string(lexer);
        case '@':
            return annotation(lexer);
    }

    return error_token(lexer, "Unexpected character.");
}
