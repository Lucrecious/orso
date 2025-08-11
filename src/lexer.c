#include "lexer.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

operator_t operators[TOKEN_SIZE] = {
    [TOKEN_PLUS] = {.type=OPERATOR_TYPE_ARITHMETIC},
    [TOKEN_MINUS] = {.type=OPERATOR_TYPE_ARITHMETIC},
    [TOKEN_SLASH] = {.type=OPERATOR_TYPE_ARITHMETIC},
    [TOKEN_STAR] = {.type=OPERATOR_TYPE_ARITHMETIC},
    [TOKEN_PERCENT] = {.type=OPERATOR_TYPE_ARITHMETIC},
    [TOKEN_PERCENT_PERCENT] = {.type=OPERATOR_TYPE_ARITHMETIC},

    [TOKEN_EQUAL_EQUAL] = {.type=OPERATOR_TYPE_COMPARE},
    [TOKEN_BANG_EQUAL] = {.type=OPERATOR_TYPE_COMPARE},

    [TOKEN_GREATER] = {.type=OPERATOR_TYPE_ORDERING},
    [TOKEN_GREATER_EQUAL] = {.type=OPERATOR_TYPE_ORDERING},
    [TOKEN_LESS] = {.type=OPERATOR_TYPE_ORDERING},
    [TOKEN_LESS_EQUAL] = {.type=OPERATOR_TYPE_ORDERING},

    [TOKEN_AND] = {.type=OPERATOR_TYPE_LOGICAL},
    [TOKEN_OR] = {.type=OPERATOR_TYPE_LOGICAL},
    [TOKEN_NOT] = {.type=OPERATOR_TYPE_LOGICAL},
};

static token_t create_token(lexer_t *lexer, token_type_t type) {
    token_t token = { 
        .source = lexer->source,
        .type = type,
        .view = {.length=(size_t)(lexer->current - lexer->start), .data = lexer->start},
        .loc = texloc(lexer->file_path, (size_t)lexer->line, (size_t)(lexer->start - (lexer->line_start + 1))),
    };

    return token;
}

static token_t error_token(lexer_t *lexer, string_view_t message_view) {
    token_t token = {
        .source = lexer->source,
        .type = TOKEN_ERROR,
        .view = message_view,
        .loc = texloc(lexer->file_path, (size_t)lexer->line, (size_t)(lexer->current - lexer->line_start)),
    };
    
    return token;
}

void lexer_init(lexer_t *lexer, orstring_t file_path_no_copy, string_view_t code) {
    lexer->file_path = file_path_no_copy;
    lexer->source = code;
    lexer->line = 0;
    lexer->start = (char*)code.data;
    lexer->line_start = lexer->start;
    lexer->current = (char*)code.data;

    lexer->previous_token = error_token(lexer, lit2sv("<previous token>"));
}

static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || c == '_';
}

static bool is_at_end(lexer_t *lexer) {
    return *lexer->current == '\0';
}

static char advance(lexer_t *lexer) {
    lexer->current++;
    return lexer->current[-1];
}

static char FORCE_INLINE peek(lexer_t *lexer) {
    return lexer->current[0];
}

static char peek_next(lexer_t *lexer) {
    if (is_at_end(lexer)) {
        return '\0';
    }

    return lexer->current[1];
}

static bool match(lexer_t *lexer, char expected) {
    if (is_at_end(lexer)) {
        return false;
    }

    if (*lexer->current != expected) {
        return false;
    }

    lexer->current++;
    return true;
}

static bool match2(lexer_t *lexer, const char* expected) {
    ASSERT(strlen(expected) == 2, "the size of expected must be 2");

    if (is_at_end(lexer)) {
        return false;
    }

    if (peek(lexer) != expected[0] || peek_next(lexer) != expected[1]) {
        return false;
    }

    lexer->current += 2;
    return true;
}

static void skip_whitespace(lexer_t *lexer) {
    for (;;) {
        char c = peek(lexer);
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
                advance(lexer);
                break;
            case '\n':
                ++lexer->line;
                lexer->line_start = lexer->current;
                advance(lexer);
                break;
            default:
                return;
        }
    }
}

static bool skip_comments(lexer_t *lexer) {
    if (match(lexer, '#')) {
        while (!is_at_end(lexer)) {
            char c = advance(lexer);
            if (c == '\n') {
                ++lexer->line;
                lexer->line_start = lexer->current-1;
                return true;
            }
        }
    } else if (match2(lexer, "#*")) {
        ors32 rings = 1;

        while (!is_at_end(lexer)) {
            if (match2(lexer, "*#")) {
                rings--;
                continue;
            }

            if (rings == 0) {
                return true;
            }

            if (match2(lexer, "#*")) {
                rings++;
                continue;
            }

            char c = advance(lexer);
            if (c == '\n') {
                ++lexer->line;
                lexer->line_start = lexer->current;
            }
        }

        error_token(lexer, lit2sv("Expected closing comment */ before file end"));
    }

    return false;
}

static FORCE_INLINE token_t _string_symbol(lexer_t *lexer, char terminator, token_type_t type) {
    while (peek(lexer) != terminator && !is_at_end(lexer)) {
        if (peek(lexer) == '\n') {
            lexer->line++;
        }
        advance(lexer);
    }

    if (is_at_end(lexer)) {
        return error_token(lexer, lit2sv("Unterminated string."));
    }

    advance(lexer);
    return create_token(lexer, type);
}

static token_t string(lexer_t *lexer) {
    return _string_symbol(lexer, '"', TOKEN_STRING);
}

static token_t symbol(lexer_t *lexer) {
    return _string_symbol(lexer, '\'', TOKEN_SYMBOL);
}

static token_t number(lexer_t *lexer) {
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
    } else if (peek(lexer) == 's' && peek_next(lexer) == 'z') {
        advance(lexer);
        advance(lexer);
    } else if (peek(lexer) == 'p' && peek_next(lexer) == 'd') {
        advance(lexer);
        advance(lexer);
    } else if (peek(lexer) == 'f' || peek(lexer) == 'd') {
        advance(lexer);
    } else if (peek(lexer) == 's' || peek(lexer) == 'u') {
        advance(lexer);

        switch (peek(lexer)) {
        case '8': advance(lexer); break;
        case '1': {
            switch (peek_next(lexer)) {
            case '6': advance(lexer); advance(lexer); break;
            default: break;
            }
            break;
        }

        case '3': {
            switch (peek_next(lexer)) {
            case '2': advance(lexer); advance(lexer); break;
            default: break;
            }
            break;
        }

        case '6': {
            switch (peek_next(lexer)) {
            case '4': advance(lexer); advance(lexer); break;
            default: break;
            }
        }

        default: break;
        }
    }

    return create_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INTEGER);
}

static token_type_t check_keyword(lexer_t *lexer, ors32 start, ors32 length,
        const char* rest, token_type_t type) {
    if (lexer->current - lexer->start == start + length &&
        memcmp(lexer->start + start, rest, (size_t)length)== 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static token_type_t identifier_type(lexer_t *lexer) {
    switch (lexer->start[0]) {
        case 'a':
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'n': return check_keyword(lexer, 2, 1, "d", TOKEN_AND);
                    case 's': return check_keyword(lexer, 2, 0, "", TOKEN_AS);
                }
            }
            break;
        case 'b': return check_keyword(lexer, 1, 4, "reak", TOKEN_BREAK);
        case 'c': return check_keyword(lexer, 1, 7, "ontinue", TOKEN_CONTINUE);
        case 'd': return check_keyword(lexer, 1, 1, "o", TOKEN_DO);
        case 'e':
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'l': return check_keyword(lexer, 2, 2, "se", TOKEN_ELSE);
                    case 'n': return check_keyword(lexer, 2, 2, "um", TOKEN_ENUM);
                }
            }
            break;
        case 'f': {
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'a': return check_keyword(lexer, 2, 3, "lse", TOKEN_FALSE);
                    case 'o': return check_keyword(lexer, 2, 1, "r", TOKEN_FOR);
                }
            }
            break;
        }
        case 'i': return check_keyword(lexer, 1, 1, "f", TOKEN_IF);
        case 'l': return check_keyword(lexer, 1, 2, "en", TOKEN_LEN);
        case 'n': return check_keyword(lexer, 1, 2, "ot", TOKEN_NOT);
        case 'o':
            if (lexer->current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'f': return check_keyword(lexer, 2, 7, "fsetptr", TOKEN_OFFSETPTR);
                    case 'r': return check_keyword(lexer, 2, 0, "", TOKEN_OR);
                }
            }
            break;

        case 'p': return check_keyword(lexer, 1, 6, "trdiff", TOKEN_PTRDIFF);

        case 'r': return check_keyword(lexer, 1, 5, "eturn", TOKEN_RETURN);
        case 's': 
            if (lexer->current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 't': return check_keyword(lexer, 2, 4, "ruct", TOKEN_STRUCT);
                    case 'i': return check_keyword(lexer, 2, 4, "zeof", TOKEN_SIZEOF);
                }
                
            }
            break;
        case 't':
            if (lexer->current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'h': return check_keyword(lexer, 2, 2, "en", TOKEN_THEN);
                    case 'r': return check_keyword(lexer, 2, 2, "ue", TOKEN_TRUE);
                    case 'y': return check_keyword(lexer, 2, 4, "peof", TOKEN_TYPEOF);
                }
            }
            break;
            
        case 'u': {
            if (lexer->current - lexer->start > 2 && lexer->start[1] == 'n') {
                switch (lexer->start[2]) {
                    case 'l': return check_keyword(lexer, 3, 3, "ess", TOKEN_UNLESS);
                    case 't': return check_keyword(lexer, 3, 2, "il", TOKEN_UNTIL);
                }
            }
            break;
        }
        return check_keyword(lexer, 1, 5, "nless", TOKEN_UNLESS);
        case 'w': return check_keyword(lexer, 1, 4, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}

static token_t identifier(lexer_t* lexer) {
    while (is_alpha(peek(lexer)) || is_digit(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, identifier_type(lexer));
}

static bool is_directive_character(char c) {
    if (is_alpha(c)) {
        return true;
    }

    if (is_digit(c)) {
        return true;
    }

    switch (c) {
        case ',':
        case '?':
        case '!':
            return true;
        default:
            return false;
    }

    return false;
}

static token_t directive(lexer_t *lexer) {
    while (is_directive_character(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, TOKEN_DIRECTIVE);
}

token_t _lexer_next_token(lexer_t *lexer) {
    // skip preceeding comments and whitespace
    while (true) {
        skip_whitespace(lexer);

        bool comments_skipped = skip_comments(lexer);

        if (comments_skipped) {
            continue;
        }
        break;
    }

    lexer->start = lexer->current;

    if (is_at_end(lexer)) {
        return create_token(lexer, TOKEN_EOF);
    }

    char c = advance(lexer);

    if (c == '@') {
        return directive(lexer);
    }

    if (is_alpha(c)) {
        token_t t = identifier(lexer);
        return t;
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

        case '-': {
            if (match(lexer, '>')) return create_token(lexer, TOKEN_ARROW_RIGHT);
            if (match(lexer, '-')) return create_token(lexer, TOKEN_MINUS_MINUS);
            if (match(lexer, '=')) return create_token(lexer, TOKEN_MINUS_EQUAL);
            return create_token(lexer, TOKEN_MINUS);
        }

        case '*': return create_token(lexer, match(lexer, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);

        case '%': {
            if (match(lexer,'%')) {
                if (match(lexer, '=')) return create_token(lexer, TOKEN_PERCENT_PERCENT_EQUAL);
                return create_token(lexer, TOKEN_PERCENT_PERCENT);
            } else {
                if (match(lexer, '=')) return create_token(lexer, TOKEN_PERCENT_EQUAL);
                return create_token(lexer, TOKEN_PERCENT);
            }
        }

        case '/': return create_token(lexer, match(lexer, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
        case ':': return create_token(lexer, TOKEN_COLON);
        case ';': return create_token(lexer, TOKEN_SEMICOLON);
        case '|': return create_token(lexer, TOKEN_BAR);
        case '&':
            if (match(lexer, '&')) return create_token(lexer, TOKEN_AMPERSAND_AMPERSAND);
            return create_token(lexer, TOKEN_AMPERSAND);
        case '+': {
            if (match(lexer, '+')) return create_token(lexer, TOKEN_PLUS_PLUS);
            if (match(lexer, '=')) return create_token(lexer, TOKEN_PLUS_EQUAL);
            return create_token(lexer, TOKEN_PLUS);
        }
        case '=': return create_token(lexer, match(lexer, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '!': return create_token(lexer, match(lexer, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '<': return create_token(lexer, match(lexer, '=') ? TOKEN_LESS_EQUAL :
                                            (match(lexer, '<') ? TOKEN_LESS_LESS : TOKEN_LESS));
        case '>': return create_token(lexer, match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '^': return create_token(lexer, TOKEN_HAT);
        
        case '"': return string(lexer);
        case '\'': return symbol(lexer);
        case '~': return create_token(lexer, TOKEN_SQUIGGLE);
    }

    return error_token(lexer, lit2sv("Unexpected character."));
}

token_t lexer_next_token(lexer_t* lexer) {
    lexer->previous_token = _lexer_next_token(lexer);
    return lexer->previous_token;
}
