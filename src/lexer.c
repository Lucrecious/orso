#include "lexer.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static token_t create_token(lexer_t *lexer, token_type_t type) {
    token_t token = { 
        .file_path = lexer->file_path,
        .type = type,
        .start = lexer->start,
        .length = lexer->current - lexer->start,
        .start_location = texloc(lexer->line, lexer->current - lexer->line_start),
    };

    return token;
}

static token_t error_token(lexer_t *lexer, cstr_t message) {
    token_t token = {
        .file_path = lexer->file_path,
        .type = TOKEN_ERROR,
        .start = (char*)message,
        .length = strlen(message),
        .start_location = texloc(lexer->line, lexer->current - lexer->line_start),
    };
    
    return token;
}

void lexer_init(lexer_t *lexer, string_t file_path, cstr_t code) {
    lexer->file_path = file_path;
    lexer->line = 0;
    lexer->start = (char*)code;
    lexer->line_start = lexer->start;
    lexer->current = (char*)code;

    lexer->previous_token = error_token(lexer, "<previous token>");
}

static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || c == '_';
}

static bool is_at_end(lexer_t* lexer) {
    return *lexer->current == '\0';
}

static char advance(lexer_t* lexer) {
    lexer->current++;
    return lexer->current[-1];
}

static char FORCE_INLINE peek(lexer_t* lexer) {
    return lexer->current[0];
}

static char peek_next(lexer_t* lexer) {
    if (is_at_end(lexer)) {
        return '\0';
    }

    return lexer->current[1];
}

static bool match(lexer_t* lexer, char expected) {
    if (is_at_end(lexer)) {
        return false;
    }

    if (*lexer->current != expected) {
        return false;
    }

    lexer->current++;
    return true;
}

static bool match2(lexer_t* lexer, const char* expected) {
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

static void skip_whitespace(lexer_t* lexer) {
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
                lexer->line_start = lexer->current;
                advance(lexer);
                break;
            default:
                return;
        }
    }
}

static bool skip_comments(lexer_t* lexer) {
    if (match2(lexer, "//")) {
        while (!is_at_end(lexer)) {
            char c = advance(lexer);
            if (c == '\n') {
                lexer->line++;
                return true;
            }
        }
    } else if (match2(lexer, "/*")) {
        i32 rings = 1;

        while (!is_at_end(lexer)) {
            if (match2(lexer, "*/")) {
                rings--;
                continue;
            }

            if (rings == 0) {
                return true;
            }

            if (match2(lexer, "/*")) {
                rings++;
                continue;
            }

            char c = advance(lexer);
            lexer->line += (c == '\n');
        }

        error_token(lexer, "Expected closing comment */ before file end");
    }

    return false;
}

static FORCE_INLINE token_t _string_symbol(lexer_t* lexer, char terminator, token_type_t type) {
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

static token_t string(lexer_t* lexer) {
    return _string_symbol(lexer, '"', TOKEN_STRING);
}

static token_t symbol(lexer_t* lexer) {
    return _string_symbol(lexer, '\'', TOKEN_SYMBOL);
}

static token_t annotation(lexer_t* lexer) {
    while (is_alpha(peek(lexer)) || is_digit(peek(lexer))) {
        advance(lexer);
    }

    return create_token(lexer, TOKEN_ANNOTATION);
}

static token_t number(lexer_t* lexer) {
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

static token_type_t check_keyword(lexer_t* lexer, i32 start, i32 length,
        const char* rest, token_type_t type) {
    if (lexer->current - lexer->start == start + length &&
        memcmp(lexer->start + start, rest, length)== 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static token_type_t identifier_type(lexer_t* lexer) {
    switch (lexer->start[0]) {
        case 'a': return check_keyword(lexer, 1, 2, "nd", TOKEN_AND);
        case 'd': return check_keyword(lexer, 1, 1, "o", TOKEN_DO);
        case 'e': return check_keyword(lexer, 1, 3, "lse", TOKEN_ELSE);
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
        case 'n': {
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'o': return check_keyword(lexer, 2, 1, "t", TOKEN_NOT);
                    case 'u': return check_keyword(lexer, 2, 2, "ll", TOKEN_NULL);
                }
            }
            break;
        }
        case 'o': return check_keyword(lexer, 1, 1, "r", TOKEN_OR);
        case 'p':
            if (memcmp(lexer->start + 1, "rint", 4) == 0) {
                if (lexer->current - lexer->start == 5) {
                    return TOKEN_PRINT;
                }

                return check_keyword(lexer, 1, 9, "rint_expr", TOKEN_PRINT_EXPR);
            }
            break;
        case 'r': return check_keyword(lexer, 1, 5, "eturn", TOKEN_RETURN);
        case 's': return check_keyword(lexer, 1, 5, "truct", TOKEN_STRUCT);
        case 't':
            if (lexer->current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'h': return check_keyword(lexer, 2, 2, "en", TOKEN_THEN);
                    case 'r': return check_keyword(lexer, 2, 2, "ue", TOKEN_TRUE);
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

static token_t directive(lexer_t* lexer) {
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

    // if (lexer->previous_token.line != lexer->line || is_at_end(lexer)) {
    //     switch (lexer->previous_token.type) {
    //         case TOKEN_IDENTIFIER:
    //         case TOKEN_STRING:
    //         case TOKEN_DIRECTIVE:
    //         case TOKEN_ANNOTATION:
    //         case TOKEN_FALSE:
    //         case TOKEN_TRUE:
    //         case TOKEN_SYMBOL:
    //         case TOKEN_RETURN:
    //         case TOKEN_NULL:
    //         case TOKEN_BRACE_CLOSE:
    //         case TOKEN_BRACKET_CLOSE:
    //         case TOKEN_PARENTHESIS_CLOSE:
    //         case TOKEN_INTEGER:
    //         case TOKEN_FLOAT:
    //             return  (token_t) {
    //                 .length = 0,
    //                 .line = lexer->line,
    //                 .start = lexer->start,
    //                 .type = TOKEN_SEMICOLON,
    //             };
    //         default: break;
    //     }
    // }

    if (is_at_end(lexer)) {
        return create_token(lexer, TOKEN_EOF);
    }

    char c = advance(lexer);

    if (c == '#') {
        return directive(lexer);
    }

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
        case '-': return create_token(lexer, match(lexer, '>') ? TOKEN_ARROW_RIGHT : TOKEN_MINUS);
        case '*': return create_token(lexer, TOKEN_STAR);
        case '/': return create_token(lexer, TOKEN_SLASH);
        case ':': return create_token(lexer, TOKEN_COLON);
        case ';': return create_token(lexer, TOKEN_SEMICOLON);
        case '|': return create_token(lexer, TOKEN_BAR);
        case '&': return create_token(lexer, TOKEN_AMPERSAND);
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

token_t lexer_next_token(lexer_t* lexer) {
    lexer->previous_token = _lexer_next_token(lexer);
    return lexer->previous_token;
}
