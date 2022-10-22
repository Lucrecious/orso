#include "abstract_syntax_tree.h"

#include <stdio.h>
#include <stdlib.h>

#include "str_to_value.h"

typedef struct Parser {
    Lexer lexer;
    Token previous;
    Token current;
    bool had_error;
    bool panic_mode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // -, not
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef SavineExpressionNode* (*ParseFn)(Parser*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void parser_init(Parser* parser, const char* source) {
    lexer_init(&parser->lexer, source);
    parser->had_error = false;
    parser->panic_mode = false;
}

static void error_at(Parser* parser, Token* token, const char* message) {
    if (parser->panic_mode) {
        return;
    }
    parser->panic_mode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser->had_error = true;
}

static void error_at_current(Parser* parser, const char* message) {
    error_at(parser, &parser->current, message);
}

static void error(Parser* parser, const char* message) {
    error_at(parser, &parser->previous, message);
}

static void advance(Parser* parser) {
    parser->previous = parser->current;

    for (;;) {
        parser->current = lexer_next_token(&parser->lexer);
        if (parser->current.type != TOKEN_ERROR) {
            break;
        }

        error_at_current(parser, parser->current.start);
    }
}

static void consume(Parser* parser, TokenType type, const char* message) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }

    error_at_current(parser, message);
}

static SavineExpressionNode* number(Parser* parser) {
    i32 value = cstr_to_i32(parser->previous.start, parser->previous.length);

    SavineExpressionNode* expression_node = ALLOCATE(SavineExpressionNode);
    expression_node->type = EXPRESSION_PRIMARY;
    expression_node->primary.token = parser->previous;
    expression_node->primary.value = value;

    return expression_node;
}

static SavineExpressionNode* expression(Parser* parser);
static ParseRule* get_rule(TokenType type);
static SavineExpressionNode* parse_precedence(Parser* parser, Precedence precedence);

static SavineExpressionNode* grouping(Parser* parser) {
    SavineExpressionNode* expression_node = ALLOCATE(SavineExpressionNode);
    expression_node->type = EXPRESSION_GROUPING;
    expression_node->grouping.expression = expression(parser);
    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after expression.");

    return expression_node;
}

static SavineExpressionNode* unary(Parser* parser) {
    SavineExpressionNode* expression_node = ALLOCATE(SavineExpressionNode);

    expression_node->type = EXPRESSION_UNARY;
    expression_node->unary.operator = parser->previous;
    expression_node->unary.operand = parse_precedence(parser, PREC_UNARY);

    return expression_node;
}

static SavineExpressionNode* binary(Parser* parser) {
    SavineExpressionNode* expression_node = ALLOCATE(SavineExpressionNode);
    expression_node->type = EXPRESSION_BINARY;
    expression_node->binary.operator = parser->previous;
    expression_node->binary.left = NULL;
    expression_node->binary.right = NULL;

    ParseRule* rule = get_rule(expression_node->binary.operator.type);
    expression_node->binary.right = parse_precedence(parser, (Precedence)(rule->precedence + 1));

    return expression_node;
}

ParseRule rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { grouping,   NULL,       PREC_NONE },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACE_CLOSE]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACKET_OPEN]            = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACKET_CLOSE]           = { NULL,       NULL,       PREC_NONE },
    [TOKEN_COMMA]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_DOT]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_MINUS]                   = { unary,      binary,     PREC_TERM },
    [TOKEN_PLUS]                    = { NULL,       binary,     PREC_TERM },
    [TOKEN_STAR]                    = { NULL,       binary,     PREC_FACTOR },
    [TOKEN_SLASH]                   = { NULL,       binary,     PREC_FACTOR },
    [TOKEN_COLIN]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EQUAL]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BANG]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_LESS]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_GREATER]                 = { NULL,       NULL,       PREC_NONE },
    [TOKEN_PLUS_PLUS]               = { NULL,       NULL,       PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EQUAL_EQUAL]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BANG_EQUAL]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_LESS_EQUAL]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_GREATER_EQUAL]           = { NULL,       NULL,       PREC_NONE },
    [TOKEN_IDENTIFIER]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_STRING]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_INTEGER]                 = { number,     NULL,       PREC_NONE },
    [TOKEN_DECIMAL]                 = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ANNOTATION]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_STRUCT]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_VAR]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_FUNCTION]                = { NULL,       NULL,       PREC_NONE },
    [TOKEN_NOT]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_AND]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_OR]                      = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EOF]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,       NULL,       PREC_NONE },
};

static SavineExpressionNode* parse_precedence(Parser* parser, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
    SavineExpressionNode* left_operand;

    if (prefix_rule == NULL) {
        error(parser, "Expect expression.");
        left_operand = ALLOCATE(SavineExpressionNode);
        left_operand->type = EXPRESSION_NONE;
    } else {
        left_operand = prefix_rule(parser);
    }

    while (precedence <= get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = get_rule(parser->previous.type)->infix;
        SavineExpressionNode* right_operand = infix_rule(parser);
        switch (right_operand->type) {
            case EXPRESSION_BINARY:
                right_operand->binary.left = left_operand;
                left_operand = right_operand;
                break;
            default: // unreachable
                break;
        }
    }

    return left_operand;
}

static ParseRule* get_rule(TokenType type) {
    return &rules[type];
}

static SavineExpressionNode* expression(Parser* parser) {
    return parse_precedence(parser, PREC_ASSIGNMENT);
}

bool savine_parse_to_ast(const char* source, SavineAST* ast) {
    Parser parser;
    parser_init(&parser, source);

    advance(&parser);

    ast->expression = expression(&parser);

    consume(&parser, TOKEN_EOF, "Expect end of expression.");

    return !parser.had_error;
}

void ast_print_expression(SavineExpressionNode* expression, i32 initial_indent) {
    printf("%*s", initial_indent, "");

    switch (expression->type) {
        case EXPRESSION_BINARY: {
            printf("BINARY %.*s\n", expression->binary.operator.length, expression->binary.operator.start);

            ast_print_expression(expression->binary.left, initial_indent + 1);
            ast_print_expression(expression->binary.right, initial_indent + 1);
            break;
        }
        case EXPRESSION_GROUPING: {
            printf("GROUPING\n");

            ast_print_expression(expression->grouping.expression, initial_indent + 1);
            break;
        }
        case EXPRESSION_UNARY: {
            printf("UNARY %.*s\n", expression->unary.operator.length, expression->unary.operator.start);

            ast_print_expression(expression->unary.operand, initial_indent + 1);
            break;
        }
        case EXPRESSION_PRIMARY: {
            printf("PRIMARY %d\n", expression->primary.value);
            break;
        }
    }
}

void savine_ast_print(SavineAST* ast, const char* name) {
    printf("=== %s ===\n", name);
    ast_print_expression(ast->expression, 0);
}

void savine_ast_init(SavineAST* ast) {
    ast->expression = NULL;
}

static void savine_free_expression(SavineExpressionNode* expression) {
    if (!expression) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_BINARY:
            savine_free_expression(expression->binary.left);
            savine_free_expression(expression->binary.right);
            free(expression->binary.left);
            free(expression->binary.right);
            break;
        case EXPRESSION_GROUPING:
            savine_free_expression(expression->grouping.expression);
            free(expression->grouping.expression);
            break;
        case EXPRESSION_UNARY:
            savine_free_expression(expression->unary.operand);
            free(expression->unary.operand);
            break;
        case EXPRESSION_PRIMARY:
            // no need
            break;
    }
}

void savine_ast_free(SavineAST* ast) {
    savine_free_expression(ast->expression);
    free(ast->expression);
}