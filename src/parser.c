#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "common.h"
#include "type.h"
#include "sb.h"

/*
program                  -> declaration* EOF
declaration              -> variable_declaration | statement
variable_declaration     -> IDENTIFIER `:` types? (`=` expression)? `;`
statement                -> expression_statement
expression_statement     -> expression `;`
types                    -> type (`|` type)*

expression               -> assignment
assignment               -> IDENTIFIER `=` expression
                          | equality
equality                 -> comparison ((`!=` | `==`) comparison)*
comparison               -> term ((`<` | `>` | `<=` | `>=`) term)*
term                     -> factor ((`+` | `-`) factor)*
factor                   -> unary ((`/` | `*`) unary)*
unary                    -> (`not`)? unary | primary

primary                  -> `true` | `false` | `null` | IDENTIFIER | INTEGER | DECIMAL
                          | STRING | SYMBOL
*/

typedef struct Parser {
    OrsoErrorFunction error_fn;
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

typedef OrsoExpressionNode* (*ParseFn)(Parser*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

void orso_ast_init(OrsoAST* ast) {
    ast->declarations = NULL;
}

static void orso_free_expression(OrsoExpressionNode* expression) {
    if (!expression) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_BINARY:
            orso_free_expression(expression->binary.left);
            orso_free_expression(expression->binary.right);
            free(expression->binary.left);
            free(expression->binary.right);
            break;
        case EXPRESSION_GROUPING:
            orso_free_expression(expression->grouping.expression);
            free(expression->grouping.expression);
            break;
        case EXPRESSION_IMPLICIT_CAST:
            orso_free_expression(expression->cast.operand);
            free(expression->cast.operand);
            break;
        case EXPRESSION_UNARY:
            orso_free_expression(expression->unary.operand);
            free(expression->unary.operand);
            break;
        case EXPRESSION_ASSIGNMENT:
            orso_free_expression(expression->assignment.right_side);
            free(expression->assignment.right_side);
            break;
        case EXPRESSION_VARIABLE:
            // identifier symbol is freed by gc
        case EXPRESSION_PRIMARY:
            // no need
            break;
    }
}

static void free_declaration(OrsoDeclarationNode* declaration_node) {
    switch (declaration_node->type) {
        case ORSO_DECLARATION_STATEMENT: {
            orso_free_expression(declaration_node->statement->expression);
            free(declaration_node->statement->expression);
            free(declaration_node->statement);
            break;
        }
        case ORSO_DECLARATION_VAR: {
            orso_free_expression(declaration_node->var->expression);
            free(declaration_node->var->expression);
            free(declaration_node->var);
            break;
        }
    }
}

void orso_ast_free(OrsoAST* ast) {
    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        free_declaration(ast->declarations[i]);
        free(ast->declarations[i]);
    }
    sb_free(ast->declarations);
}

static void parser_init(Parser* parser, const char* source, OrsoErrorFunction error_fn) {
    lexer_init(&parser->lexer, source);
    parser->error_fn = error_fn;
    parser->had_error = false;
    parser->panic_mode = false;
}

static void error_at(Parser* parser, Token* token, const char* specific) {
    if (parser->panic_mode) {
        return;
    }
    parser->panic_mode = true;
    parser->had_error = true;

    if (!parser->error_fn) {
        return;
    }

    char message[100];
    char* msg = message;
    msg += sprintf(msg, "Error");

    if (token->type == TOKEN_EOF) {
        msg += sprintf(msg, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing
    } else {
        msg += sprintf(msg, " at '%.*s'", token->length, token->start);
    }

    msg += sprintf(msg, ": %s", specific);

    parser->error_fn(ORSO_ERROR_COMPILE, token->line, message);
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

static FORCE_INLINE bool check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, TokenType type) {
    if (!check(parser, type)) {
        return false;
    }

    advance(parser);
    return true;
}

static void synchronize(Parser* parser) {
    parser->panic_mode = false;

    while (parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) {
            return;
        }

        switch (parser->current.type) {
            case TOKEN_PRINT_EXPR: 
                return;
            default: break;
        }

        advance(parser);
    }
}

static OrsoExpressionNode* expression(Parser* parser);
static ParseRule* get_rule(TokenType type);
static OrsoExpressionNode* parse_precedence(Parser* parser, Precedence precedence);

static OrsoType integer_type(i64 value) {
    if (value >= INT32_MIN && value <= INT32_MAX) {
        return ORSO_TYPE_INT32;
    }
    return ORSO_TYPE_INT64;
}

static OrsoExpressionNode* number(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);
    expression_node->start = expression_node->end = parser->previous;
    expression_node->type = EXPRESSION_PRIMARY;
    expression_node->primary.token = parser->previous;

    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            i64 value = cstrn_to_i64(parser->previous.start, parser->previous.length);
            expression_node->value_type = integer_type(value);
            expression_node->primary.constant = ORSO_SLOT_I(value, expression_node->value_type);
            break;
        }
        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.start, parser->previous.length);
            expression_node->value_type = ORSO_TYPE_FLOAT64;
            expression_node->primary.constant = ORSO_SLOT_F(value, ORSO_TYPE_FLOAT64);
            break;
        }
        default: break; // unreachable
    }

    return expression_node;
}

static OrsoExpressionNode* literal(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);
    expression_node->start = expression_node->end = parser->previous;
    expression_node->type = EXPRESSION_PRIMARY;
    expression_node->primary.token = parser->previous;

    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expression_node->value_type = ORSO_TYPE_BOOL;

            i64 is_true = (i64)(parser->previous.type == TOKEN_TRUE);
            expression_node->primary.constant = ORSO_SLOT_I(is_true, ORSO_TYPE_BOOL);
            break;
        }
        case TOKEN_NULL: {
            expression_node->value_type = ORSO_TYPE_NULL;
            expression_node->primary.constant = ORSO_SLOT_I(0, ORSO_TYPE_NULL);
            break;
        }
        case TOKEN_STRING: {
            expression_node->value_type = ORSO_TYPE_STRING;
            Token string = expression_node->primary.token;
            expression_node->primary.constant = ORSO_SLOT_P(0, ORSO_TYPE_STRING);
            break;
        };

        case TOKEN_SYMBOL: {
            expression_node->value_type = ORSO_TYPE_SYMBOL;
            Token symbol = expression_node->primary.token;
            expression_node->primary.constant = ORSO_SLOT_P(0, ORSO_TYPE_SYMBOL);
            break;
        }
    }

    return expression_node;
}

static OrsoExpressionNode* convert_assignment_expression(Parser* parser, OrsoExpressionNode* left_operand, OrsoExpressionNode* assignment) {
    if (left_operand->type != EXPRESSION_VARIABLE) {
        error_at(parser, &left_operand->start, "Expect variable.");
        orso_free_expression(left_operand);
        orso_free_expression(assignment);
        free(assignment);
        left_operand->type = EXPRESSION_NONE;
        return left_operand;
    }

    assignment->assignment.variable_name = left_operand->variable.name;
    orso_free_expression(left_operand);
    free(left_operand);

    return assignment;
}

static OrsoExpressionNode* assignment(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);
    expression_node->type = EXPRESSION_ASSIGNMENT;
    expression_node->value_type = ORSO_TYPE_UNRESOLVED;
    expression_node->start = parser->previous;
    expression_node->assignment.right_side = expression(parser);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* named_variable(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);
    Token start = parser->previous;
    expression_node->type = EXPRESSION_VARIABLE;

    expression_node->variable.name = parser->previous;
    expression_node->start = start;
    expression_node->end = start;

    expression_node->value_type = ORSO_TYPE_UNRESOLVED;

    return expression_node;
}

static OrsoExpressionNode* variable(Parser* parser) {
    return named_variable(parser);
}

static OrsoExpressionNode* grouping(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);

    expression_node->value_type = ORSO_TYPE_UNRESOLVED;
    expression_node->type = EXPRESSION_GROUPING;
    expression_node->start = parser->previous;
    expression_node->grouping.expression = expression(parser);
    expression_node->value_type = expression_node->grouping.expression->value_type;
    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after expression.");

    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* unary(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);

    expression_node->value_type = ORSO_TYPE_UNRESOLVED;
    expression_node->type = EXPRESSION_UNARY;
    expression_node->unary.operator = parser->previous;
    expression_node->start = parser->previous;
    expression_node->unary.operand = parse_precedence(parser, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* binary(Parser* parser) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);

    expression_node->value_type = ORSO_TYPE_UNRESOLVED;
    expression_node->type = EXPRESSION_BINARY;
    expression_node->binary.operator = parser->previous;
    expression_node->binary.left = NULL;
    expression_node->binary.right = NULL;

    ParseRule* rule = get_rule(expression_node->binary.operator.type);

    expression_node->start = parser->previous;
    expression_node->binary.right = parse_precedence(parser, (Precedence)(rule->precedence + 1));
    expression_node->end = parser->previous;

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
    [TOKEN_COLON]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EQUAL]                   = { NULL,       assignment, PREC_ASSIGNMENT},
    [TOKEN_BANG]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_LESS]                    = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_GREATER]                 = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_SEMICOLON]               = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BAR]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_PLUS_PLUS]               = { NULL,       NULL,       PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EQUAL_EQUAL]             = { NULL,       binary,     PREC_EQUALITY },
    [TOKEN_BANG_EQUAL]              = { NULL,       binary,     PREC_EQUALITY },
    [TOKEN_LESS_EQUAL]              = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL]           = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_IDENTIFIER]              = { variable,   NULL,       PREC_NONE },
    [TOKEN_STRING]                  = { literal,    NULL,       PREC_NONE },
    [TOKEN_SYMBOL]                  = { literal,    NULL,       PREC_NONE },
    [TOKEN_INTEGER]                 = { number,     NULL,       PREC_NONE },
    [TOKEN_FLOAT]                   = { number,     NULL,       PREC_NONE },
    [TOKEN_ANNOTATION]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_STRUCT]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_FUNCTION]                = { NULL,       NULL,       PREC_NONE },
    [TOKEN_NOT]                     = { unary,      NULL,       PREC_NONE },
    [TOKEN_AND]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_OR]                      = { NULL,       NULL,       PREC_NONE },
    [TOKEN_TRUE]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_FALSE]                   = { literal,    NULL,       PREC_NONE },
    [TOKEN_NULL]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_PRINT_EXPR]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EOF]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,       NULL,       PREC_NONE },
};

static OrsoExpressionNode* parse_precedence(Parser* parser, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
    OrsoExpressionNode* left_operand;

    if (prefix_rule == NULL) {
        error(parser, "Expect expression.");
        left_operand = ORSO_ALLOCATE(OrsoExpressionNode);
        left_operand->type = EXPRESSION_NONE;
    } else {
        left_operand = prefix_rule(parser);
    }

    while (precedence <= get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = get_rule(parser->previous.type)->infix;
        OrsoExpressionNode* right_operand = infix_rule(parser);
        switch (right_operand->type) {
            case EXPRESSION_BINARY:
                right_operand->binary.left = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            case EXPRESSION_ASSIGNMENT:
                left_operand = convert_assignment_expression(parser, left_operand, right_operand);
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

static OrsoExpressionNode* expression(Parser* parser) {
    return parse_precedence(parser, PREC_ASSIGNMENT);
}

static OrsoStatementNode* statement(Parser* parser) {
    OrsoStatementNode* statement_node = ORSO_ALLOCATE(OrsoStatementNode);

    statement_node->start = parser->current;

    if (match(parser, TOKEN_PRINT_EXPR)) {
        statement_node->type = ORSO_STATEMENT_PRINT_EXPR;
    } else {
        statement_node->type = ORSO_STATEMENT_EXPRESSION;
    }

    statement_node->expression = expression(parser);

    consume(parser, TOKEN_SEMICOLON, "Expect end of statement semicolin.");

    statement_node->end = parser->previous;

    return statement_node;
}

static Token parse_variable(Parser* parser) {
    Token identifier = parser->current;
    advance(parser);
    return identifier;
}

static OrsoVarDeclarationNode* var_declaration(Parser* parser) {
    OrsoVarDeclarationNode* var_declaration_node = ORSO_ALLOCATE(OrsoVarDeclarationNode);
    var_declaration_node->start = parser->current;

    var_declaration_node->var_type = ORSO_TYPE_UNRESOLVED;

    Token type_token = { .type = TOKEN_ERROR, .length = 0 };
    var_declaration_node->type_identifier = type_token;
    var_declaration_node->expression = NULL;

    var_declaration_node->variable_name = parse_variable(parser);

    consume(parser, TOKEN_COLON, "Expect explicit type.");
    if (match(parser, TOKEN_IDENTIFIER)) {
        var_declaration_node->type_identifier = parser->previous;
        if (match(parser, TOKEN_EQUAL)) {
            var_declaration_node->expression = expression(parser);
        }
    } else {
        consume(parser, TOKEN_EQUAL, "Expect define assignment.");
        var_declaration_node->expression = expression(parser);
    }

    consume(parser, TOKEN_SEMICOLON, "Expect end of declaration semicolin.");

    var_declaration_node->end = parser->previous;
    return var_declaration_node;
}

static bool variable_declaration_look_ahead(Parser* parser) {
    if (parser->current.type != TOKEN_IDENTIFIER) {
        return false;
    }

    Lexer lookahead_lexer = parser->lexer;

    Token token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static OrsoDeclarationNode* declaration(Parser* parser) {
    Token start = parser->current;

    OrsoDeclarationNode* declaration_node = ORSO_ALLOCATE(OrsoDeclarationNode);
    declaration_node->type = ORSO_DECLARATION_NONE;
    if (variable_declaration_look_ahead(parser)) {
        declaration_node->type = ORSO_DECLARATION_VAR;
        declaration_node->var = var_declaration(parser);
    } else {
        declaration_node->type = ORSO_DECLARATION_STATEMENT;
        declaration_node->statement = statement(parser);
    }

    declaration_node->start = start;
    declaration_node->end = parser->previous;

    if (parser->panic_mode) {
        synchronize(parser);
    }

    return declaration_node;
}

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn) {
    Parser parser;
    parser_init(&parser, source, error_fn);

    advance(&parser);

    while (!match(&parser, TOKEN_EOF)) {
        OrsoDeclarationNode* declaration_node = declaration(&parser);
        sb_push(ast->declarations, declaration_node);
    }

    consume(&parser, TOKEN_EOF, "Expect end of expression.");

    return !parser.had_error;
}

void ast_print_expression(OrsoExpressionNode* expression, i32 initial) {
    printf("%*s [%s] ", initial, "", orso_type_to_cstr(expression->value_type));

    switch (expression->type) {
        case EXPRESSION_BINARY: {
            printf("BINARY %.*s\n", expression->binary.operator.length, expression->binary.operator.start);

            ast_print_expression(expression->binary.left, initial + 1);
            ast_print_expression(expression->binary.right, initial + 1);
            break;
        }
        case EXPRESSION_GROUPING: {
            printf("GROUPING\n");

            ast_print_expression(expression->grouping.expression, initial + 1);
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            printf("IMPLICIT CAST \n");

            ast_print_expression(expression->cast.operand, initial + 1);
            break;
        }
        case EXPRESSION_UNARY: {
            printf("UNARY %.*s\n", expression->unary.operator.length, expression->unary.operator.start);

            ast_print_expression(expression->unary.operand, initial + 1);
            break;
        }
        case EXPRESSION_PRIMARY: {
            printf("PRIMARY ");
            orso_print_slot(expression->primary.constant, expression->value_type);
            printf("\n");
            break;
        }
        case EXPRESSION_VARIABLE: {
            printf("VARIABLE - %.*s\n", expression->variable.name.length, expression->variable.name.start);
            break;
        }
        case EXPRESSION_ASSIGNMENT: {
            printf("ASSIGNMENT - %.*s\n", expression->assignment.variable_name.length, expression->assignment.variable_name.start);
            ast_print_expression(expression->assignment.right_side, initial + 1);
            break;
        }
    }
}

void ast_print_statement(OrsoStatementNode* statement, i32 indent) {
    printf("%*s", indent, "");
    printf("STATEMENT - ");
    switch(statement->type) {
        case ORSO_STATEMENT_NONE: return;
        case ORSO_STATEMENT_PRINT_EXPR:
            printf("print_expr\n");
            ast_print_expression(statement->expression, indent + 1);
            break;
        case ORSO_STATEMENT_EXPRESSION:
            printf("expression\n");
            ast_print_expression(statement->expression, indent + 1);
            break;
    }
}

void ast_print_var_declaration(OrsoVarDeclarationNode* var_declaration_node, i32 indent) {
    printf("%*s", indent, "");
    printf("VAR DECLARATION - identifier: %.*s, type: ", var_declaration_node->variable_name.length, var_declaration_node->variable_name.start);
    printf(orso_type_to_cstr(var_declaration_node->var_type));
    printf("\n");

    if (var_declaration_node->expression != NULL) {
        ast_print_expression(var_declaration_node->expression, indent + 1);
    }
}

void ast_print_declaration(OrsoDeclarationNode* declaration, i32 initial_indent) {
    if (declaration->type == ORSO_DECLARATION_NONE) {
        return;
    }

    Token start = declaration->start;
    Token end = declaration->end;
    printf("DECLARATION %*s %.*s\n", initial_indent, "", (end.start + end.length) - start.start, start.start);

    switch (declaration->type) {
        case ORSO_DECLARATION_NONE: return;
        case ORSO_DECLARATION_STATEMENT: {
            ast_print_statement(declaration->statement, initial_indent + 1);
            break;
        }
        case ORSO_DECLARATION_VAR: {
            ast_print_var_declaration(declaration->var, initial_indent + 1);
            break;
        }
    }
}

void orso_ast_print(OrsoAST* ast, const char* name) {
    printf("=== %s ===\n", name);
    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        ast_print_declaration(ast->declarations[i], 0);
    }
}