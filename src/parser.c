#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "common.h"
#include "type.h"
#include "type_set.h"
#include "sb.h"

/*
program                  -> declaration* EOF
declaration              -> entity_declaration | function_declaration | statement
entity_declaration       -> IDENTIFIER `:` union_type? (`=` expression)? `;`
parameters               -> parameter (`,` parameter)*
parameter                -> IDENTIFIER `:` ((union_type (`=` expression)?) | (`=` expression))
statement                -> expression_statement
expression_statement     -> expression `;`

union_type               -> type (`|` type)*
function_type            -> `(` (union_type (`,` union_type)*)? `)` `->` union_type
type                     -> IDENTIFIER | function_type

directive                -> `#` ~(\s)*
expression               -> directive? (assignment | block | ifthen)
block                    -> `{` declaration* `}`
ifthen                   -> (`if` | `unless` | `while` | `until` ) expression block (`else` (ifthen | block))?
assignment               -> (call `.`)? IDENTIFIER `=` expression
                          | logic_or
logic_or                 -> logic_and (`or` logic_and)*
logic_and                -> equality (`and` equality)*
equality                 -> comparison ((`!=` | `==`) comparison)*
comparison               -> term ((`<` | `>` | `<=` | `>=`) term)*
term                     -> factor ((`+` | `-`) factor)*
factor                   -> unary ((`/` | `*`) unary)*
unary                    -> (`not` | `-`) unary | call
call                     -> primary ( `(` arguments? `)` ) | `.` IDENTIFIER )*
arguments                -> argument (`,` argument)*
argument                 -> (IDENTIFIER `=`)? expression

primary                  -> `true` | `false` | `null` | IDENTIFIER | INTEGER | DECIMAL
                          | STRING | SYMBOL | `(` expression `)` | function_definition
function_definition      -> `(` parameters? `)` (`->` union_type)? block
*/

typedef struct Parser {
    OrsoErrorFunction error_fn;
    OrsoAST* ast; // TODO: Consider moving this outside parser and instead passed through arguments
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
    PREC_BITWISE_OR,  // | and type separator
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // - not
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef OrsoASTNode* (*ParseFn)(Parser*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

void orso_ast_init(OrsoAST* ast, OrsoSymbolTable* symbols) {
    ASSERT(symbols, "cannot be null");
    ast->resolved = false;
    ast->root = NULL;
    ast->nodes = NULL;
    ast->folded_constants = NULL;
    ast->symbols = symbols;
    ast->function_definition_pairs = NULL;
    orso_type_set_init(&ast->type_set);

    orso_symbol_table_init(&ast->builtins);
}

void orso_ast_free(OrsoAST* ast) {
    for (i32 i = 0; i < sb_count(ast->nodes); i++) {
        free(ast->nodes[i]);
        ast->nodes[i] = NULL;
    }

    orso_symbol_table_free(&ast->builtins);
    orso_type_set_free(&ast->type_set);
}

OrsoASTNode* orso_ast_node_new(OrsoAST* ast, OrsoASTNodeType node_type, Token start) {
    OrsoASTNode* node = ORSO_ALLOCATE(OrsoASTNode);
    
    sb_push(ast->nodes, node);

    node->node_type = node_type;
    node->start = start;
    node->end = start;
    node->operator = start;
    node->return_guarentee = ORSO_NO_RETURN_GUARENTEED;
    node->type = &OrsoTypeUnresolved;
    node->narrowed_type = &OrsoTypeUnresolved;

    node->value_index = -1;

    switch (node_type) {
        case ORSO_AST_NODE_TYPE_DECLARATION:
            node->data.declaration.initial_value_expression = NULL;
            node->data.declaration.fold_level_resolved_at = -1;
            node->data.declaration.identifier = NULL;
            node->data.declaration.is_mutable = false;
            node->data.declaration.type_expression = NULL;
            break;

        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY:
            node->data.binary.lhs = NULL;
            node->data.binary.rhs = NULL;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK:
            node->data.block = NULL;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING:
            node->data.branch.condition = NULL;
            node->data.branch.condition_negated = false;
            node->data.branch.then_expression = NULL;
            node->data.branch.else_expression = NULL;
            node->data.branch.looping = false;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL:
            node->data.call.callee = NULL;
            node->data.call.arguments = NULL;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR:
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING:
            node->data.expression = NULL;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
            node->data.function_definition.block = NULL;
            node->data.function_definition.compilable = false;
            node->data.function_definition.parameters = NULL;
            node->data.function_definition.return_type_expression = NULL;
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY:
            break;
    }

    return node;
}

static void parser_init(Parser* parser, OrsoAST* ast, const char* source, OrsoErrorFunction error_fn) {
    lexer_init(&parser->lexer, source);
    parser->ast = ast;
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

    const i32 MESSAGE_SIZE = 100;
    char message[MESSAGE_SIZE];
    char* msg = message;
    i32 n = 0;
    n += snprintf(msg, MESSAGE_SIZE, "Error");

    if (token->type == TOKEN_EOF) {
        n += snprintf(msg + n, MESSAGE_SIZE - n, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing
    } else {
        n += snprintf(msg + n, MESSAGE_SIZE - n, " at '%.*s'", token->length, token->start);
    }

    n += snprintf(msg + n, MESSAGE_SIZE - n, ": %s", specific);

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

    while (!check(parser, TOKEN_EOF)) {
        if (parser->previous.type == TOKEN_SEMICOLON) {
            return;
        }

        switch (parser->current.type) {
            case TOKEN_PRINT_EXPR: 
            case TOKEN_PRINT:
                return;
            default: break;
        }

        advance(parser);
    }
}

static OrsoASTNode* declaration(Parser* parser);
static OrsoASTNode* expression(Parser* parser);
static ParseRule* get_rule(TokenType type);
static OrsoASTNode* parse_precedence(Parser* parser, Precedence precedence);

static OrsoType* value_to_integer_type(i64 value) {
    if (value >= INT32_MIN && value <= INT32_MAX) {
        return &OrsoTypeInteger32;
    }
    return &OrsoTypeInteger64;
}

static i32 add_constant_value(Parser* parser, OrsoSlot value) {
    i32 index = sb_count(parser->ast->folded_constants);
    sb_push(parser->ast->folded_constants, value);
    return index;
}

bool orso_ast_node_type_is_decl_or_stmt(OrsoASTNodeType node_type) {
    switch (node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING:
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY:
            return false;
        
        case ORSO_AST_NODE_TYPE_DECLARATION:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
            return true;
    }
}

bool orso_ast_node_type_is_expression(OrsoASTNodeType node_type) {
    switch (node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING:
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY:
            return true;
        
        case ORSO_AST_NODE_TYPE_DECLARATION:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT:
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
            return false;
    }
}

static OrsoASTNode* number(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY, parser->previous);

    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            i64 value = cstrn_to_i64(parser->previous.start, parser->previous.length);
            expression_node->type = value_to_integer_type(value);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(value, expression_node->type));
            break;
        }
        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.start, parser->previous.length);
            expression_node->type = &OrsoTypeFloat64;
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_F(value, &OrsoTypeFloat64));
            break;
        }
        default: UNREACHABLE();
    }

    expression_node->narrowed_type = expression_node->type;

    expression_node->foldable = true;

    return expression_node;
}

static OrsoExpressionNode* literal(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY, parser->previous);

    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expression_node->type = &OrsoTypeBool;

            i64 is_true = (i64)(parser->previous.type == TOKEN_TRUE);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(is_true, &OrsoTypeBool));
            break;
        }
        case TOKEN_NULL: {
            expression_node->type = &OrsoTypeVoid;
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(0, &OrsoTypeVoid));
            break;
        }
        case TOKEN_STRING: {
            expression_node->type = &OrsoTypeString;
            OrsoString* value = orso_new_string_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_P(value, &OrsoTypeString));
            break;
        };

        case TOKEN_SYMBOL: {
            expression_node->type = &OrsoTypeSymbol;
            OrsoSymbol* value = orso_new_symbol_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2, parser->ast->symbols);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_P(value, &OrsoTypeSymbol));
            break;
        }
        default:
            UNREACHABLE();
    }

    expression_node->narrowed_type = expression_node->type;

    expression_node->foldable = true;

    return expression_node;
}

static OrsoExpressionNode* convert_assignment_expression(Parser* parser, OrsoASTNode* left_operand, OrsoASTNode* assignment) {
    if (left_operand->node_type != ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY) {
        error_at(parser, &left_operand->start, "Expect entity name.");
        free(left_operand);
        free(assignment);
        left_operand->node_type = ORSO_AST_NODE_TYPE_UNDEFINED;
        return left_operand;
    }

    assignment->start = left_operand->start;
    assignment->data.binary.lhs = left_operand;

    return assignment;
}

static OrsoExpressionNode* convert_call_expression(Parser* parser, OrsoASTNode* left_operand, OrsoASTNode* call) {
    if (left_operand->node_type != ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY) {
        error_at(parser, &left_operand->start, "Expect function name.");
        free(left_operand);
        free(call);
        left_operand->node_type = ORSO_AST_NODE_TYPE_UNDEFINED;
        return left_operand;
    }

    call->start = left_operand->start;

    call->data.call.callee = left_operand;

    return call;
}

static OrsoASTNode* assignment(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT, parser->previous);

    expression_node->data.binary.rhs = expression(parser);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoASTNode* named_variable(Parser* parser) {
    OrsoExpressionNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY, parser->previous);
    return expression_node;
}

static OrsoExpressionNode* variable(Parser* parser) {
    return named_variable(parser);
}

static void parse_block(Parser* parser, OrsoASTNode* block) {
    block->return_guarentee = ORSO_NO_RETURN_GUARENTEED;
    block->data.block = NULL;

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        OrsoASTNode* declaration_node = declaration(parser);
        sb_push(block->data.block, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, "Expect '}' after block.");
}

static OrsoExpressionNode* block(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK, parser->previous);

    parse_block(parser, expression_node);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoASTNode* ifelse(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING, parser->previous);

    expression_node->data.branch.condition_negated = false;
    if (parser->previous.type == TOKEN_UNLESS || parser->previous.type == TOKEN_UNTIL) {
        expression_node->data.branch.condition_negated = true;
    }

    expression_node->data.branch.looping = false;
    if (parser->previous.type == TOKEN_WHILE || parser->previous.type == TOKEN_UNTIL) {
        expression_node->data.branch.looping = true;
    }

    expression_node->return_guarentee = ORSO_NO_RETURN_GUARENTEED;

    expression_node->data.branch.condition = expression(parser);

    consume(parser, TOKEN_BRACE_OPEN, "Expect '{' after condition.");

    expression_node->data.branch.then_expression = block(parser);

    if (!match(parser, TOKEN_ELSE)) {
        expression_node->data.branch.else_expression = NULL;
        return expression_node;
    }

    if (match(parser, TOKEN_IF) || match(parser, TOKEN_UNLESS) ||
        match(parser, TOKEN_WHILE) || match(parser, TOKEN_UNTIL)) {
        expression_node->data.branch.else_expression = ifelse(parser);
        return expression_node;
    }

    consume(parser, TOKEN_BRACE_OPEN, "Expect '{' after else.");

    expression_node->data.branch.else_expression = block(parser);

    expression_node->end = parser->previous;

    return expression_node;
}

static bool is_incoming_expression_function_definition(Parser* parser) {
    ASSERT(parser->previous.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    Lexer look_ahead_lexer = parser->lexer;
    i32 parenthesis_level = 1;
    Token next = parser->current;
    // get matching close parenthesis
    while (true) {
        if (next.type == TOKEN_EOF) {
            error(parser, "File ended before closing the parenthesis.");
            return false;
        }

        if (next.type == TOKEN_PARENTHESIS_CLOSE) {
            parenthesis_level--;
        } else if (next.type == TOKEN_PARENTHESIS_OPEN) {
            parenthesis_level++;
        }

        if (parenthesis_level <= 0) {
            break;
        }

        next = lexer_next_token(&look_ahead_lexer);
    }

    next = lexer_next_token(&look_ahead_lexer);
    
    return next.type == TOKEN_ARROW_RIGHT || next.type == TOKEN_BRACE_OPEN;
}

static OrsoASTNode* entity_declaration(Parser* parser, bool as_parameter);

static OrsoASTNode** parse_parameters(Parser* parser) {
    if (!check(parser, TOKEN_IDENTIFIER)) {
        return NULL;
    }

    OrsoASTNode** parameters = NULL;
    while (check(parser, TOKEN_IDENTIFIER)) {
        sb_push(parameters, entity_declaration(parser, true));
        if (match(parser, TOKEN_COMMA)) {
            continue;
        }
    }

    return parameters;
}

static void parse_function_definition(Parser* parser, OrsoASTNode* function_definition) {
    ASSERT(function_definition->node_type == ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be a function defintion");
    function_definition->data.function_definition.parameters = NULL;
    function_definition->data.function_definition.return_type_expression = NULL;
    function_definition->data.function_definition.compilable = true;

    function_definition->data.function_definition.parameters = parse_parameters(parser);

    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect close parenthesis for end of arguments.");

    if (match(parser, TOKEN_ARROW_RIGHT)) {
        function_definition->data.function_definition.return_type_expression = expression(parser);
    }

    consume(parser, TOKEN_BRACE_OPEN, "Expect open brace for function body.");

    function_definition->data.function_definition.block = block(parser);
}

static OrsoExpressionNode* grouping_or_function_definition(Parser* parser) {
    OrsoASTNodeType node_type;
    if (is_incoming_expression_function_definition(parser)) {
        node_type = ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION;
    } else {
        node_type = ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, node_type, parser->previous);

    if (node_type == ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
        parse_function_definition(parser, expression_node);

        expression_node->type = &OrsoTypeUnresolved;
        expression_node->narrowed_type = &OrsoTypeUnresolved;
    } else {
        expression_node->data.expression = expression(parser);

        consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after expression.");

        expression_node->type = expression_node->data.expression->type;
        expression_node->narrowed_type = expression_node->type;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoASTNode** parse_arguments(Parser* parser) {
    OrsoASTNode** arguments = NULL;

    if (!check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            OrsoASTNode* argument = expression(parser);
            sb_push(arguments, argument);
        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after arguments.");

    if (sb_count(arguments) > MAX_PARAMETERS - 1) {
        error(parser, "Cannot have more than 100 arguments");
    }

    return arguments;
}

static OrsoExpressionNode* call(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_CALL, parser->previous);

    expression_node->data.call.callee = NULL;
    expression_node->data.call.arguments = parse_arguments(parser);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* unary(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_UNARY, parser->previous);
    expression_node->operator = parser->previous;

    expression_node->data.expression = parse_precedence(parser, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* binary(Parser* parser) {
    OrsoASTNode* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BINARY, parser->previous);

    Token operator = parser->previous;
    expression_node->operator = operator;
    expression_node->data.binary.lhs = NULL;
    expression_node->data.binary.rhs = NULL;

    ParseRule* rule = get_rule(operator.type);

    expression_node->start = parser->previous;
    expression_node->data.binary.rhs = parse_precedence(parser, (Precedence)(rule->precedence + 1));
    expression_node->end = parser->previous;

    return expression_node;
}

ParseRule rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { grouping_or_function_definition,   call,       PREC_CALL },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { block,      NULL,       PREC_CALL },
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
    [TOKEN_BAR]                     = { NULL,       binary,     PREC_BITWISE_OR },
    [TOKEN_PLUS_PLUS]               = { NULL,       NULL,       PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EQUAL_EQUAL]             = { NULL,       binary,     PREC_EQUALITY },
    [TOKEN_BANG_EQUAL]              = { NULL,       binary,     PREC_EQUALITY },
    [TOKEN_LESS_EQUAL]              = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL]           = { NULL,       binary,     PREC_COMPARISON },
    [TOKEN_ARROW_RIGHT]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_IDENTIFIER]              = { variable,   NULL,       PREC_NONE },
    [TOKEN_STRING]                  = { literal,    NULL,       PREC_NONE },
    [TOKEN_SYMBOL]                  = { literal,    NULL,       PREC_NONE },
    [TOKEN_INTEGER]                 = { number,     NULL,       PREC_NONE },
    [TOKEN_FLOAT]                   = { number,     NULL,       PREC_NONE },
    [TOKEN_ANNOTATION]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_STRUCT]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_FUNCTION]                = { NULL,       NULL,       PREC_NONE },
    [TOKEN_NOT]                     = { unary,      NULL,       PREC_NONE },
    [TOKEN_AND]                     = { NULL,       binary,     PREC_AND },
    [TOKEN_OR]                      = { NULL,       binary,     PREC_OR },
    [TOKEN_IF]                      = { ifelse,     NULL,       PREC_NONE },
    [TOKEN_UNLESS]                  = { ifelse,     NULL,       PREC_NONE },
    [TOKEN_WHILE]                   = { ifelse,     NULL,       PREC_NONE },
    [TOKEN_UNTIL]                   = { ifelse,     NULL,       PREC_NONE },
    [TOKEN_FOR]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_TRUE]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_FALSE]                   = { literal,    NULL,       PREC_NONE },
    [TOKEN_NULL]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_RETURN]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_PRINT_EXPR]              = { NULL,       NULL,       PREC_NONE },
    [TOKEN_PRINT]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EOF]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,       NULL,       PREC_NONE },
};

static OrsoASTNode* parse_precedence(Parser* parser, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
    OrsoASTNode* left_operand;

    if (prefix_rule == NULL) {
        error(parser, "Expect expression.");
        left_operand = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_UNDEFINED, parser->previous);
    } else {
        left_operand = prefix_rule(parser);
    }

    while (precedence <= get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = get_rule(parser->previous.type)->infix;
        OrsoASTNode* right_operand = infix_rule(parser);
        switch (right_operand->node_type) {
            case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY:
                right_operand->data.binary.lhs = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            case ORSO_AST_NODE_TYPE_EXPRESSION_CALL:
                left_operand = convert_call_expression(parser, left_operand, right_operand);
                break;
            case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
                left_operand = convert_assignment_expression(parser, left_operand, right_operand);
                break;
            default: UNREACHABLE();
        }
    }

    return left_operand;
}

static ParseRule* get_rule(TokenType type) {
    return &rules[type];
}

static OrsoASTNode* expression(Parser* parser) {
    bool fold = false;
    if (match(parser, TOKEN_DIRECTIVE)) {
        Token directive = parser->previous;
        fold = (directive.length - 1 == strlen("fold") && strncmp(directive.start + 1, "fold", 4) == 0);
    }

    OrsoASTNode* expression_node = parse_precedence(parser, PREC_ASSIGNMENT);
    expression_node->fold = fold;

    return expression_node;
}

static OrsoASTNode* statement(Parser* parser) {
    bool is_return = false;
    OrsoASTNodeType node_type;
    if (match(parser, TOKEN_RETURN)) {
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_RETURN;
        is_return = true;
    } else if (match(parser, TOKEN_PRINT_EXPR)) { 
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR;
    } if (match(parser, TOKEN_PRINT)) {
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_PRINT;
    } else {
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION;
    }

    OrsoASTNode* statement_node = orso_ast_node_new(parser->ast, node_type, parser->previous);

    if (!is_return || !match(parser, TOKEN_SEMICOLON)) {
        statement_node->data.expression = expression(parser);
        consume(parser, TOKEN_SEMICOLON, "Expect end of statement semicolon.");
    }

    statement_node->end = parser->previous;

    return statement_node;
}

static OrsoASTNode* entity_declaration(Parser* parser, bool as_parameter) {
    OrsoASTNode* entity_declaration_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_DECLARATION, parser->current);

    entity_declaration_node->type = &OrsoTypeUnresolved;

    entity_declaration_node->data.declaration.type_expression = NULL;
    entity_declaration_node->data.declaration.initial_value_expression = NULL;
    entity_declaration_node->value_index = -1;
    entity_declaration_node->data.declaration.fold_level_resolved_at = -1;
    entity_declaration_node->data.declaration.is_mutable = false;

    consume(parser, TOKEN_COLON, "Expect explicit type.");
    entity_declaration_node->data.declaration.type_expression = expression(parser);

    // TODO: try to do constant vs variable detection a little more clever...
    bool requires_expression = false;
    if (entity_declaration_node->data.declaration.type_expression) {
        if (match(parser, TOKEN_EQUAL)) {
            requires_expression = true;
            entity_declaration_node->data.declaration.is_mutable = true;
        } else if (match(parser, TOKEN_COLON)) {
            requires_expression = true;
        }
    } else {
        if (match(parser, TOKEN_EQUAL)) {
            entity_declaration_node->data.declaration.is_mutable = true;
        } else {
            consume(parser, TOKEN_COLON, "Expect define assignment.");
        }
        requires_expression = true;
    }

    if (requires_expression) {
        entity_declaration_node->data.declaration.initial_value_expression = expression(parser);
    }

    if (entity_declaration_node->data.declaration.initial_value_expression == NULL) {
        entity_declaration_node->data.declaration.is_mutable = true;
    }

    if (!as_parameter) {
        consume(parser, TOKEN_SEMICOLON, "Expect end of declaration semicolon.");
    }

    entity_declaration_node->end = parser->previous;
    return entity_declaration_node;
}

static bool is_incoming_declaration_declaration(Parser* parser) {
    if (parser->current.type != TOKEN_IDENTIFIER) {
        return false;
    }

    Lexer lookahead_lexer = parser->lexer;

    Token token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static OrsoASTNode* declaration(Parser* parser) {
    OrsoASTNode* node;
    if (is_incoming_declaration_declaration(parser)) {
        node = entity_declaration(parser, false);
    } else {
        node = statement(parser);
    }

    if (parser->panic_mode) {
        synchronize(parser);
    }

    return node;
}

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn) {
    Parser parser;
    parser_init(&parser, ast, source, error_fn);

    advance(&parser);

    ast->root = orso_ast_node_new(parser.ast, ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK, parser.previous);

    while (!match(&parser, TOKEN_EOF)) {
        OrsoASTNode* declaration_node = declaration(&parser);
        sb_push(ast->root->data.block, declaration_node);
    }

    consume(&parser, TOKEN_EOF, "Expect end of expression.");

    return !parser.had_error;
}

void ast_print_ast_node(OrsoASTNode* node, i32 initial) {
    const char type_str[128];
    orso_type_to_cstrn(node->type, (char*)type_str, 128);

    const char narrow_type_str[128];
    orso_type_to_cstrn(node->narrowed_type, (char*)narrow_type_str, 128);

    printf("%*s [%s][%s] ", initial, "", (char*)type_str, narrow_type_str);

    switch (node->node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: {
            Token operator = node->operator;
            printf("BINARY %.*s\n", operator.length, operator.start);

            ast_print_ast_node(node->data.binary.lhs, initial + 1);
            ast_print_ast_node(node->data.binary.rhs, initial + 1);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: {
            printf("GROUPING\n");

            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            printf("IMPLICIT CAST \n");

            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY: {
            printf("UNARY %.*s\n", node->operator.length, node->operator.start);

            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            printf("PRIMARY %.*s\n", node->start.length, node->start.start);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY: {
            printf("ENTITY - %.*s\n", node->start.length, node->start.start);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            printf("ASSIGNMENT\n");
            ast_print_ast_node(node->data.binary.lhs, initial + 1);
            ast_print_ast_node(node->data.binary.rhs, initial + 1);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: {
            printf("BLOCK\n");
            for (i32 i = 0; i < sb_count(node->data.block); i++) {
                ast_print_ast_node(node->data.block[i], initial + 1);
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            OrsoASTNode* condition = node->data.branch.condition;
            printf("BRANCH - %.*s\n", (i32)(condition->end.start + condition->end.length - condition->start.start), condition->start.start);
            ast_print_ast_node(node->data.branch.then_expression, initial + 1);
            if (node->data.branch.else_expression) {
                ast_print_ast_node(node->data.branch.else_expression, initial + 1);
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: {
            printf("CALL\n");

            for (i32 i = 0; i < sb_count(node->data.call.arguments); i++) {
                ast_print_ast_node(node->data.call.arguments[i], initial + 1);
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            printf("FUNCTION_DEFINITION - TODO: better print for this\n");
            break;
        }
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR:
            printf("print_expr\n");
            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        case ORSO_AST_NODE_TYPE_STATEMENT_PRINT:
            printf("print\n");
            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
            printf("expression\n");
            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
            printf("return\n");
            ast_print_ast_node(node->data.expression, initial + 1);
            break;
        case ORSO_AST_NODE_TYPE_DECLARATION: {
            printf("%*s", initial, "");
            printf("ENTITY DECLARATION - identifier: %.*s, type: ", node->start.length, node->start.start);

            const char type_str[128];
            orso_type_to_cstrn(node->type, (char*)type_str, 128);
            printf("%s", (char*)type_str);
            printf("\n");

            if (node->data.declaration.initial_value_expression != NULL) {
                ast_print_ast_node(node->data.declaration.initial_value_expression, initial + 1);
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_UNDEFINED:
            printf("undefined\n");
            break;
    }
}

void orso_ast_print(OrsoAST* ast, const char* name) {
    printf("=== %s ===\n", name);
    ast_print_ast_node(ast->root, 0);
}
