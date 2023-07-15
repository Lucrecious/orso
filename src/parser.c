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
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // - not
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef OrsoExpressionNode* (*ParseFn)(Parser*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

void orso_ast_init(OrsoAST* ast, OrsoSymbolTable* symbols) {
    ASSERT(symbols, "cannot be null");
    ast->resolved = false;
    ast->declarations = NULL;
    ast->folded_constants = NULL;
    ast->symbols = symbols;
    ast->function_definition_pairs = NULL;
    orso_type_set_init(&ast->type_set);

    orso_symbol_table_init(&ast->builtins);
}

static void free_declaration(OrsoDeclarationNode* declaration_node);

static void orso_free_expression(OrsoExpressionNode* expression) {
    if (!expression) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_BINARY:
            orso_free_expression(expression->expr.binary.left);
            orso_free_expression(expression->expr.binary.right);
            free(expression->expr.binary.left);
            free(expression->expr.binary.right);
            expression->expr.binary.left = NULL;
            expression->expr.binary.right = NULL;
            break;
        case EXPRESSION_GROUPING:
            orso_free_expression(expression->expr.grouping.expression);
            free(expression->expr.grouping.expression);
            expression->expr.grouping.expression = NULL;
            break;
        case EXPRESSION_IMPLICIT_CAST:
            orso_free_expression(expression->expr.cast.operand);
            free(expression->expr.cast.operand);
            expression->expr.cast.operand = NULL;
            break;
        case EXPRESSION_UNARY:
            orso_free_expression(expression->expr.unary.operand);
            free(expression->expr.unary.operand);
            expression->expr.unary.operand = NULL;
            break;
        case EXPRESSION_ASSIGNMENT:
            orso_free_expression(expression->expr.assignment.right_side);
            free(expression->expr.assignment.right_side);
            expression->expr.assignment.right_side = NULL;
            break;
        case EXPRESSION_ENTITY:
        case EXPRESSION_PRIMARY:
            break;
        case EXPRESSION_BLOCK: {
            for (i32 i = 0; i < sb_count(expression->expr.block.declarations); i++) {
                free_declaration(expression->expr.block.declarations[i]);
                free(expression->expr.block.declarations[i]);
                expression->expr.block.declarations[i] = NULL;
            }

            sb_free(expression->expr.block.declarations);
            expression->expr.block.declarations = NULL;
            break;
        }
        case EXPRESSION_IFELSE: {
            orso_free_expression(expression->expr.ifelse.condition);
            free(expression->expr.ifelse.condition);
            expression->expr.ifelse.condition = NULL;
            
            orso_free_expression(expression->expr.ifelse.then);
            free(expression->expr.ifelse.then);
            expression->expr.ifelse.then = NULL;

            orso_free_expression(expression->expr.ifelse.else_);
            free(expression->expr.ifelse.else_);
            expression->expr.ifelse.else_ = NULL;
            break;
        }

        case EXPRESSION_FUNCTION_DEFINITION: {
            // TODO: Gonna use regions for this shit.
            break;
        }
        case EXPRESSION_CALL: {
            break;
        }
        case EXPRESSION_FOR: UNREACHABLE();

        case EXPRESSION_NONE: {
            break;
        }

    }
}

static void orso_type_node_free(OrsoTypeNode* type_node) {
    if (type_node == NULL) {
        return;
    }

    switch (type_node->type) {
        case ORSO_TYPE_NODE_TYPE_IDENTIFIER:
            break;
        case ORSO_TYPE_NODE_TYPE_UNION: {
            for (i32 i = 0; i < sb_count(type_node->items.union_); i++) {
                orso_type_node_free(type_node->items.union_[i]);
                free(type_node->items.union_[i]);
            }

            sb_free(type_node->items.union_);
            type_node->items.union_ = NULL;
            break;
        }
        case ORSO_TYPE_NODE_TYPE_FUNCTION: {
            orso_type_node_free(type_node->items.function.return_type);
            free(type_node->items.function.return_type);
            type_node->items.function.return_type = NULL;

            for (i32 i = 0; i < sb_count(type_node->items.function.argument_types); i++) {
                orso_type_node_free(type_node->items.function.argument_types[i]);
                free(type_node->items.function.argument_types[i]);
            }

            sb_free(type_node->items.function.argument_types);
            type_node->items.function.argument_types = NULL;
            break;
        }
    }
}

static void free_declaration(OrsoDeclarationNode* declaration_node) {
    switch (declaration_node->type) {
        case ORSO_DECLARATION_STATEMENT: {
            orso_free_expression(declaration_node->decl.statement->stmt.expression);
            free(declaration_node->decl.statement->stmt.expression);
            free(declaration_node->decl.statement);
            declaration_node->decl.statement = NULL;
            break;
        }
        case ORSO_DECLARATION_ENTITY: {
            orso_free_expression(declaration_node->decl.entity->expression);
            free(declaration_node->decl.entity->expression);
            declaration_node->decl.entity->expression = NULL;

            orso_type_node_free(declaration_node->decl.entity->type_node);
            free(declaration_node->decl.entity->type_node);
            declaration_node->decl.entity->type_node = NULL;

            free(declaration_node->decl.entity);
            declaration_node->decl.entity = NULL;
            break;
        }
        case ORSO_DECLARATION_NONE: UNREACHABLE();
    }
}

void orso_ast_free(OrsoAST* ast) {
    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        free_declaration(ast->declarations[i]);
        free(ast->declarations[i]);
        ast->declarations[i] = NULL;
    }
    sb_free(ast->declarations);
    sb_free(ast->function_definition_pairs);
    ast->declarations = NULL;

    // TODO: Remember to free folded constants
    //sb_free(ast->folded_constants);
    //ast->folded_constants = NULL;

    orso_symbol_table_free(&ast->builtins);
    orso_type_set_free(&ast->type_set);
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

static OrsoDeclarationNode* declaration(Parser* parser);
static OrsoExpressionNode* expression(Parser* parser);
static ParseRule* get_rule(TokenType type);
static OrsoExpressionNode* parse_precedence(Parser* parser, Precedence precedence);

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

static OrsoExpressionNode* expression_new(Token start) {
    OrsoExpressionNode* expression_node = ORSO_ALLOCATE(OrsoExpressionNode);

    expression_node->start = start;
    expression_node->end = start;
    expression_node->type = EXPRESSION_NONE;

    expression_node->fold = false;
    expression_node->foldable = false;
    expression_node->folded_value_index = -1;
    
    expression_node->value_type = &OrsoTypeUnresolved;
    expression_node->narrowed_value_type = &OrsoTypeUnresolved;

    return expression_node;
}

static OrsoExpressionNode* number(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_PRIMARY;

    expression_node->expr.primary.token = parser->previous;

    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            i64 value = cstrn_to_i64(parser->previous.start, parser->previous.length);
            expression_node->value_type = value_to_integer_type(value);
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_I(value, expression_node->value_type));
            break;
        }
        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.start, parser->previous.length);
            expression_node->value_type = &OrsoTypeFloat64;
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_F(value, &OrsoTypeFloat64));
            break;
        }
        default: UNREACHABLE();
    }

    expression_node->narrowed_value_type = expression_node->value_type;

    expression_node->foldable = true;
    expression_node->folded_value_index = expression_node->expr.primary.value_index;

    return expression_node;
}

static OrsoExpressionNode* literal(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_PRIMARY;

    expression_node->expr.primary.token = parser->previous;

    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expression_node->value_type = &OrsoTypeBool;

            i64 is_true = (i64)(parser->previous.type == TOKEN_TRUE);
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_I(is_true, &OrsoTypeBool));
            break;
        }
        case TOKEN_NULL: {
            expression_node->value_type = &OrsoTypeVoid;
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_I(0, &OrsoTypeVoid));
            break;
        }
        case TOKEN_STRING: {
            expression_node->value_type = &OrsoTypeString;
            OrsoString* value = orso_new_string_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2);
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_P(value, &OrsoTypeString));
            break;
        };

        case TOKEN_SYMBOL: {
            expression_node->value_type = &OrsoTypeSymbol;
            OrsoSymbol* value = orso_new_symbol_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2, parser->ast->symbols);
            expression_node->expr.primary.value_index = add_constant_value(parser, ORSO_SLOT_P(value, &OrsoTypeSymbol));
            break;
        }
        default:
            UNREACHABLE();
    }

    expression_node->narrowed_value_type = expression_node->value_type;

    expression_node->foldable = true;
    expression_node->folded_value_index = expression_node->expr.primary.value_index;

    return expression_node;
}

static OrsoExpressionNode* convert_assignment_expression(Parser* parser, OrsoExpressionNode* left_operand, OrsoExpressionNode* assignment) {
    if (left_operand->type != EXPRESSION_ENTITY) {
        error_at(parser, &left_operand->start, "Expect entity name.");
        orso_free_expression(left_operand);
        orso_free_expression(assignment);
        free(assignment);
        left_operand->type = EXPRESSION_NONE;
        return left_operand;
    }

    assignment->start = left_operand->start;
    assignment->expr.assignment.name = left_operand->expr.entity.name;
    orso_free_expression(left_operand);
    free(left_operand);

    return assignment;
}

static OrsoExpressionNode* convert_call_expression(Parser* parser, OrsoExpressionNode* left_operand, OrsoExpressionNode* call) {
    if (left_operand->type != EXPRESSION_ENTITY) {
        error_at(parser, &left_operand->start, "Expect function name.");
        orso_free_expression(left_operand);
        orso_free_expression(call);
        free(call);
        left_operand->type = EXPRESSION_NONE;
        return left_operand;
    }

    call->start = left_operand->start;

    call->expr.call.callee = left_operand;

    return call;
}

static OrsoExpressionNode* assignment(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_ASSIGNMENT;

    expression_node->expr.assignment.right_side = expression(parser);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* named_variable(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_ENTITY;

    expression_node->expr.entity.name = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* variable(Parser* parser) {
    return named_variable(parser);
}

static void parse_block(Parser* parser, OrsoBlock* block) {
    block->declarations = NULL;
    block->final_expression_statement = NULL;

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        OrsoDeclarationNode* declaration_node = declaration(parser);
        sb_push(block->declarations, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, "Expect '}' after block.");
}

static OrsoExpressionNode* block(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_BLOCK;

    parse_block(parser, &expression_node->expr.block);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* ifelse(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);

    expression_node->type = EXPRESSION_IFELSE;

    expression_node->expr.ifelse.is_negated = false;
    if (parser->previous.type == TOKEN_UNLESS || parser->previous.type == TOKEN_UNTIL) {
        expression_node->expr.ifelse.is_negated = true;
    }

    expression_node->expr.ifelse.loop_block = false;
    if (parser->previous.type == TOKEN_WHILE || parser->previous.type == TOKEN_UNTIL) {
        expression_node->expr.ifelse.loop_block = true;
    }

    expression_node->expr.ifelse.condition = expression(parser);

    consume(parser, TOKEN_BRACE_OPEN, "Expect '{' after condition.");

    expression_node->expr.ifelse.then = block(parser);

    if (!match(parser, TOKEN_ELSE)) {
        expression_node->expr.ifelse.else_ = NULL;
        return expression_node;
    }

    if (match(parser, TOKEN_IF) || match(parser, TOKEN_UNLESS) ||
        match(parser, TOKEN_WHILE) || match(parser, TOKEN_UNTIL)) {
        expression_node->expr.ifelse.else_ = ifelse(parser);
        return expression_node;
    }

    consume(parser, TOKEN_BRACE_OPEN, "Expect '{' after else.");

    expression_node->expr.ifelse.else_ = block(parser);

    return expression_node;
}

static bool is_incoming_expression_function_definition(Parser* parser) {
    ASSERT(parser->previous.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    Lexer look_ahead_lexer = parser->lexer;
    i32 parenthesis_level = 1;
    Token next = parser->current;
    // get matching close parenthesis
    while (parenthesis_level > 0) {
        if (next.type == TOKEN_EOF) {
            error(parser, "File ended before closing the parenthesis.");
            return false;
        }

        if (next.type == TOKEN_PARENTHESIS_CLOSE) {
            parenthesis_level--;
            continue;
        }

        if (next.type == TOKEN_PARENTHESIS_OPEN) {
            parenthesis_level++;
        }

        next = lexer_next_token(&look_ahead_lexer);
    }

    next = lexer_next_token(&look_ahead_lexer);
    
    return next.type == TOKEN_ARROW_RIGHT || next.type == TOKEN_BRACE_OPEN;
}

static OrsoEntityDeclarationNode* entity_declaration(Parser* parser, bool as_parameter);

static OrsoEntityDeclarationNode** parse_parameters(Parser* parser) {
    if (!check(parser, TOKEN_IDENTIFIER)) {
        return NULL;
    }

    OrsoEntityDeclarationNode** parameters = NULL;
    while (check(parser, TOKEN_IDENTIFIER)) {
        sb_push(parameters, entity_declaration(parser, true));
        if (match(parser, TOKEN_COMMA)) {
            continue;
        }
    }

    return parameters;
}

static OrsoTypeNode* parse_type(Parser* parser, bool parse_union) {
    OrsoTypeNode* type_node = NULL;
    if (match(parser, TOKEN_IDENTIFIER)) {
        type_node = ORSO_ALLOCATE(OrsoTypeNode);
        type_node->type = ORSO_TYPE_NODE_TYPE_IDENTIFIER;
        type_node->items.primitive = parser->previous;
        type_node->start = type_node->end = parser->previous;
    } else if (match(parser, TOKEN_BRACE_OPEN)) {
        ASSERT(false, "not implemented");
    }

    if (parse_union && type_node && check(parser, TOKEN_BAR)) {
        OrsoTypeNode* union_type = ORSO_ALLOCATE(OrsoTypeNode);
        union_type->type = ORSO_TYPE_NODE_TYPE_UNION;
        union_type->items.union_ = NULL;
        sb_push(union_type->items.union_, type_node);

        union_type->start = type_node->start;

        while (match(parser, TOKEN_BAR)) {
            OrsoTypeNode* single_type = parse_type(parser, false);
            if (single_type) {
                sb_push(union_type->items.union_, single_type);
                union_type->end = single_type->end;
            } else {
                error(parser, "Expect type for union.");
                break;
            }
        }

        return union_type;
    }

    return type_node;
}

static void parse_function_definition(Parser* parser, OrsoFunctionDefinition* function_definition) {
    function_definition->parameters = NULL;
    function_definition->return_type = NULL;
    function_definition->cannot_compile = false;

    function_definition->parameters = parse_parameters(parser);

    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect close parenthesis for end of arguments.");

    if (match(parser, TOKEN_ARROW_RIGHT)) {
        function_definition->return_type = parse_type(parser, true);
    }

    consume(parser, TOKEN_BRACE_OPEN, "Expect open brace for function body.");

    function_definition->block_expression = block(parser);
}

static OrsoExpressionNode* grouping_or_function_definition(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);

    if (is_incoming_expression_function_definition(parser)) {
        expression_node->type = EXPRESSION_FUNCTION_DEFINITION;
        parse_function_definition(parser, &expression_node->expr.function_definition);

        expression_node->value_type = &OrsoTypeUnresolved;
        expression_node->narrowed_value_type = &OrsoTypeUnresolved;
    } else {
        expression_node->type = EXPRESSION_GROUPING;
        expression_node->expr.grouping.expression = expression(parser);

        consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after expression.");

        expression_node->value_type = expression_node->expr.grouping.expression->value_type;
        expression_node->narrowed_value_type = expression_node->expr.grouping.expression->value_type;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode** parse_arguments(Parser* parser) {
    OrsoExpressionNode** arguments = NULL;

    if (!check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            OrsoExpressionNode* argument = expression(parser);
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
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_CALL;

    expression_node->expr.call.callee = NULL;
    expression_node->expr.call.arguments = parse_arguments(parser);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* unary(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_UNARY;

    expression_node->expr.unary.operator = parser->previous;
    expression_node->expr.unary.operand = parse_precedence(parser, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static OrsoExpressionNode* binary(Parser* parser) {
    OrsoExpressionNode* expression_node = expression_new(parser->previous);
    expression_node->type = EXPRESSION_BINARY;

    expression_node->expr.binary.operator = parser->previous;
    expression_node->expr.binary.left = NULL;
    expression_node->expr.binary.right = NULL;

    ParseRule* rule = get_rule(expression_node->expr.binary.operator.type);

    expression_node->start = parser->previous;
    expression_node->expr.binary.right = parse_precedence(parser, (Precedence)(rule->precedence + 1));
    expression_node->end = parser->previous;

    return expression_node;
}

ParseRule rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { grouping_or_function_definition,   call,       PREC_CALL },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { block,      NULL,       PREC_NONE },
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
                right_operand->expr.binary.left = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            case EXPRESSION_CALL:
                left_operand = convert_call_expression(parser, left_operand, right_operand);
                break;
            case EXPRESSION_ASSIGNMENT:
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

static OrsoExpressionNode* expression(Parser* parser) {
    bool fold = false;
    if (match(parser, TOKEN_DIRECTIVE)) {
        Token directive = parser->previous;
        fold = (directive.length - 1 == strlen("fold") && strncmp(directive.start + 1, "fold", 4) == 0);
    }

    OrsoExpressionNode* expression_node = parse_precedence(parser, PREC_ASSIGNMENT);
    expression_node->fold = fold;

    return expression_node;
}

static OrsoStatementNode* statement(Parser* parser) {
    OrsoStatementNode* statement_node = ORSO_ALLOCATE(OrsoStatementNode);

    statement_node->start = parser->current;

    if (match(parser, TOKEN_RETURN)) {
        statement_node->type = ORSO_STATEMENT_RETURN;
    } else if (match(parser, TOKEN_PRINT_EXPR) || match(parser, TOKEN_PRINT)) {
        statement_node->type = ORSO_STATEMENT_PRINT_EXPR;
        if (parser->previous.type == TOKEN_PRINT) {
            statement_node->type = ORSO_STATEMENT_PRINT;
        }
    } else {
        statement_node->type = ORSO_STATEMENT_EXPRESSION;
    }

    statement_node->stmt.expression = expression(parser);

    consume(parser, TOKEN_SEMICOLON, "Expect end of statement semicolon.");

    statement_node->end = parser->previous;

    return statement_node;
}

static Token parse_variable(Parser* parser) {
    Token identifier = parser->current;
    advance(parser);
    return identifier;
}

static OrsoEntityDeclarationNode* entity_declaration(Parser* parser, bool as_parameter) {
    OrsoEntityDeclarationNode* entity_declaration_node = ORSO_ALLOCATE(OrsoEntityDeclarationNode);
    entity_declaration_node->start = parser->current;

    entity_declaration_node->type = &OrsoTypeUnresolved;

    entity_declaration_node->type_node = NULL;
    entity_declaration_node->expression = NULL;
    entity_declaration_node->implicit_default_value_index = -1;
    entity_declaration_node->fold_level_resolved_at = -1;
    entity_declaration_node->is_mutable = false;

    entity_declaration_node->name = parse_variable(parser);

    consume(parser, TOKEN_COLON, "Expect explicit type.");
    entity_declaration_node->type_node = parse_type(parser, true);

    // TODO: try to do constant vs variable detection a little more clever...
    bool requires_expression = false;
    if (entity_declaration_node->type_node) {
        if (match(parser, TOKEN_EQUAL)) {
            requires_expression = true;
            entity_declaration_node->is_mutable = true;
        } else if (match(parser, TOKEN_COLON)) {
            requires_expression = true;
        }
    } else {
        if (match(parser, TOKEN_EQUAL)) {
            entity_declaration_node->is_mutable = true;
        } else {
            consume(parser, TOKEN_COLON, "Expect define assignment.");
        }
        requires_expression = true;
    }

    if (requires_expression) {
        entity_declaration_node->expression = expression(parser);
    }

    if (entity_declaration_node->expression == NULL) {
        entity_declaration_node->is_mutable = true;
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

static OrsoDeclarationNode* declaration(Parser* parser) {
    Token start = parser->current;

    OrsoDeclarationNode* declaration_node = ORSO_ALLOCATE(OrsoDeclarationNode);
    declaration_node->type = ORSO_DECLARATION_NONE;
    if (is_incoming_declaration_declaration(parser)) {
        declaration_node->type = ORSO_DECLARATION_ENTITY;
        declaration_node->decl.entity = entity_declaration(parser, false);
    } else {
        declaration_node->type = ORSO_DECLARATION_STATEMENT;
        declaration_node->decl.statement = statement(parser);
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
    parser_init(&parser, ast, source, error_fn);

    advance(&parser);

    while (!match(&parser, TOKEN_EOF)) {
        OrsoDeclarationNode* declaration_node = declaration(&parser);
        sb_push(ast->declarations, declaration_node);
    }

    consume(&parser, TOKEN_EOF, "Expect end of expression.");

    return !parser.had_error;
}

void ast_print_declaration(OrsoDeclarationNode* declaration, i32 initial_indent);

void ast_print_expression(OrsoExpressionNode* expression, i32 initial) {
    const char type_str[128];
    orso_type_to_cstrn(expression->value_type, (char*)type_str, 128);

    const char narrow_type_str[128];
    orso_type_to_cstrn(expression->narrowed_value_type, (char*)narrow_type_str, 128);

    printf("%*s [%s][%s] ", initial, "", (char*)type_str, narrow_type_str);

    switch (expression->type) {
        case EXPRESSION_BINARY: {
            printf("BINARY %.*s\n", expression->expr.binary.operator.length, expression->expr.binary.operator.start);

            ast_print_expression(expression->expr.binary.left, initial + 1);
            ast_print_expression(expression->expr.binary.right, initial + 1);
            break;
        }
        case EXPRESSION_GROUPING: {
            printf("GROUPING\n");

            ast_print_expression(expression->expr.grouping.expression, initial + 1);
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            printf("IMPLICIT CAST \n");

            ast_print_expression(expression->expr.cast.operand, initial + 1);
            break;
        }
        case EXPRESSION_UNARY: {
            printf("UNARY %.*s\n", expression->expr.unary.operator.length, expression->expr.unary.operator.start);

            ast_print_expression(expression->expr.unary.operand, initial + 1);
            break;
        }
        case EXPRESSION_PRIMARY: {
            printf("PRIMARY %.*s\n", expression->expr.primary.token.length, expression->expr.primary.token.start);
            break;
        }
        case EXPRESSION_ENTITY: {
            printf("VARIABLE - %.*s\n", expression->expr.entity.name.length, expression->expr.entity.name.start);
            break;
        }
        case EXPRESSION_ASSIGNMENT: {
            printf("ASSIGNMENT - %.*s\n", expression->expr.assignment.name.length, expression->expr.assignment.name.start);
            ast_print_expression(expression->expr.assignment.right_side, initial + 1);
            break;
        }
        case EXPRESSION_BLOCK: {
            printf("BLOCK\n");
            for (i32 i = 0; i < sb_count(expression->expr.block.declarations); i++) {
                ast_print_declaration(expression->expr.block.declarations[i], initial + 1);
            }
            break;
        }
        case EXPRESSION_IFELSE: {
            OrsoExpressionNode* condition = expression->expr.ifelse.condition;
            printf("IFWHILEELSE - %.*s\n", (i32)(condition->end.start + condition->end.length - condition->start.start), condition->start.start);
            ast_print_expression(expression->expr.ifelse.then, initial + 1);
            if (expression->expr.ifelse.else_) {
                ast_print_expression(expression->expr.ifelse.else_, initial + 1);
            }
            break;
        }
        case EXPRESSION_CALL: {
            printf("CALL - (");

            OrsoExpressionNode** arguments = expression->expr.call.arguments;
            for (i32 i = 0; i < sb_count(arguments); i++) {
                OrsoExpressionNode* e = arguments[i];
                printf("%.*s%s", (i32)(e->end.start + e->end.length - e->start.start), e->start.start,
                        i == sb_count(arguments) - 1 ? "" : ", ");
            }

            printf(")\n");
            break;
        }
        case EXPRESSION_FUNCTION_DEFINITION: {
            printf("FUNCTION_DEFINITION - TODO: better print for this\n");
            break;
        }
        case EXPRESSION_FOR:
        case EXPRESSION_NONE: UNREACHABLE();
    }
}

void ast_print_statement(OrsoStatementNode* statement, i32 indent) {
    printf("%*s", indent, "");
    printf("STATEMENT - ");
    switch(statement->type) {
        case ORSO_STATEMENT_NONE: return;
        case ORSO_STATEMENT_PRINT_EXPR:
            printf("print_expr\n");
            ast_print_expression(statement->stmt.expression, indent + 1);
            break;
        case ORSO_STATEMENT_PRINT:
            printf("print\n");
            ast_print_expression(statement->stmt.expression, indent + 1);
            break;
        case ORSO_STATEMENT_EXPRESSION:
            printf("expression\n");
            ast_print_expression(statement->stmt.expression, indent + 1);
            break;
        case ORSO_STATEMENT_RETURN:
            printf("return\n");
            ast_print_expression(statement->stmt.expression, indent + 1);
            break;
    }
}

void ast_print_entity_declaration(OrsoEntityDeclarationNode* entity_declaration_node, i32 indent) {
    printf("%*s", indent, "");
    printf("ENTITY DECLARATION - identifier: %.*s, type: ", entity_declaration_node->name.length, entity_declaration_node->name.start);

    const char type_str[128];
    orso_type_to_cstrn(entity_declaration_node->type, (char*)type_str, 128);
    printf("%s", (char*)type_str);
    printf("\n");

    if (entity_declaration_node->expression != NULL) {
        ast_print_expression(entity_declaration_node->expression, indent + 1);
    }
}

void ast_print_declaration(OrsoDeclarationNode* declaration, i32 initial_indent) {
    if (declaration->type == ORSO_DECLARATION_NONE) {
        return;
    }

    Token start = declaration->start;
    Token end = declaration->end;
    printf("%*s DECLARATION %.*s\n", initial_indent, "", (u32)((end.start + end.length) - start.start), start.start);

    switch (declaration->type) {
        case ORSO_DECLARATION_NONE: return;
        case ORSO_DECLARATION_STATEMENT: {
            ast_print_statement(declaration->decl.statement, initial_indent + 1);
            break;
        }
        case ORSO_DECLARATION_ENTITY: {
            ast_print_entity_declaration(declaration->decl.entity, initial_indent + 2);
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
