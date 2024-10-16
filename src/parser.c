#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "common.h"
#include "error.h"
#include "type.h"
#include "type_set.h"
#include "sb.h"

/*
program                  -> declaration* EOF
declaration              -> entity_declaration | statement
entity_declaration       -> IDENTIFIER `:` ((logic_or ((`=` | `:`) expression)?) | ((`=` | `:`) expression) `;`
parameters               -> parameter (`,` parameter)*
parameter                -> IDENTIFIER `:` ((logic_or (`=` expression)?)
statement                -> expression_statement
expression_statement     -> expression `;`

directive                -> `#` ~(\s)*
expression               -> directive? (assignment | block | branch)
block                    -> `{` declaration* `}`
branch                   -> (`if` | `unless` | `while` | `until` ) expression (block | (`then` | `do`) expression) (`else` expression)?

assignment               -> (call `.`)? IDENTIFIER `=` expression
                          | logic_or
logic_or                 -> logic_and (`or` logic_and)*
logic_and                -> equality (`and` equality)*
equality                 -> comparison ((`!=` | `==`) comparison)*
comparison               -> term ((`<` | `>` | `<=` | `>=`) term)*
term                     -> factor ((`+` | `-`) factor)*
factor                   -> logical_operations ((`/` | `*`) logical_operations)*
logical_operations       -> unary ((`|` |`&`) unary)*
unary                    -> (`not` | `-` | `&`) unary | call
call                     -> primary ( `(` arguments? `)` ) | `.` IDENTIFIER )*
arguments                -> argument (`,` argument)*
argument                 -> (IDENTIFIER `:`)? expression

primary                  -> `true` | `false` | `null` | IDENTIFIER | INTEGER | DECIMAL
                          | STRING | SYMBOL | `(` expression `)` | function_definition | function_type | struct_definition 
                          | type_init
function_definition      -> `(` parameters? `)` `->` logic_or block
function_type            -> `(` (logic_or (`,` logic_or)*)? `)` `->` logic_or
struct_definition        -> `struct` `{` entity_declaration* `}`
type_init                ->  logic_or`{` `}`
*/

static int ptr_hash(void* ptr) {
    return kh_int64_hash_func((khint64_t)ptr);
}

static int ptr_equal(void* a, void* b) {
    return kh_int64_hash_equal((khint64_t)a, (khint64_t)b);
}

implement_table(ptr2i32, void*, i32, ptr_hash, ptr_equal)
implement_table(type2ns, type_t*, ast_node_and_scope_t, ptr_hash, ptr_equal)

typedef struct Parser {
    error_function_t error_fn;
    ast_t* ast; // TODO: Consider moving this outside parser and instead passed through arguments
    lexer_t lexer;
    token_t previous;
    token_t current;

    bool had_error;
    bool panic_mode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_BLOCK,       // {}, branch expressions (if/unless/while/until else)
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_BITWISE_OR,  // | and type separator
    PREC_UNARY,       // - not &
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef ast_node_t* (*ParseFn)(Parser*, bool is_in_type_context);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

void orso_ast_init(ast_t* ast, symbol_table_t* symbols) {
    ASSERT(symbols, "cannot be null");
    ast->allocator = (arena_t){0};

    ast->resolved = false;
    ast->root = NULL;
    ast->nodes = NULL;
    ast->folded_constant_types = NULL;
    ast->folded_constants = NULL;
    ast->symbols = symbols;
    ast->function_definition_pairs = NULL;
    type_set_init(&ast->type_set, &ast->allocator);

    slot_t void_slot = ORSO_SLOT_I(0);
    slot_t bool_slot = ORSO_SLOT_I(1);
    sb_push(ast->folded_constants, void_slot);
    sb_push(ast->folded_constants, bool_slot);

    ast->void_index = 0;
    ast->true_index = 1;

    symbol_table_init(&ast->builtins, &ast->allocator);

    ast->type_to_zero_index = table_new(ptr2i32, &ast->allocator);
    ast->type_to_creation_node = table_new(type2ns, &ast->allocator);
}

void orso_ast_free(ast_t* ast) {
    arena_free(&ast->allocator);
}

ast_node_t* orso_ast_node_new(ast_t* ast, ast_node_type_t node_type, bool is_in_type_context, token_t start) {
    ast_node_t* node = (ast_node_t*)arena_alloc(&ast->allocator, sizeof(ast_node_t));
    
    sb_push(ast->nodes, node);

    node->node_type = node_type;
    node->start = start;
    node->end = start;
    node->operator = start;
    node->return_guarentee = ORSO_NO_RETURN_GUARENTEED;
    node->value_type = &OrsoTypeUnresolved;
    node->value_type_narrowed = &OrsoTypeUnresolved;

    node->fold = false;
    node->foldable = false;
    node->is_in_type_context = is_in_type_context;
    node->lvalue_node = NULL;

    node->value_index = -1;

    switch (node_type) {
        case ORSO_AST_NODE_TYPE_DECLARATION: {
            node->data.declaration.initial_value_expression = NULL;
            node->data.declaration.fold_level_resolved_at = -1;
            node->data.declaration.identifier = (token_t){ .start = 0, .length = 0, .type = TOKEN_IDENTIFIER, .line = -1 };
            node->data.declaration.is_mutable = false;
            node->data.declaration.type_expression = NULL;
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: {
            node->data.binary.lhs = NULL;
            node->data.binary.rhs = NULL;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: {
            node->data.block = NULL;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            node->data.branch.condition = NULL;
            node->data.branch.condition_negated = false;
            node->data.branch.then_expression = NULL;
            node->data.branch.else_expression = NULL;
            node->data.branch.looping = false;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: {
            node->data.call.callee = NULL;
            node->data.call.arguments = NULL;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
        case ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: {
            node->data.expression = NULL;
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            node->data.initiailizer.type = NULL;
            node->data.initiailizer.arguments = NULL;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            node->data.function.block = NULL;
            node->data.function.compilable = false;
            node->data.function.parameter_nodes = NULL;
            node->data.function.return_type_expression = NULL;
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            node->data.struct_.declarations = NULL;
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY:
            break;
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY:
        case ORSO_AST_NODE_TYPE_EXPRESSION_DOT: {
            node->data.dot.lhs = NULL;
            node->data.dot.referencing_declaration = NULL;
            node->data.dot.identifier = (token_t){ .length = 0, .line = -1, .start = NULL, .type = TOKEN_IDENTIFIER };
            break;
        }

        case ORSO_AST_NODE_TYPE_UNDEFINED: break;// UNREACHABLE();
    }

    return node;
}

static void parser_init(Parser* parser, ast_t* ast, const char* source, error_function_t error_fn) {
    lexer_init(&parser->lexer, source);
    parser->ast = ast;
    parser->error_fn = error_fn;
    parser->had_error = false;
    parser->panic_mode = false;
}

static void error_at(Parser* parser, token_t* token, const char* specific) {
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

    error_t error = {
        .type = ORSO_ERROR_COMPILE,
        .region_type = ORSO_ERROR_REGION_TYPE_TOKEN,
        .message = message,
        .region.token = *token,
    };
    parser->error_fn(error);
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

static void consume(Parser* parser, token_type_t type, const char* message) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }

    error_at_current(parser, message);
}

static FORCE_INLINE bool check(Parser* parser, token_type_t type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, token_type_t type) {
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

static ast_node_t* declaration(Parser* parser, bool is_top_level);
static ast_node_t* expression(Parser* parser, bool is_in_type_context);
static ast_node_t* statement(Parser* parser, bool lack_semicolin);
static ParseRule* get_rule(token_type_t type);
static bool check_expression(Parser* parser);
static ast_node_t* parse_precedence(Parser* parser, bool is_in_type_context, Precedence precedence);

static type_t* value_to_integer_type(i64 value) {
    if (value >= INT32_MIN && value <= INT32_MAX) {
        return &OrsoTypeInteger32;
    }
    return &OrsoTypeInteger64;
}

static i32 add_constant_value(Parser* parser, slot_t value, type_t* type) {
    i32 index = sb_count(parser->ast->folded_constants);
    sb_push(parser->ast->folded_constant_types, type);
    sb_push(parser->ast->folded_constants, value);
    return index;
}

bool orso_ast_node_type_is_decl_or_stmt(ast_node_type_t node_type) {
    switch (node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_CASE:
        case ORSO_AST_NODE_TYPE_UNDEFINED:
            return false;
        
        case ORSO_AST_NODE_TYPE_DECLARATION:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
            return true;
    }
}

bool orso_ast_node_type_is_expression(ast_node_type_t node_type) {
    switch (node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_CASE:
            return true;
        
        case ORSO_AST_NODE_TYPE_DECLARATION:
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
        case ORSO_AST_NODE_TYPE_UNDEFINED:
            return false;
    }
}

static ast_node_t* number(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY, is_in_type_context, parser->previous);

    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            i64 value = cstrn_to_i64(parser->previous.start, parser->previous.length);
            expression_node->value_type = value_to_integer_type(value);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(value), expression_node->value_type);
            break;
        }
        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.start, parser->previous.length);
            expression_node->value_type = &OrsoTypeFloat64;
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_F(value), &OrsoTypeFloat64);
            break;
        }
        default: UNREACHABLE();
    }

    expression_node->value_type_narrowed = expression_node->value_type;

    expression_node->foldable = true;

    return expression_node;
}

static ast_node_t* literal(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY, is_in_type_context, parser->previous);

    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expression_node->value_type = &OrsoTypeBool;

            i64 is_true = (i64)(parser->previous.type == TOKEN_TRUE);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(is_true), &OrsoTypeBool);
            break;
        }
        case TOKEN_NULL: {
            expression_node->value_type = &OrsoTypeVoid;
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_I(0), &OrsoTypeVoid);
            break;
        }
        case TOKEN_STRING: {
            expression_node->value_type = &OrsoTypeString;
            OrsoString* value = orso_new_string_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_P(value), &OrsoTypeString);
            break;
        };

        case TOKEN_SYMBOL: {
            expression_node->value_type = &OrsoTypeSymbol;
            symbol_t* value = orso_new_symbol_from_cstrn(expression_node->start.start + 1, expression_node->start.length - 2, parser->ast->symbols);
            expression_node->value_index = add_constant_value(parser, ORSO_SLOT_P(value), &OrsoTypeSymbol);
            break;
        }
        default:
            UNREACHABLE();
    }

    expression_node->value_type_narrowed = expression_node->value_type;

    expression_node->foldable = true;

    return expression_node;
}

static ast_node_t* convert_assignment_expression(Parser* parser, ast_node_t* left, ast_node_t* assignment) {
    (void)parser;
    assignment->start = left->start;
    assignment->data.binary.lhs = left;
    return assignment;
}

static ast_node_t* convert_call_expression(ast_node_t* left_operand, ast_node_t* call) {
    call->start = left_operand->start;
    call->data.call.callee = left_operand;
    return call;
}

static ast_node_t* convert_function_definition(Parser* parser, ast_node_t* left_operand, ast_node_t* function_definition) {
    for (i32 i = 0; i < sb_count(left_operand->data.function.parameter_nodes); i++) {
        ast_node_t* parameter = left_operand->data.function.parameter_nodes[i];
        if (parameter->node_type != ORSO_AST_NODE_TYPE_DECLARATION) {
            error_at(parser, &parameter->start, "Expect an entity declaration.");
            left_operand->node_type = ORSO_AST_NODE_TYPE_UNDEFINED;
            return left_operand;
        }
    }

    function_definition->data.function.parameter_nodes = left_operand->data.function.parameter_nodes;
    function_definition->data.function.return_type_expression = left_operand->data.function.return_type_expression;

    // prevents freeing the node  parameter and return expressions
    left_operand->node_type = ORSO_AST_NODE_TYPE_UNDEFINED;

    return function_definition;
}

static ast_node_t* assignment(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT, is_in_type_context, parser->previous);

    expression_node->data.binary.rhs = expression(parser, is_in_type_context);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t* named_variable(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY, is_in_type_context, parser->previous);
    expression_node->data.dot.identifier = parser->previous;
    return expression_node;
}

static ast_node_t* variable(Parser* parser, bool is_in_type_context) {
    return named_variable(parser, is_in_type_context);
}

static void parse_block(Parser* parser, ast_node_t* block) {
    block->return_guarentee = ORSO_NO_RETURN_GUARENTEED;
    block->data.block = NULL;

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        ast_node_t* declaration_node = declaration(parser, false);
        sb_push(block->data.block, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, "Expect '}' after block.");
}

static ast_node_t* block(Parser* parser, bool is_in_type_context) {
    (void)is_in_type_context;

    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK, is_in_type_context, parser->previous);

    parse_block(parser, expression_node);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t* ifelse(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING, is_in_type_context, parser->previous);

    expression_node->data.branch.condition_negated = false;
    if (parser->previous.type == TOKEN_UNLESS || parser->previous.type == TOKEN_UNTIL) {
        expression_node->data.branch.condition_negated = true;
    }

    expression_node->data.branch.looping = false;
    if (parser->previous.type == TOKEN_WHILE || parser->previous.type == TOKEN_UNTIL) {
        expression_node->data.branch.looping = true;
    }

    expression_node->return_guarentee = ORSO_NO_RETURN_GUARENTEED;

    expression_node->data.branch.condition = expression(parser, is_in_type_context);
    if (match(parser, TOKEN_BRACE_OPEN)) {
        expression_node->data.branch.then_expression = block(parser, is_in_type_context);
    } else {
        if (expression_node->data.branch.looping) {
            consume(parser, TOKEN_DO, "Expect 'do' or block after condition.");
        } else {
            consume(parser, TOKEN_THEN, "Expect 'then' or block after condition.");
        }

        {
            ast_node_t* then_statement = statement(parser, true);
            ast_node_t* then_expression = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT, is_in_type_context, then_statement->start);
            then_expression->data.statement = then_statement;
            then_expression->end = then_statement->end;
            expression_node->data.branch.then_expression = then_expression;
        }
    }

    if (!match(parser, TOKEN_ELSE)) {
        expression_node->data.branch.else_expression = NULL;
        return expression_node;
    }

    {
        ast_node_t* else_statement = statement(parser, true);
        ast_node_t* else_expression = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT, is_in_type_context, else_statement->start);
        else_expression->data.statement = else_statement;
        else_expression->end = else_statement->end;
        expression_node->data.branch.else_expression = else_expression;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static bool is_incoming_function_signature(Parser* parser) {
    ASSERT(parser->previous.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    lexer_t look_ahead_lexer = parser->lexer;
    i32 parenthesis_level = 1;
    token_t next = parser->current;
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
    
    return next.type == TOKEN_ARROW_RIGHT;
}

static bool is_incoming_declaration_declaration(Parser* parser) {
    if (parser->current.type != TOKEN_IDENTIFIER) {
        return false;
    }

    lexer_t lookahead_lexer = parser->lexer;

    token_t token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static ast_node_t* entity_declaration(Parser* parser, bool as_parameter);

static ast_node_t** parse_parameters(Parser* parser) {
    ast_node_t** parameters = NULL;
    until (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        if (is_incoming_declaration_declaration(parser)) {
            sb_push(parameters, entity_declaration(parser, true));
        } else {
            sb_push(parameters, expression(parser, true));
        }
        if (match(parser, TOKEN_COMMA)) {
            continue;
        }
    }

    return parameters;
}

static void parse_function_signature(Parser* parser, ast_node_t* function_definition) {
    ASSERT(function_definition->node_type == ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");
    function_definition->data.function.parameter_nodes = NULL;
    function_definition->data.function.return_type_expression = NULL;

    function_definition->data.function.parameter_nodes = parse_parameters(parser);

    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect close parenthesis for end of arguments.");

    consume(parser, TOKEN_ARROW_RIGHT, "Expect -> for return type.");

    function_definition->data.function.return_type_expression = expression(parser, true);
}

static ast_node_t* function_definition(Parser* parser, bool is_in_type_context) {
    ast_node_t* function_definition_expression = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, is_in_type_context, parser->previous);
    function_definition_expression->data.function.compilable = true;
    function_definition_expression->data.function.block = block(parser, is_in_type_context);
    function_definition_expression->end = parser->previous;

    return function_definition_expression;
}

static ast_node_t* grouping_or_function_signature_or_definition(Parser* parser, bool is_in_type_context) {
    ast_node_type_t node_type;
    if (is_incoming_function_signature(parser)) {
        node_type = ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE;
    } else {
        node_type = ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    ast_node_t* expression_node = orso_ast_node_new(parser->ast, node_type, is_in_type_context, parser->previous);

    if (node_type == ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        parse_function_signature(parser, expression_node);

        expression_node->value_type = &OrsoTypeUnresolved;
        expression_node->value_type_narrowed = &OrsoTypeUnresolved;
        expression_node->end = parser->previous;

        if (match(parser, TOKEN_BRACE_OPEN)) {
            ast_node_t *definition = function_definition(parser, is_in_type_context);
            expression_node = convert_function_definition(parser, expression_node, definition);
        }
    } else {
        expression_node->data.expression = expression(parser, is_in_type_context);

        consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after expression.");

        expression_node->value_type = expression_node->data.expression->value_type;
        expression_node->value_type_narrowed = expression_node->value_type;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t **parse_arguments(Parser* parser, bool is_in_type_context) {
    ast_node_t **arguments = NULL;

    if (!check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            ast_node_t *argument = expression(parser, is_in_type_context);
            sb_push(arguments, argument);
        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_PARENTHESIS_CLOSE, "Expect ')' after arguments.");

    if (sb_count(arguments) > MAX_PARAMETERS - 1) {
        error(parser, "Cannot have more than 100 arguments");
    }

    return arguments;
}

static ast_node_t* call(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_CALL, is_in_type_context, parser->previous);

    expression_node->data.call.callee = NULL;
    expression_node->data.call.arguments = parse_arguments(parser, is_in_type_context);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t* unary(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_UNARY, is_in_type_context, parser->previous);
    expression_node->operator = parser->previous;

    expression_node->data.expression = parse_precedence(parser, is_in_type_context, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t* binary(Parser* parser, bool is_in_type_context) {
    ast_node_t* expression_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_BINARY, is_in_type_context, parser->previous);

    token_t operator = parser->previous;
    expression_node->operator = operator;
    expression_node->data.binary.lhs = NULL;
    expression_node->data.binary.rhs = NULL;

    ParseRule* rule = get_rule(operator.type);

    expression_node->start = parser->previous;
    expression_node->data.binary.rhs = parse_precedence(parser, is_in_type_context, (Precedence)(rule->precedence + 1));
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t* struct_(Parser* parser, bool is_in_type_context) {
    (void)is_in_type_context;

    ast_node_t* struct_definition = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION, is_in_type_context, parser->previous);

    consume(parser, TOKEN_BRACE_OPEN, "Expect open brace.");

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        ast_node_t* declaration_node = entity_declaration(parser, false);
        sb_push(struct_definition->data.block, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, "Expect close brace.");

    struct_definition->end = parser->previous;

    return struct_definition;
}

static ast_node_t* print_(Parser* parser, bool is_in_type_context) {
    ast_node_type_t node_type = parser->previous.type == TOKEN_PRINT ? ORSO_AST_NODE_TYPE_EXPRESSION_PRINT : ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR;
    ast_node_t* print_expression = orso_ast_node_new(parser->ast, node_type, is_in_type_context, parser->previous);

    print_expression->data.expression = expression(parser, is_in_type_context);

    print_expression->end = parser->previous;

    return print_expression;
}

static ast_node_t *dot(Parser* parser, bool is_in_type_context) {


    if (match(parser, TOKEN_BRACE_OPEN)) {
        ast_node_t *initiailizer = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER, is_in_type_context, parser->previous);
        if (!match(parser, TOKEN_BRACE_CLOSE)) {
            do {
                ast_node_t *argument = expression(parser, is_in_type_context);
                sb_push(initiailizer->data.initiailizer.arguments, argument);

            } while (match(parser, TOKEN_COMMA));

            consume(parser, TOKEN_BRACE_CLOSE, "Expect brace close after brace open for initializer right now.");
        }

        initiailizer->end = parser->previous;

        return initiailizer;
    } else {
        ast_node_t *dot_expression = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_EXPRESSION_DOT, is_in_type_context, parser->previous);
        consume(parser, TOKEN_IDENTIFIER, "dot can only have identifiers on the right for now.");
        dot_expression->data.dot.identifier = parser->previous;
        dot_expression->end = parser->previous;

        return dot_expression;
    }
}

ParseRule rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { grouping_or_function_signature_or_definition,   call,       PREC_CALL },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { block,      NULL,       PREC_BLOCK },
    [TOKEN_BRACE_CLOSE]             = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACKET_OPEN]            = { NULL,       NULL,       PREC_NONE },
    [TOKEN_BRACKET_CLOSE]           = { NULL,       NULL,       PREC_NONE },
    [TOKEN_COMMA]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_DOT]                     = { NULL,       dot,        PREC_CALL },
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
    [TOKEN_AMPERSAND]               = { unary,      NULL,       PREC_UNARY },
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
    [TOKEN_STRUCT]                  = { struct_,    NULL,       PREC_NONE },
    [TOKEN_NOT]                     = { unary,      NULL,       PREC_NONE },
    [TOKEN_AND]                     = { NULL,       binary,     PREC_AND },
    [TOKEN_OR]                      = { NULL,       binary,     PREC_OR },
    [TOKEN_IF]                      = { ifelse,     NULL,       PREC_BLOCK },
    [TOKEN_THEN]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_UNLESS]                  = { ifelse,     NULL,       PREC_BLOCK },
    [TOKEN_WHILE]                   = { ifelse,     NULL,       PREC_BLOCK },
    [TOKEN_UNTIL]                   = { ifelse,     NULL,       PREC_BLOCK },
    [TOKEN_FOR]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_DO]                      = { NULL,       NULL,       PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,       NULL,       PREC_NONE },
    [TOKEN_TRUE]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_FALSE]                   = { literal,    NULL,       PREC_NONE },
    [TOKEN_NULL]                    = { literal,    NULL,       PREC_NONE },
    [TOKEN_RETURN]                  = { NULL,       NULL,       PREC_NONE },
    [TOKEN_PRINT_EXPR]              = { print_,     NULL,       PREC_NONE },
    [TOKEN_PRINT]                   = { print_,     NULL,       PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,       NULL,       PREC_NONE },
    [TOKEN_EOF]                     = { NULL,       NULL,       PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,       NULL,       PREC_NONE },
};

static bool check_expression(Parser* parser) {
    return get_rule(parser->current.type)->prefix != NULL;
}

static ast_node_t* parse_precedence(Parser* parser, bool is_in_type_context, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
    ast_node_t* left_operand;

    if (prefix_rule == NULL) {
        error(parser, "Expect expression.");
        left_operand = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_UNDEFINED, is_in_type_context, parser->previous);
    } else {
        left_operand = prefix_rule(parser, is_in_type_context);
        left_operand->is_in_type_context = is_in_type_context;
    }

    while (precedence <= get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = get_rule(parser->previous.type)->infix;
        ast_node_t* right_operand = infix_rule(parser, is_in_type_context);
        right_operand->is_in_type_context = is_in_type_context;

        switch (right_operand->node_type) {
            case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY:
                right_operand->data.binary.lhs = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            case ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
                right_operand->data.initiailizer.type = left_operand;
                left_operand = right_operand;
                break;
            }
            case ORSO_AST_NODE_TYPE_EXPRESSION_DOT:
                right_operand->data.dot.lhs = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            case ORSO_AST_NODE_TYPE_EXPRESSION_CALL:
                left_operand = convert_call_expression(left_operand, right_operand);
                break;
            case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
                left_operand = convert_assignment_expression(parser, left_operand, right_operand);
                break;
            default: UNREACHABLE();
        }
    }

    /*
     * I was a little liberal with the parsing of the function signature because I wanted it 
     * to capture both entity declarations and expressions for the parameters. This allows me
     * to combine a block and a signature to make a definition and then check if all the
     * parameters and entity declarations. That case is handled already.
     * 
     * Now here, I need to verify that all the parameters are expressions because
     * this is the signature case. I only want the parameters to be expressions of types.
     * 
     * On a special note, this is a good example on why implementing your own parser is
     * good. It allows you to easily make special cases for your grammar. Unless you're,
     * for some reason, focused on making an LL1 parser, don't worry about writing a
     * restrictive grammar.
     */
    if (left_operand->node_type == ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        for (i32 i = 0; i < sb_count(left_operand->data.function.parameter_nodes); i++) {
            ast_node_t* parameter = left_operand->data.function.parameter_nodes[i];
            if (!orso_ast_node_type_is_expression(parameter->node_type)) {
                error_at(parser, &parameter->start, "Expect a type expression here.");
                break;
            }
        }
    }

    return left_operand;
}

static ParseRule* get_rule(token_type_t type) {
    return &rules[type];
}

static ast_node_t* expression(Parser* parser, bool is_in_type_context) {
    bool fold = false;
    if (match(parser, TOKEN_DIRECTIVE)) {
        token_t directive = parser->previous;
        fold = (directive.length - 1 == strlen("fold") && strncmp(directive.start + 1, "fold", 4) == 0);

        bool is_type_directive = (directive.length - 1 == strlen("type") && strncmp(directive.start + 1, "type", 4) == 0);
        if (is_type_directive) {
            is_in_type_context = is_type_directive;
        }
    }

    ast_node_t* expression_node = parse_precedence(parser, is_in_type_context, is_in_type_context ? PREC_OR : PREC_ASSIGNMENT);
    expression_node->fold = fold;

    return expression_node;
}

// TODO: I don't even remember why i have as_expression_statement here... i don't know whats the point???
static ast_node_t* statement(Parser* parser, bool omit_end_of_statement) {
    bool is_return = false;
    ast_node_type_t node_type;
    if (match(parser, TOKEN_RETURN)) {
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_RETURN;
        is_return = true;
    } else {
        node_type = ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION;
    }

    ast_node_t* statement_node = orso_ast_node_new(parser->ast, node_type, false, parser->current);

    // this can be shortened to (!is_return || check_expression(parser)) but this is clearer
    if (is_return) {
        // expression is option in this case
        if (check_expression(parser)) {
            statement_node->data.expression = expression(parser, false);
        }
    } else {
        statement_node->data.expression = expression(parser, false);
    }
        
    if (!omit_end_of_statement) {
        consume(parser, TOKEN_SEMICOLON, "Expect end of statement semicolon.");
    }

    statement_node->end = parser->previous;

    return statement_node;
}

static ast_node_t* entity_declaration(Parser* parser, bool as_parameter) {
    ast_node_t* entity_declaration_node = orso_ast_node_new(parser->ast, ORSO_AST_NODE_TYPE_DECLARATION, false, parser->current);

    advance(parser);

    entity_declaration_node->value_type = &OrsoTypeUnresolved;

    entity_declaration_node->data.declaration.type_expression = NULL;
    entity_declaration_node->data.declaration.initial_value_expression = NULL;
    entity_declaration_node->value_index = -1;
    entity_declaration_node->data.declaration.fold_level_resolved_at = -1;
    entity_declaration_node->data.declaration.is_mutable = false;

    entity_declaration_node->data.declaration.identifier = parser->previous;

    consume(parser, TOKEN_COLON, "Expect explicit type.");

    if (!check(parser, TOKEN_EQUAL) && !check(parser, TOKEN_COLON)) {
        entity_declaration_node->data.declaration.type_expression = expression(parser, true);
    }

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
        entity_declaration_node->data.declaration.initial_value_expression = expression(parser, false);
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

static ast_node_t* declaration(Parser* parser, bool is_top_level) {
    ast_node_t* node = NULL;
    if (is_incoming_declaration_declaration(parser)) {
        node = entity_declaration(parser, false);
    } else {
        unless (is_top_level) {
            node = statement(parser, false);
        } else {
            error_at_current(parser, "Expect global entity declarations.");
            advance(parser);
        }
    }

    if (parser->panic_mode) {
        synchronize(parser);
    }

    return node;
}

bool orso_parse(ast_t* ast, const char* source, error_function_t error_fn) {
    Parser parser;
    parser_init(&parser, ast, source, error_fn);

    advance(&parser);

    ast->root = orso_ast_node_new(parser.ast, ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK, false, parser.previous);

    while (!match(&parser, TOKEN_EOF)) {
        ast_node_t* declaration_node = declaration(&parser, true);
        if (declaration_node) {
            sb_push(ast->root->data.block, declaration_node);
        }
    }

    consume(&parser, TOKEN_EOF, "Expect end of file.");

    return !parser.had_error;
}

void ast_print_ast_node(ast_node_t* node, i32 initial, const char* prefix) {
    printf("%*s%s", initial, "", prefix);

    const char type_str[128];
    const char narrow_type_str[128];
    
    orso_type_to_cstrn(node->value_type, (char*)type_str, 128);
    const char type_info[128];

    if (node->value_type != node->value_type_narrowed) {
        orso_type_to_cstrn(node->value_type_narrowed, (char*)narrow_type_str, 128);
        snprintf((char*)type_info, 128, "%s (%s)", type_str, narrow_type_str);
    } else {
        snprintf((char*)type_info, 128, "%s", type_str);
    }

    switch (node->node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: {
            token_t operator = node->operator;
            printf("binary(%.*s): %s\n", operator.length, operator.start, type_info);

            ast_print_ast_node(node->data.binary.lhs, initial + 1, "left: ");
            ast_print_ast_node(node->data.binary.rhs, initial + 1, "right: ");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: {
            printf("grouping: %s\n", type_info);

            ast_print_ast_node(node->data.expression, initial + 1, "");
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            printf("TODO: print arguments as well, type initializer: %s\n", type_info);
            ast_print_ast_node(node->data.initiailizer.type, initial + 1, "type: ");
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            printf("implicit cast: %s\n", type_info);

            ast_print_ast_node(node->data.expression, initial + 1, "from: ");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY: {
            printf("unary(%.*s): %s\n", node->operator.length, node->operator.start, type_info);

            ast_print_ast_node(node->data.expression, initial + 1, "");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            printf("primary(%.*s): %s\n", node->start.length, node->start.start, type_info);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY: {
            printf("entity(%.*s): %s\n", node->data.dot.identifier.length, node->data.dot.identifier.start, type_info);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            printf("assignment: %s\n", type_info);
            ast_print_ast_node(node->data.binary.lhs, initial + 1, "lvalue: ");
            ast_print_ast_node(node->data.binary.rhs, initial + 1, "rvalue: ");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: {
            printf("block: %s\n", type_info);
            for (i32 i = 0; i < sb_count(node->data.block); i++) {
                ast_print_ast_node(node->data.block[i], initial + 1, "");
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            printf("branch: %s\n", type_info);
            ast_print_ast_node(node->data.branch.condition, initial + 1, "condition: ");
            ast_print_ast_node(node->data.branch.then_expression, initial + 2, "then: ");
            if (node->data.branch.else_expression) {
                ast_print_ast_node(node->data.branch.else_expression, initial + 2, "else: ");
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: {
            printf("call: %s\n", type_info);
            ast_print_ast_node(node->data.call.callee, initial + 1, "callee: ");

            for (i32 i = 0; i < sb_count(node->data.call.arguments); i++) {
                ast_print_ast_node(node->data.call.arguments[i], initial + 2, "arg: ");
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            printf("function: %s\n", type_info);
            for (i32 i = 0; i < sb_count(node->data.function.parameter_nodes); i++) {
                ast_print_ast_node(node->data.function.parameter_nodes[i], initial + 1, "param: ");
            }
            ast_print_ast_node(node->data.function.block, initial + 2, "");
            ast_print_ast_node(node->data.function.return_type_expression, initial + 1, "returns: ");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            printf("%s\n", type_info);
            for (i32 i = 0; i < sb_count(node->data.struct_.declarations); i++) {
                ast_print_ast_node(node->data.struct_.declarations[i], initial + 1, "field: ");
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            printf("function signature: %s\n", type_info);
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT:
            printf("expression: %s\n", type_info);
            ast_print_ast_node(node->data.statement, initial + 1, "statement: ");
            break;
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR:
            printf("print_expr\n");
            ast_print_ast_node(node->data.expression, initial + 1, "");
            break;
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT:
            printf("print\n");
            ast_print_ast_node(node->data.expression, initial + 1, "");
            break;
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
            printf("statement\n");
            ast_print_ast_node(node->data.expression, initial + 1, "expression: ");
            break;
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
            printf("return\n");
            ast_print_ast_node(node->data.expression, initial + 1, "value: ");
            break;
        case ORSO_AST_NODE_TYPE_EXPRESSION_DOT:
            printf("dot(%.*s): %s\n", node->data.dot.identifier.length, node->data.dot.identifier.start, type_info);
            if (node->data.dot.lhs) {
                ast_print_ast_node(node->data.dot.lhs, initial + 1, "target: ");
            }
            break;
        case ORSO_AST_NODE_TYPE_DECLARATION: {
            printf("declaration(%.*s): ", node->start.length, node->start.start);

            const char type_str[128];
            orso_type_to_cstrn(node->value_type, (char*)type_str, 128);
            printf("%s", (char*)type_str);
            printf("\n");

            if (node->data.declaration.initial_value_expression != NULL) {
                ast_print_ast_node(node->data.declaration.initial_value_expression, initial + 1, "initial: ");
            }
            break;
        }
        case ORSO_AST_NODE_TYPE_UNDEFINED:
            printf("undefined\n");
            break;
    }
}

void orso_ast_print(ast_t* ast, const char* name) {
    printf("=== %s ===\n", name);
    ast_print_ast_node(ast->root, 0, "");
}
