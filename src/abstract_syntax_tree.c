#include "abstract_syntax_tree.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"

#include "sb.h"

void abstract_syntax_tree_init(AbstractSyntaxTree* ast) {
    ast->program = NULL;
}

static void identifier_node_free(IdentifierNode* identifier) {
    free(identifier->value);
    identifier->value = NULL;
}

static void expression_node_free(ExpressionNode* expression) {
    if (expression == NodeType_IDENTIFIER) {
        identifier_node_free(&expression->identifier);
    }
}

static void assignment_node_free(AssignmentNode* assignment) {
    expression_node_free(assignment->expression);
    identifier_node_free(&assignment->identifier);
    free(assignment->expression);
    assignment->expression = NULL;
}

static void statement_node_free(StatementNode* statement) {
    assignment_node_free(statement->assignment);
    statement->assignment = NULL;
}

void abstract_syntax_tree_free(AbstractSyntaxTree* ast) {
    for (i32 i = 0; i < sb_count(ast->program->statement_ptr_list); i++) {
        StatementNode* statement = ast->program->statement_ptr_list[i];
        statement_node_free(statement);
        free(statement);
    }

    sb_free(ast->program->statement_ptr_list);
    free(ast->program);
    ast->program = NULL;
}

static void advance(TreeParserState* state) {
    state->consumed = state->consuming;
    for (;;) {
        state->consuming = lexer_next_token(state->lexer_state);
        if (state->consuming.type == TokenType_INVALID) {
            state->error = SavineError_Parse_UNEXPECTED_TOKEN;
            // TODO: instead of skipping errors, report it something
            continue;
        }

        break;
    }
}

static void consume(TreeParserState* state, TokenType type) {
    if (state->consuming.type == type) {
        advance(state);
        return;
    }

    state->error = SavineError_Parse_UNEXPECTED_TOKEN;
}

static bool inline __attribute__((always_inline)) check(TreeParserState* state, TokenType type) {
    return state->consuming.type == type;
}

static bool match(TreeParserState* state, TokenType type) {
    if (!check(state, type)) {
        return false;
    }

    advance(state);
    return true;
}

static void parse_identifier(TreeParserState* state, IdentifierNode* identifier) {
    identifier->value = (char*)malloc(sizeof(char) * (state->consumed.length + 1));
    identifier->value[state->consumed.length] = 0;
    memcpy(identifier->value, state->consumed.start, state->consumed.length);
}

static void parse_integer(TreeParserState* state, IntegerNode* integer) {
    i32 num = 0;
    for (i32 i = 0; i < state->consumed.length; i++) {
        num *= 10;
        char c = state->consumed.start[i];
        i32 i = c - '0';
        num += i;
    }

    integer->value = num;
}

static void parse_expression(TreeParserState* state, ExpressionNode* expression) {
    if (match(state, TokenType_IDENTIFIER)) {
        expression->type = NodeType_IDENTIFIER;
        parse_identifier(state, &expression->identifier);
        return;
    }

    if (match(state, TokenType_INTEGER)) {
        expression->type = NodeType_INTEGER;
        parse_integer(state, &expression->integer);
        return;
    }

    state->error = SavineError_Parse_UNEXPECTED_TOKEN;
}

static void parse_assignment(TreeParserState* state, AssignmentNode* assignment) {
    consume(state, TokenType_IDENTIFIER);

    parse_identifier(state, &assignment->identifier);

    match(state, TokenType_COLIN);
    consume(state, TokenType_EQUALS);

    assignment->expression = (ExpressionNode*)malloc(sizeof(ExpressionNode));
    parse_expression(state, assignment->expression);
}

static void parse_statement(TreeParserState* state, StatementNode* statement) {
    statement->assignment = (AssignmentNode*)malloc(sizeof(AssignmentNode));
    parse_assignment(state, statement->assignment);
}

static void parse_statement_list(TreeParserState* state, StatementNode*** statement_ptr_list) {
    while (!match(state, TokenType_EOF)) {
        StatementNode* statement = (StatementNode*)malloc(sizeof(StatementNode));
        sb_push(*statement_ptr_list, statement);

        parse_statement(state, statement);
    }
}

static void parse_program(TreeParserState* state, ProgramNode* program) {
    parse_statement_list(state, &program->statement_ptr_list);
}

void savine_parse_tree(LexerState* lexer_state, AbstractSyntaxTree* tree) {
    TreeParserState state;

    state.error = SavineError_OK;
    state.lexer_state = lexer_state;
    state.tree = tree;
    state.tree->program = (ProgramNode*)malloc(sizeof(ProgramNode));
    state.tree->program->statement_ptr_list = NULL;

    advance(&state);
    parse_program(&state, state.tree->program);
}

static void print_indent(i32 level) {
    for (i32 i = 0; i < level; i++) {
        printf(" ");
    }
}

void print_program(ProgramNode* program);
void print_tree(AbstractSyntaxTree* ast) {
    if (!ast) {
        return;
    }

    print_program(ast->program);
}

void print_statement(StatementNode* statement, i32 level);
void print_program(ProgramNode* program) {
    if (!program) {
        return;
    }

    printf("PROGRAM\n");

    for (i32 i = 0; i < sb_count(program->statement_ptr_list); i++) {
        print_statement(program->statement_ptr_list[i], 1);
    }
}

void print_assignment(AssignmentNode* assignment, i32 level);
void print_statement(StatementNode* statement, i32 level) {
    if (!statement) {
        return;
    }

    print_indent(level);

    printf("STATEMENT\n");

    print_assignment(statement->assignment, level + 1);
}

void print_identifier(IdentifierNode* identifier, i32 level);
void print_expression(ExpressionNode* expression, i32 level);
void print_assignment(AssignmentNode* assignment, i32 level) {
    if (!assignment) {
        return;
    }

    print_indent(level);
    printf("ASSIGNMENT\n");

    print_identifier(&assignment->identifier, level + 1);
    print_expression(assignment->expression, level + 1);
}

void print_identifier(IdentifierNode* identifier, i32 level) {
    if (!identifier) {
        return;
    }

    print_indent(level);
    printf("IDENTIFIER: \"%s\"\n", identifier->value);
}

void print_integer(IntegerNode* integer, i32 level);
void print_expression(ExpressionNode* expression, i32 level) {
    if (!expression) {
        return;
    }

    print_indent(level);
    printf("EXPRESSION\n");

    if (expression->type == NodeType_IDENTIFIER) {
        print_identifier(&expression->identifier, level + 1);
    } else if (expression->type == NodeType_INTEGER) {
        print_integer(&expression->integer, level + 1);
    }
}

void print_integer(IntegerNode* integer, i32 level) {
    if (!integer) {
        return;
    }

    print_indent(level);
    printf("INTEGER: %d\n", integer->value);
}