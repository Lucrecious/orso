#include "static_analyzer.h"

#include <stdio.h>

#include "sb.h"
#include "mathutils.h"

typedef OrsoSymbolTable SymbolTable;

typedef struct {
    SymbolTable types;
    SymbolTable implications;
} VariableInferences;

static void init_variable_inferences(VariableInferences* variable_inferences) {
    orso_symbol_table_init(&variable_inferences->types);
    orso_symbol_table_init(&variable_inferences->implications);
}

static void free_variable_inferences(VariableInferences* variable_inferences) {
    orso_symbol_table_free(&variable_inferences->types);

    for (i32 i = 0; i < variable_inferences->implications.capacity; i++) {
        OrsoSymbolTableEntry entry = variable_inferences->implications.entries[i];
        if (entry.key == NULL) {
            continue;
        }

        orso_symbol_table_free((SymbolTable*)entry.value.as.p);
        free((SymbolTable*)entry.value.as.p);
    }
    orso_symbol_table_free(&variable_inferences->implications);
}

static void update_variable_inference(VariableInferences* variable_inferences, OrsoSymbol* variable_name, OrsoType type, SymbolTable* implications) {
    orso_symbol_table_set(&variable_inferences->types, variable_name, ORSO_SLOT_U(type.one, ORSO_TYPE_ONE(ORSO_TYPE_TYPE)));

    OrsoSlot variable_implications;
    if (!orso_symbol_table_get(&variable_inferences->implications, variable_name, &variable_implications)) {
        SymbolTable* new_implications = ORSO_ALLOCATE(SymbolTable);
        orso_symbol_table_init(new_implications);

        variable_implications.as.p = new_implications;
        orso_symbol_table_set(&variable_inferences->implications, variable_name, variable_implications);

    }

    orso_symbol_table_add_all(implications, (SymbolTable*)variable_implications.as.p);
}

static void error(OrsoStaticAnalyzer* analyzer, i32 line, const char* message) {
    if (analyzer->panic_mode) {
        return;
    }

    analyzer->panic_mode = true;
    analyzer->had_error = true;

    if (!analyzer->error_fn) {
        return;
    }

    analyzer->error_fn(ORSO_ERROR_COMPILE, line, message);
}

static void error_incompatible_binary_types(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType left, OrsoType right, i32 line) {

    const char message[256];
    char* msg = (char*)message;

    const char left_type_str[128];
    const char right_type_str[128];
    orso_type_to_cstr(left, (char*)left_type_str);
    orso_type_to_cstr(right, (char*)right_type_str);

    msg += sprintf(msg, "Incompatible Types: '%s' %.*s '%s'",
        left_type_str, operation.length, operation.start, right_type_str);

    error(analyzer, line, message);
}

static void error_incompatible_unary_type(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType operand, i32 line) {
    const char message[256];
    char* msg = (char*)message;

    const char operand_type_str[128];
    orso_type_to_cstr(operand, (char*)operand_type_str);

    msg += sprintf(msg, "Incompatible Type: unary(%.*s) and type '%s'",
        operation.length, operation.start, operand_type_str);

    error(analyzer, line, message);
}

static OrsoType orso_resolve_unary(TokenType operator, OrsoType operand) {
    if (ORSO_TYPE_IS_UNION(operand)) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (operand.one == ORSO_TYPE_INVALID) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (operand.one == ORSO_TYPE_UNRESOLVED) {
        return ORSO_TYPE_ONE(ORSO_TYPE_UNRESOLVED);
    }

    switch (operator) {
        case TOKEN_MINUS: {
            if (orso_is_number_type_kind(operand.one, false)) {
                return operand;
            } else if (operand.one == ORSO_TYPE_BOOL) {
                return ORSO_TYPE_ONE(ORSO_TYPE_INT32);
            } else {
                return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
            }
        }
        case TOKEN_NOT:
            return ORSO_TYPE_ONE(ORSO_TYPE_BOOL);
        default: return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }
}

static OrsoExpressionNode* implicit_cast(OrsoExpressionNode* operand, OrsoType value_type) {
    OrsoExpressionNode* implicit_cast = ORSO_ALLOCATE(OrsoExpressionNode);

    implicit_cast->start = operand->start;
    implicit_cast->end = operand->end;
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->narrowed_value_type = implicit_cast->value_type;
    implicit_cast->expr.cast.operand = operand;

    return implicit_cast;
}

static OrsoTypeKind resolve_type_kind_identifier(OrsoStaticAnalyzer* analyzer, Token identifier_type) {
    u32 hash = orso_hash_cstrn(identifier_type.start, identifier_type.length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(&analyzer->symbol_to_type, identifier_type.start, identifier_type.length, hash);
    if (symbol == NULL) {
        return ORSO_TYPE_INVALID;
    }

    OrsoSlot slot;
    orso_symbol_table_get(&analyzer->symbol_to_type, symbol, &slot);
    return slot.as.u;
}

// static void reconcile_type_assumptions_with_implications(OrsoStaticAnalyzer* analyzer, SymbolTable* assumptions, SymbolTable* implications) {
//     for (i32 i = 0; i < implications->capacity; i++) {
//         OrsoSymbolTableEntry entry = implications->entries[i];
//         if (entry.key == NULL) {
//             continue;
//         }

//         OrsoSlot type;
//         bool replace_assumption = false;
//         if (orso_symbol_table_get(assumptions, entry.key, &type)) {
//             OrsoSlot full_type;
//             bool found_type = orso_symbol_table_get(&analyzer->defined_variables, entry.key, &full_type);
//             // ASSERT that found_type actually found something
//             replace_assumption = orso_type_fits(ORSO_TYPE_ONE(full_type.as.u), ORSO_TYPE_ONE(type.as.u));
//         } else {
//             replace_assumption = true;
//         }

//         // ASSERT replace_assumption is true

//         orso_symbol_table_set(assumptions, entry.key, entry.value);
//     }
// }

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, VariableInferences* inferences, OrsoDeclarationNode* declaration_node);

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, VariableInferences* inferences, SymbolTable* type_implications, OrsoExpressionNode* expression) {
    if (expression->value_type.one != ORSO_TYPE_UNRESOLVED) {
        return;
    }

    SymbolTable new_type_implications;
    orso_symbol_table_init(&new_type_implications);

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(analyzer, inferences, &new_type_implications, expression->expr.grouping.expression);
            expression->value_type = expression->expr.grouping.expression->narrowed_value_type;
            expression->narrowed_value_type = expression->value_type;
            break;
        }
        case EXPRESSION_PRIMARY: {
            expression->value_type.one = ORSO_TYPE_INVALID;
            break;
        }
        case EXPRESSION_BINARY: {
            OrsoExpressionNode* left = expression->expr.binary.left;
            OrsoExpressionNode* right = expression->expr.binary.right;
            orso_resolve_expression(analyzer, inferences, &new_type_implications, left);
            orso_resolve_expression(analyzer, inferences, &new_type_implications, right);

            // TODO: Remember to do different things depending on operation
            //   if it's arthimetic, or comparisons, then let them merge at the end
            //   if it's logical, then you need to do special things

            OrsoType cast_left;
            OrsoType cast_right;

            switch (expression->expr.binary.operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    OrsoType combined_type = orso_binary_arithmetic_cast(left->narrowed_value_type, right->narrowed_value_type, expression->expr.binary.operator.type);
                    expression->value_type = combined_type;

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    orso_binary_comparison_casts(left->narrowed_value_type, right->narrowed_value_type, &cast_left, &cast_right);
                    expression->value_type.one = ORSO_TYPE_BOOL;
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    orso_binary_equality_casts(left->narrowed_value_type, right->narrowed_value_type, &cast_left, &cast_right);
                    expression->value_type.one = ORSO_TYPE_BOOL;
                    break;
                }
                default:
                    UNREACHABLE();
            }

            expression->narrowed_value_type = expression->value_type;

            if (cast_left.one == ORSO_TYPE_INVALID || cast_right.one == ORSO_TYPE_INVALID) {
                expression->value_type.one = ORSO_TYPE_INVALID;
                error_incompatible_binary_types(analyzer, expression->expr.binary.operator, left->narrowed_value_type, right->narrowed_value_type, expression->expr.binary.operator.line);
            } else {
                if (cast_left.one != left->narrowed_value_type.one) {
                    expression->expr.binary.left = implicit_cast(left, cast_left);
                }

                if (cast_right.one != right->narrowed_value_type.one) {
                    expression->expr.binary.right = implicit_cast(right, cast_right);
                }
            }
            break;
        }
        case EXPRESSION_UNARY: {
            OrsoUnaryOp* unary_op = &expression->expr.unary;
            orso_resolve_expression(analyzer, inferences, &new_type_implications, unary_op->operand);
            expression->value_type = orso_resolve_unary(unary_op->operator.type, unary_op->operand->value_type);
            expression->narrowed_value_type = expression->value_type;
            
            // TODO: Must negate the new type implications if the unary operation is NOT

            if (expression->narrowed_value_type.one == ORSO_TYPE_INVALID) {
                error_incompatible_unary_type(analyzer, unary_op->operator, unary_op->operand->narrowed_value_type, unary_op->operator.line);
            }
            break;
        }
        case EXPRESSION_VARIABLE: {
            OrsoSlot slot;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->expr.variable.name.start, expression->expr.variable.name.length, &analyzer->symbols);
            if (!orso_symbol_table_get(&analyzer->defined_variables, variable_name, &slot)) {
                error(analyzer, expression->expr.variable.name.line, "Variable does not exist.");
            } else {
                expression->value_type.one = slot.as.u;
                if (orso_symbol_table_get(&inferences->types, variable_name, &slot)) {
                    expression->narrowed_value_type.one = slot.as.u;
                } else {
                    expression->narrowed_value_type = expression->value_type;
                }

                if (orso_symbol_table_get(&inferences->implications, variable_name, &slot)) {
                    orso_symbol_table_add_all((SymbolTable*)slot.as.p, &new_type_implications);
                }
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            OrsoSlot type_slot;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->expr.assignment.variable_name.start, expression->expr.assignment.variable_name.length, &analyzer->symbols);
            bool exist = orso_symbol_table_get(&analyzer->defined_variables, variable_name, &type_slot);
            if (!exist) {
                error(analyzer, expression->start.line, "Variable does not exist.");
            } else {
                OrsoType type = ORSO_TYPE_ONE(type_slot.as.u);

                orso_resolve_expression(analyzer, inferences, &new_type_implications, expression->expr.assignment.right_side);
                expression->value_type = type;
                expression->narrowed_value_type = expression->expr.assignment.right_side->narrowed_value_type;
                
                if (!orso_type_fits(type, expression->expr.assignment.right_side->narrowed_value_type)) {
                    error(analyzer, expression->start.line, "Expression needs explicit cast to store in variable.");
                }

                if (ORSO_TYPE_IS_UNION(type)) {
                    update_variable_inference(inferences, variable_name, expression->narrowed_value_type, &new_type_implications);
                }
            }
            break;
        }

        case EXPRESSION_BLOCK: {
            i32 declarations_count = sb_count(expression->expr.block.declarations);
            for (i32 i = 0; i < declarations_count; i++) {
                OrsoDeclarationNode* declaration = expression->expr.block.declarations[i];
                resolve_declaration(analyzer, inferences, declaration);
            }

            OrsoDeclarationNode* last_expression_statement = NULL;
            if (declarations_count > 0) {
                OrsoDeclarationNode* last_declaration = expression->expr.block.declarations[declarations_count - 1];
                if (last_declaration->type == ORSO_DECLARATION_STATEMENT && last_declaration->decl.statement->type == ORSO_STATEMENT_EXPRESSION) {
                    last_expression_statement = last_declaration;
                }
            }

            if (last_expression_statement == NULL) {
                expression->value_type = ORSO_TYPE_ONE(ORSO_TYPE_NULL);
            } else {
                expression->value_type = last_expression_statement->decl.statement->stmt.expression->narrowed_value_type;
                expression->expr.block.final_expression_statement = last_expression_statement;
            }

            expression->narrowed_value_type = expression->value_type;
            break;
        }

        default: UNREACHABLE();
    }

    // TODO: Replace this with a better merge function (since types need to be merged not replaced)
    orso_symbol_table_add_all(&new_type_implications, type_implications);
    orso_symbol_table_free(&new_type_implications);
}

static void resolve_var_declaration(OrsoStaticAnalyzer* analyzer, VariableInferences* inferences, OrsoVarDeclarationNode* var_declaration) {
    if (sb_count(var_declaration->type_identifiers) >= ORSO_UNION_NUM_MAX) {
        error(analyzer, var_declaration->type_identifiers[0].line, "Orso only allows for a maximum of 4 types in a union.");
        return;
    }

    bool invalid_type_kind_exists = false;
    Token invalid_type_kind_identifier;
    OrsoType type = ORSO_TYPE_ONE(sb_count(var_declaration->type_identifiers) == 0 ? ORSO_TYPE_UNRESOLVED : ORSO_TYPE_INVALID);

    for (i32 i = 0; i < sb_count(var_declaration->type_identifiers); i++) {
        OrsoTypeKind type_kind = resolve_type_kind_identifier(analyzer, var_declaration->type_identifiers[i]);

        type.union_[i] = type_kind;

        if (type_kind != ORSO_TYPE_INVALID) {
            continue;
        }

        invalid_type_kind_exists = true;
        invalid_type_kind_identifier = var_declaration->type_identifiers[i];
        break;
    }

    if (invalid_type_kind_exists) {
        char error_message[256];
        sprintf(error_message, "Type %.*s does not exist.", invalid_type_kind_identifier.length, invalid_type_kind_identifier.start);
        error(analyzer, invalid_type_kind_identifier.line, error_message);
        return;
    }

    var_declaration->var_type = type;

    SymbolTable type_implications;
    orso_symbol_table_init(&type_implications);

    if (var_declaration->expression != NULL) {
        orso_resolve_expression(analyzer, inferences, &type_implications, var_declaration->expression);
    }

    if (ORSO_TYPE_IS_SINGLE(var_declaration->var_type)) {
        switch (var_declaration->var_type.one) {
            case ORSO_TYPE_INVALID: {
                error(analyzer, var_declaration->start.length, "Type does not exist.");
                break;
            }
            case ORSO_TYPE_UNRESOLVED: {
                ASSERT(var_declaration->expression != NULL, "this should be a parsing error.");
                if (orso_integer_fit(ORSO_TYPE_ONE(ORSO_TYPE_INT32), var_declaration->expression->value_type, false)) {
                    var_declaration->var_type.one = ORSO_TYPE_INT32;
                } else {
                    var_declaration->var_type = var_declaration->expression->narrowed_value_type;
                }
                break;
            }
            default: {
                if (var_declaration->expression != NULL && !orso_type_fits(var_declaration->var_type, var_declaration->expression->value_type)) {
                    error(analyzer, var_declaration->start.line, "Must cast expression explicitly to match var type.");
                }
                break;
            }
        }
    } else {
        if (var_declaration->expression == NULL) {
            if (!orso_type_fits(var_declaration->var_type, ORSO_TYPE_ONE(ORSO_TYPE_NULL))) {
                error(analyzer, var_declaration->start.line, "Non-void union types must have a default value.");
            } 
        } else if (!orso_type_fits(var_declaration->var_type, var_declaration->expression->value_type)) {
            error(analyzer, var_declaration->start.line, "Type mismatch between expression and declaration.");
        }
    }

    if (!analyzer->had_error) {
        OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(var_declaration->variable_name.start, var_declaration->variable_name.length, &analyzer->symbols);
        OrsoSlot slot_type;
        if (orso_symbol_table_get(&analyzer->defined_variables, identifier, &slot_type)) {
            const char message[126];
            sprintf((char*)message, "Duplicate variable definition of '%.*s'.", var_declaration->variable_name.length, var_declaration->variable_name.start);
            error(analyzer, var_declaration->start.line, (char*)message);
            return;
        }

        orso_symbol_table_set(&analyzer->defined_variables, identifier, ORSO_SLOT_U(var_declaration->var_type.one, ORSO_TYPE_ONE(ORSO_TYPE_TYPE)));

        if (ORSO_TYPE_IS_UNION(ORSO_TYPE_ONE(var_declaration->var_type.one))) {
            OrsoType narrowed_type;
            if (var_declaration->expression) {
                narrowed_type = var_declaration->expression->narrowed_value_type;
            } else {
                ASSERT(orso_type_has_kind(var_declaration->var_type, ORSO_TYPE_NULL), "must contain void type here.");
                narrowed_type = ORSO_TYPE_ONE(ORSO_TYPE_NULL);
            }

            update_variable_inference(inferences, identifier, narrowed_type, &type_implications);
        }
    }

    orso_symbol_table_free(&type_implications);
}

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, VariableInferences* inferences, OrsoDeclarationNode* declaration_node) {
    analyzer->panic_mode = false;
    switch (declaration_node->type) {
        case ORSO_DECLARATION_STATEMENT: {
            OrsoStatementNode* statement_node = declaration_node->decl.statement;
            switch (statement_node->type) {
                case ORSO_STATEMENT_PRINT_EXPR:
                case ORSO_STATEMENT_EXPRESSION: {
                    OrsoSymbolTable type_implications;
                    orso_symbol_table_init(&type_implications);

                    orso_resolve_expression(analyzer, inferences, &type_implications, declaration_node->decl.statement->stmt.expression);

                    orso_symbol_table_free(&type_implications);
                    break;
                }
                case ORSO_STATEMENT_NONE: UNREACHABLE();
            }

            analyzer->panic_mode = false;
            break;
        }
        case ORSO_DECLARATION_VAR: {
            resolve_var_declaration(analyzer, inferences, declaration_node->decl.var);
            break;
        }
        case ORSO_DECLARATION_NONE: UNREACHABLE();
    }
}

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast) {
    if (ast->declarations == NULL) {
        return true;
    }

    VariableInferences variable_inferences;
    init_variable_inferences(&variable_inferences);

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        resolve_declaration(analyzer, &variable_inferences, ast->declarations[i]);
    }

    free_variable_inferences(&variable_inferences);

    return !analyzer->had_error;
}

static void add_builtin_types(OrsoStaticAnalyzer* analyzer) {
#define ADD_TYPE(CTYPE, N, ORSO_TYPE) do { \
    OrsoSymbol* symbol_##CTYPE = orso_unmanaged_symbol_from_cstrn(#CTYPE, N, &analyzer->symbols); \
    OrsoSlot slot = { .as.i = ORSO_TYPE }; \
    orso_symbol_table_set(&analyzer->symbol_to_type, symbol_##CTYPE, slot); \
    } while (false)

    ADD_TYPE(void, 4, ORSO_TYPE_NULL);
    ADD_TYPE(bool, 4, ORSO_TYPE_BOOL);
    ADD_TYPE(i32, 3, ORSO_TYPE_INT32);
    ADD_TYPE(i64, 3, ORSO_TYPE_INT64);
    ADD_TYPE(f32, 3, ORSO_TYPE_FLOAT32);
    ADD_TYPE(f64, 3, ORSO_TYPE_FLOAT64);
    ADD_TYPE(string, 6, ORSO_TYPE_STRING);
    ADD_TYPE(symbol, 6, ORSO_TYPE_SYMBOL);

#undef ADD_TYPE
}

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn) {
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->panic_mode = false;

    orso_symbol_table_init(&analyzer->symbols);

    orso_symbol_table_init(&analyzer->symbol_to_type);
    add_builtin_types(analyzer);

    orso_symbol_table_init(&analyzer->defined_variables);

}

void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer) {
    orso_symbol_table_free(&analyzer->defined_variables);

    for (i32 i = 0; i < analyzer->symbols.capacity; i++) {
        OrsoSymbolTableEntry* entry = &analyzer->symbols.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_unmanaged_symbol_free(entry->key);
    }

    orso_symbol_table_free(&analyzer->symbol_to_type);
    orso_symbol_table_free(&analyzer->symbols);

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}
