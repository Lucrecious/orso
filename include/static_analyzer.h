#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"
#include "type_set.h"
#include "virtual_machine.h"

struct OrsoInterpreter;

typedef struct OrsoAnalysisDependency {
    i32 fold_level;
    ast_node_t* ast_node; 
} OrsoAnalysisDependency;

typedef struct OrsoStaticAnalyzer {
    error_function_t error_fn;
    symbol_table_t symbols;

    ast_t* ast;

    struct {
        i32 count;
        OrsoAnalysisDependency* chain;
    } dependencies;

    bool had_error;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, write_function_t write_fn, error_function_t error_fn);
void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer);

bool orso_resolve_ast(OrsoStaticAnalyzer* analyzer, ast_t* ast);

i32 orso_zero_value(ast_t* ast, type_t* type, symbol_table_t* symbol_table);

#endif
