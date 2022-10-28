#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"

typedef struct OrsoStaticAnalyzer {
    OrsoErrorFunction error_fn;
    OrsoSymbolTable symbol_to_type;
    OrsoSymbolTable* vm_symbol_table;
    bool had_error;
    bool panic_mode;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoSymbolTable* vm_symbol_table, OrsoErrorFunction error_fn);

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, OrsoExpressionNode* expression);

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast);


#endif