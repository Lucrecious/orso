#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"
#include "type_set.h"
#include "virtual_machine.h"

typedef struct OrsoScope {
    OrsoExpressionNode* creator;
    OrsoSymbolTable named_entities;
    struct OrsoScope* outer;
} OrsoScope;

struct OrsoInterpreter;

typedef struct OrsoStaticAnalyzer {
    OrsoErrorFunction error_fn;
    OrsoSymbolTable symbols;

    bool had_error;
    bool panic_mode;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoWriteFunction write_fn, OrsoErrorFunction error_fn);
void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer);

bool orso_resolve_ast(OrsoStaticAnalyzer* analyzer, OrsoAST* ast);

OrsoSlot orso_zero_value(OrsoType* type, OrsoSymbolTable* symbol_table);

#endif
