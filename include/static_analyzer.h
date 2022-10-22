#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "abstract_syntax_tree.h"

void savine_resolve_expression(SavineExpressionNode* expression);

void savine_resolve_ast_types(SavineAST* ast);


#endif