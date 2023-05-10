#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"
#include "type_set.h"
#include "virtual_machine.h"

OrsoFunction* orso_generate_code(OrsoVM* vm, OrsoAST* ast, OrsoTypeSet* type_set);

#endif
