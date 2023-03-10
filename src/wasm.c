#include <emscripten/emscripten.h>

#include "interpreter.h"

EMSCRIPTEN_KEEPALIVE
uint8_t* interpreter_new(OrsoWriteFunction write_fn, OrsoErrorFunction error_fn) {
    OrsoInterpreter* interpreter = ORSO_ALLOCATE(OrsoInterpreter);
    orso_interpreter_init(interpreter, write_fn, error_fn);
    
    return (uint8_t*)interpreter;
}

EMSCRIPTEN_KEEPALIVE
void interpreter_run(uint8_t* interpreter, char* source) {
    orso_interpreter_run((OrsoInterpreter*)interpreter, source);
}

EMSCRIPTEN_KEEPALIVE
void interpreter_free(uint8_t* interpreter) {
    orso_interpreter_free((OrsoInterpreter*)interpreter);
    free((OrsoInterpreter*)interpreter);
}

int main(int argc, char** argsc) {

}
