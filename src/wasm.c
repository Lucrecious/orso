#include <emscripten/emscripten.h>

#include "interpreter.h"

EMSCRIPTEN_KEEPALIVE
void vm_run_source(char* source, write_function_t write_fn, error_function_t error_fn) {
    vm_t vm;
    vm_init(&vm, write_fn, 1000000);

    vm_run_source(*vm, source, error_fn);

    vm_free(&vm);
}

int main(int argc, char** argsc) {

}
