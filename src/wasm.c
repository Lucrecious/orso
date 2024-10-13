#include <emscripten/emscripten.h>

#include "interpreter.h"

EMSCRIPTEN_KEEPALIVE
void run_source(char* source, write_function_t write_fn, OrsoErrorFunction error_fn) {
    vm_t vm;
    vm_init(&vm, write_fn, 1000000);

    orso_run_source(*vm, source, error_fn);

    vm_free(&vm);
}

int main(int argc, char** argsc) {

}
