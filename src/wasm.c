#include <emscripten/emscripten.h>

#include "interpreter.h"

EMSCRIPTEN_KEEPALIVE
void run_source(char* source, OrsoWriteFunction write_fn, OrsoErrorFunction error_fn) {
    OrsoVM vm;
    orso_vm_init(&vm, write_fn);

    orso_run_source(*vm, source, error_fn);

    orso_vm_free(&vm);
}

int main(int argc, char** argsc) {

}
