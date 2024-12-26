#include "debugger.h"

void debugger_init(debugger_t *debugger, arena_t *allocator) {
    debugger->allocator = allocator;
    debugger->breakpoints = (breakpoints_t){.allocator=allocator};
}

static string_t get_input(arena_t *allocator) {
    char input[256] = {0};

    fgets(input, 256, stdin);
    // scanf("%255[^\n]", input);

    string_t s = cstr2string(input, allocator);
    return s;
}

static void show_line(vm_t *vm, size_t bytecode_around) {
    UNUSED(vm);
    UNUSED(bytecode_around);
}

static bool try_vm_step(vm_t *vm) {
    if (vm->halted) return false;
    vm_step(vm);
    return true;
}

bool debugger_step(debugger_t *debugger, vm_t *vm) {
    arena_t *tmp = &debugger->step_allocator;
    arena_reset(tmp);

    printf(">> ");
    string_t input = get_input(tmp);
    strings_t command_n_args = string_split(input.cstr, " \n", tmp);
    if (command_n_args.count == 0) {
        return true;
    }

    string_t command = command_n_args.items[0];
    if (cstr_eq(command.cstr, "quit") || cstr_eq(command.cstr, "q")) {
        return false;
    } else if (cstr_eq(command.cstr, "show")) {
        size_t amount = 0;
        if (command_n_args.count > 1) {
            string_t arg = command_n_args.items[1];
            amount = string2size(arg);
        }
        show_line(vm, amount);
    } else if (cstr_eq(command.cstr, "breaks")) {
        printfln("breakpoint count: %zu", debugger->breakpoints.count);
        for (size_t i = 0; i < debugger->breakpoints.count; ++i) {
            source_location_t bp = debugger->breakpoints.items[i];
            printfln("breakpoint #%zu: %s:%zu", i, bp.file_path.cstr, bp.text_location.line);
        }
        println("");
    } else if (cstr_eq(command.cstr, "break")) {
        if (command_n_args.count != 3) {
            printfln("expected 2 arguments (file and line) but got %zu", command_n_args.count-1);
            return true;
        }

        string_t file_path = string_copy(command_n_args.items[1], debugger->allocator);
        string_t line_number = command_n_args.items[2];
        size_t line = string2size(line_number);

        array_push(&debugger->breakpoints, ((source_location_t){.file_path=file_path, .text_location = texloc(line, 0) }));
    } else if (cstr_eq(command.cstr, "stepo")) {
        source_location_t bp = vm_find_source_location(vm);

        while (try_vm_step(vm)) {
            source_location_t new_location = vm_find_source_location(vm);
            if (new_location.text_location.line != bp.text_location.line || !string_eq(bp.file_path, new_location.file_path)) {
                show_line(vm, 3);
                break;
            }
        }
    } else if (cstr_eq(command.cstr, "stepi")) {
        if (try_vm_step(vm)) {
            show_line(vm, 0);
        }
    } else if (cstr_eq(command.cstr, "run")) {
        while(try_vm_step(vm));
    } else {
        printfln("unknown command: %s", command.cstr);
    }

    return true;
}

source_location_t vm_find_source_location(vm_t *vm) {
    if (vm->call_frame.function == NULL) {
        return (source_location_t){.text_location = texloc(0, 0), .file_path=lit2str("<none>")};
    }
    
    function_t *function = vm->call_frame.function;
    size_t index = vm->call_frame.ip - function->code.items;

    string_t file_path = function->file_path;
    text_location_t text_location = function->locations.items[index];

    return (source_location_t){
        .file_path = file_path,
        .text_location = text_location,
    };
}