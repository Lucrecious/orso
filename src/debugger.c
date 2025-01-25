#include "debugger.h"

#include "tmp.h"


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

static string_t disassemble_instruction(instruction_t in, arena_t *allocator) {
    op_code_t op = (op_code_t)in.op;
    switch (op) {
        case OP_NOP: return lit2str("OP_NOP");

        #define OP_MOV_MEM_TO_REG(suffix) string_format("OP_MOV"#suffix"_MEM_TO_REG(memaddr: %lu, result_result: %lu)", allocator,\
                (u32)in.as.mov_mem_to_reg.mem_address,\
                (u32)in.as.mov_mem_to_reg.reg_result)

        case OP_MOVs8_MEM_TO_REG: return OP_MOV_MEM_TO_REG(s8);
        case OP_MOVU8_MEM_TO_REG: return OP_MOV_MEM_TO_REG(U8);

        case OP_MOVs16_MEM_TO_REG: return OP_MOV_MEM_TO_REG(s16);
        case OP_MOVU16_MEM_TO_REG: return OP_MOV_MEM_TO_REG(U16);

        case OP_MOVs32_MEM_TO_REG: return OP_MOV_MEM_TO_REG(s32);
        case OP_MOVU32_MEM_TO_REG: return OP_MOV_MEM_TO_REG(U32);

        case OP_MOVF32_MEM_TO_REG: return OP_MOV_MEM_TO_REG(F32);
        case OP_MOVWORD_MEM_TO_REG: return OP_MOV_MEM_TO_REG(WORD);

        #undef OP_MOV_MEM_TO_REG

        #define OP_CAST(suffix) string_format("OP_CAST"#suffix"(reg_result: %lu, reg_op: %lu)", allocator, (u32)in.as.casting.reg_result, (u32)in.as.casting.reg_op);

        case OP_CAST_D2F: return OP_CAST(D2F);
        case OP_CAST_D2UL: return OP_CAST(D2UL);
        case OP_CAST_D2L: return OP_CAST(D2L);

        case OP_CAST_UL2UB: return OP_CAST(UL2UB);
        case OP_CAST_UL2US: return OP_CAST(UL2US);
        case OP_CAST_UL2U: return OP_CAST(UL2U);
        case OP_CAST_UL2L: return OP_CAST(UL2L);
        case OP_CAST_UL2F: return OP_CAST(UL2F);
        case OP_CAST_UL2D: return OP_CAST(UL2D);

        case OP_CAST_L2B: return OP_CAST(L2B);
        case OP_CAST_L2S: return OP_CAST(L2S);
        case OP_CAST_L2I: return OP_CAST(L2I);
        case OP_CAST_L2UL: return OP_CAST(L2UL);
        case OP_CAST_L2F: return OP_CAST(L2F);
        case OP_CAST_L2D: return OP_CAST(L2D);

        case OP_JMP_IF_REG_CONDITION:
            return string_format("OP_JMP_IF_REG_CONDITION(condition_reg: %lu, check_for: %lu, amount: %lu)", allocator,
                    (u32)in.as.jmp.condition_reg, (u32)in.as.jmp.check_for, in.as.jmp.amount);

        case OP_JMP: return string_format("OP_JMP(amount: %lu)", allocator, (u32)in.as.jmp.amount);
        case OP_LOOP: return string_format("OP_LOOP(amount: %lu)", allocator, (u32)in.as.jmp.amount);

        case OP_MOVWORD_REG_TO_REGMEM: {
            return string_format("OP_MOVWORD_REG_TO_REGMEM(reg_source: %lu, regmem_destination: %lu)", allocator,
                    (u32)in.as.mov_reg_to_regmem.reg_source,
                    (u32)in.as.mov_reg_to_regmem.regmem_destination);
        }

        case OP_MOVWORD_REGMEM_TO_REG: {
            return string_format("OP_MOVWORD_REGMEM_TO_REG(reg_source: %lu, regmem_destination: %lu)", allocator,
                    (u32)in.as.mov_regmem_to_reg.regmem_source,
                    (u32)in.as.mov_regmem_to_reg.reg_destination);
        }

        case OP_MOV_REG_TO_REG: {
            return string_format("OP_MOV_REG_TO_REG(reg_source: %lu, reg_destination: %lu)", allocator,
                    (u32)in.as.mov_reg_to_reg.reg_source,
                    (u32)in.as.mov_reg_to_reg.reg_destination);
        }

        case OP_SUBU_REG_IM32: {
            return string_format("OP_SUBU_REG_IM32(reg_operand: %lu, immediate: %lu, reg_result: %lu)", allocator,
                    (u32)in.as.binu_reg_immediate.reg_operand,
                    (u32)in.as.binu_reg_immediate.immediate,
                    (u32)in.as.binu_reg_immediate.reg_result);
        }

        case OP_ADDU_REG_IM32: {
            return string_format("OP_ADDU_REG_IM32(reg_operand: %lu, immediate: %lu, reg_result: %lu)", allocator,
                    (u32)in.as.binu_reg_immediate.reg_operand,
                    (u32)in.as.binu_reg_immediate.immediate,
                    (u32)in.as.binu_reg_immediate.reg_result);
        }

        #define OP_BIN_REG_REG(type) string_format("OP_"#type"_REG_REG(reg_op1: %lu, reg_op2: %lu, reg_result: %lu)", allocator,\
                (u32)in.as.bin_reg_to_reg.reg_op1,\
                (u32)in.as.bin_reg_to_reg.reg_op2,\
                (u32)in.as.bin_reg_to_reg.reg_result)

        case OP_ADDI_REG_REG: return OP_BIN_REG_REG(ADDI);
        case OP_SUBI_REG_REG: return OP_BIN_REG_REG(SUBI);
        case OP_MULI_REG_REG: return OP_BIN_REG_REG(MULI);
        case OP_DIVI_REG_REG: return OP_BIN_REG_REG(DIVI);
        case OP_REMI_REG_REG: return OP_BIN_REG_REG(REMI);
        case OP_MODI_REG_REG: return OP_BIN_REG_REG(MODI);

        case OP_ADDU_REG_REG: return OP_BIN_REG_REG(ADDU);
        case OP_SUBU_REG_REG: return OP_BIN_REG_REG(SUBU);
        case OP_MULU_REG_REG: return OP_BIN_REG_REG(MULU);
        case OP_DIVU_REG_REG: return OP_BIN_REG_REG(DIVU);
        case OP_REMU_REG_REG: return OP_BIN_REG_REG(REMU);
        case OP_MODU_REG_REG: return OP_BIN_REG_REG(MODU);

        case OP_ADDD_REG_REG: return OP_BIN_REG_REG(ADDD);
        case OP_SUBD_REG_REG: return OP_BIN_REG_REG(SUBD);
        case OP_MULD_REG_REG: return OP_BIN_REG_REG(MULD);
        case OP_DIVD_REG_REG: return OP_BIN_REG_REG(DIVD);
        case OP_REMD_REG_REG: return OP_BIN_REG_REG(REMD);
        case OP_MODD_REG_REG: return OP_BIN_REG_REG(MODD);

        case OP_GTU_REG_REG: return OP_BIN_REG_REG(GTU);
        case OP_GEU_REG_REG: return OP_BIN_REG_REG(GEU);
        case OP_LTU_REG_REG: return OP_BIN_REG_REG(LTU);
        case OP_LEU_REG_REG: return OP_BIN_REG_REG(LEU);
        case OP_EQU_REG_REG: return OP_BIN_REG_REG(EQU);
        case OP_NQU_REG_REG: return OP_BIN_REG_REG(NQU);

        case OP_GTI_REG_REG: return OP_BIN_REG_REG(GTI);
        case OP_GEI_REG_REG: return OP_BIN_REG_REG(GEI);
        case OP_LTI_REG_REG: return OP_BIN_REG_REG(LTI);
        case OP_LEI_REG_REG: return OP_BIN_REG_REG(LEI);
        case OP_EQI_REG_REG: return OP_BIN_REG_REG(EQI);
        case OP_NQI_REG_REG: return OP_BIN_REG_REG(NQI);

        case OP_GTD_REG_REG: return OP_BIN_REG_REG(GTD);
        case OP_GED_REG_REG: return OP_BIN_REG_REG(GED);
        case OP_LTD_REG_REG: return OP_BIN_REG_REG(LTD);
        case OP_LED_REG_REG: return OP_BIN_REG_REG(LED);
        case OP_EQD_REG_REG: return OP_BIN_REG_REG(EQD);
        case OP_NQD_REG_REG: return OP_BIN_REG_REG(NQD);

        #undef OP_BIN_REG_REG

        #define OP_UNARY_REG_REG(type) string_format("OP_"#type"_REG_REG(reg_op: %lu, reg_result: %lu)", allocator,\
                (u32)in.as.unary_reg_to_reg.reg_op,\
                (u32)in.as.unary_reg_to_reg.reg_result)
        
        case OP_NOT: return OP_UNARY_REG_REG(NOT);

        case OP_NEGATEI: return OP_UNARY_REG_REG(NEGATEI);
        case OP_NEGATED: return OP_UNARY_REG_REG(NEGATED);

        case OP_INCREMENTD: return OP_UNARY_REG_REG(INCREMENTD);
        case OP_DECREMENTD: return OP_UNARY_REG_REG(DECREMENTD);
        case OP_INCREMENTI: return OP_UNARY_REG_REG(INCREMENTI);
        case OP_DECREMENTI: return OP_UNARY_REG_REG(DECREMENTI);
        case OP_INCREMENTU: return OP_UNARY_REG_REG(INCREMENTU);
        case OP_DECREMENTU: return OP_UNARY_REG_REG(DECREMENTU);
        
        #undef OP_UNARY_REG_REG

        case OP_CALL: return lit2str("OP_CALL");
        case OP_RETURN: return lit2str("OP_RETURN");
    }
}

static void show_line(vm_t *vm, size_t bytecode_around) {
    if (vm->halted) {
        printf("<no source to show>\n");
        return;
    }

    function_t *function = vm->call_frame.function;
    size_t pc = vm->call_frame.pc;

    size_t low = bytecode_around > pc ? 0 : (pc - bytecode_around);
    size_t high = (pc + bytecode_around) > function->code.count ? function->code.count : (pc + bytecode_around);

    if (low == high && high < function->code.count) ++high;

    tmp_arena_t *tmp_arena = allocator_borrow();

    for (size_t i = low; i < high; ++i) {
        if (i == pc) {
            printf("-> ");
        }

        texloc_t location = function->locations.items[i];
        string_t as_string = disassemble_instruction(function->code.items[i], tmp_arena->allocator);
        printf("%04zu:%04zu:%04zu: %s\n", i, location.line+1, location.column+1, as_string.cstr);
    }

    allocator_return(tmp_arena);
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

        array_push(&debugger->breakpoints, ((source_location_t){.file_path=file_path, .text_location = texloc(file_path, line, 0) }));
    } else if (cstr_eq(command.cstr, "stepo") || cstr_eq(command.cstr, "o")) {
        source_location_t bp = vm_find_source_location(vm);

        while (try_vm_step(vm)) {
            source_location_t new_location = vm_find_source_location(vm);
            if (new_location.text_location.line != bp.text_location.line || !string_eq(bp.file_path, new_location.file_path)) {
                show_line(vm, 3);
                break;
            }
        }
    } else if (cstr_eq(command.cstr, "stepi") || cstr_eq(command.cstr, "i")) {
        if (try_vm_step(vm)) {
            show_line(vm, 3);
        }
    } else if (cstr_eq(command.cstr, "run") || cstr_eq(command.cstr, "r")) {
        while(try_vm_step(vm));
    } else if (cstr_eq(command.cstr, "reg")) {
        size_t number = REGISTER_COUNT;
        if (command_n_args.count > 1) {
            string_t arg = command_n_args.items[1];
            number = string2size(arg);
            number = number < REGISTER_COUNT ? number : REGISTER_COUNT-1;
        }

        if (number == REGISTER_COUNT) {
            for (size_t i = 0; i < REGISTER_COUNT; ++i) {
                word_t reg = vm->registers[i];
                printf("%02zu: %lld, %llu, %lf, %p\n", i, reg.as.s, reg.as.u, reg.as.d, reg.as.p);
            }
            printf("\n");
        } else {
            word_t reg = vm->registers[number];
            printf("%02zu: %lld, %llu, %lf, %p\n", number, reg.as.s, reg.as.u, reg.as.d, reg.as.p);
        }
    } else if (cstr_eq(command.cstr, "mem")) {
        if (command_n_args.count == 2) {
            string_t arg = command_n_args.items[1];
            memaddr_t memaddr = string2size(arg);

            
            memarr_t *memarr = vm->call_frame.function->memory;
            if (memaddr + WORD_SIZE > memarr->count) {
                printf("memory location is too high max is: %zu\n", memarr->count - WORD_SIZE);
                return true;
            }

            word_t data = *(word_t*)(memarr->data + memaddr);
            printf("mem: %llx\n", data.as.u);
        }
    } else {
        printfln("unknown command: %s", command.cstr);
    }

    return true;
}

source_location_t vm_find_source_location(vm_t *vm) {
    if (vm->call_frame.function == NULL) {
        return (source_location_t){.text_location = texloc(lit2str("<nofile>"), 0, 0), .file_path=lit2str("<nofile>")};
    }
    
    function_t *function = vm->call_frame.function;
    size_t index = vm->call_frame.pc;

    string_t file_path = function->file_path;
    texloc_t text_location = function->locations.items[index];

    return (source_location_t){
        .file_path = file_path,
        .text_location = text_location,
    };
}