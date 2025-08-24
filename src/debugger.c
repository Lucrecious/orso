#include "debugger.h"

#include "tmp.h"


void debugger_init(debugger_t *debugger, arena_t *allocator) {
    debugger->allocator = allocator;
    debugger->breakpoints = (breakpoints_t){.allocator=allocator};
}

static orstring_t get_input(arena_t *allocator) {
    char input[256] = {0};

    fgets(input, 256, stdin);
    // scanf("%255[^\n]", input);

    orstring_t s = cstr2string(input, allocator);
    return s;
}

orstring_t disassemble_instruction(instruction_t in, arena_t *allocator) {
    op_code_t op = (op_code_t)in.op;
    switch (op) {
        case OP_NOP: return lit2str("OP_NOP");

        #define OP_CAST(suffix) string_format("OP_CAST"#suffix"(reg_result: %c, reg_op: %c)", allocator, (oru32)in.as.casting.reg_result, (oru32)in.as.casting.reg_op);

        case OP_B2F: return OP_CAST(B2F);
        case OP_S2F: return OP_CAST(S2F);
        case OP_I2F: return OP_CAST(I2F);
        case OP_UB2F: return OP_CAST(UB2F);
        case OP_US2F: return OP_CAST(US2F);
        case OP_U2F: return OP_CAST(U2F);

        case OP_B2D: return OP_CAST(B2D);
        case OP_S2D: return OP_CAST(S2D);
        case OP_I2D: return OP_CAST(I2D);
        case OP_UB2D: return OP_CAST(UB2D);
        case OP_US2D: return OP_CAST(US2D);
        case OP_U2D: return OP_CAST(U2D);

        case OP_D2F: return OP_CAST(D2F);
        case OP_D2UL: return OP_CAST(D2UL);
        case OP_D2L: return OP_CAST(D2L);

        case OP_UL2UB: return OP_CAST(UL2UB);
        case OP_UL2US: return OP_CAST(UL2US);
        case OP_UL2U: return OP_CAST(UL2U);
        case OP_UL2L: return OP_CAST(UL2L);
        case OP_UL2F: return OP_CAST(UL2F);
        case OP_UL2D: return OP_CAST(UL2D);

        case OP_L2B: return OP_CAST(L2B);
        case OP_L2S: return OP_CAST(L2S);
        case OP_L2I: return OP_CAST(L2I);
        case OP_L2UL: return OP_CAST(L2UL);
        case OP_L2F: return OP_CAST(L2F);
        case OP_L2D: return OP_CAST(L2D);

        case OP_JMP_IF_COND:
            return string_format("OP_JMP_IF_COND(condition_reg: %c, check_for: %lu, amount: %lu)", allocator,
                    (oru32)in.as.jmp.condition_reg, (oru32)in.as.jmp.check_for, in.as.jmp.amount);

        case OP_LOAD_ADDR:
            return string_format("OP_LOAD_ADDR(reg_dest: %c, index: %d)", allocator,
                    (oru32)in.as.load_addr.reg_dest, (oru32)in.as.load_addr.memaddr);

        case OP_LOAD_REG_ADDR:
            return string_format("OP_LOAD_ADDR(reg_dest: %c, reg: %d)", allocator,
                    (oru32)in.as.load_addr.reg_dest, (oru32)in.as.load_addr.memaddr);

        case OP_JMP: return string_format("OP_JMP(amount: %lu)", allocator, (oru32)in.as.jmp.amount);
        case OP_LOOP: return string_format("OP_LOOP(amount: %lu)", allocator, (oru32)in.as.jmp.amount);

        #define OP_REG_TO_REG(SUFFIX) string_format("OP_MOV"#SUFFIX"(reg_src: %c, reg_dst: %c, offset: %d)", allocator, \
                (oru32)in.as.mov_reg_to_reg.reg_source, \
                (oru32)in.as.mov_reg_to_reg.reg_destination, \
                (oru32)in.as.mov_reg_to_reg.byte_offset);

        case OP_MOVU8_REG_TO_ADDR: return OP_REG_TO_REG(U8_REG_TO_REGADDR);
        case OP_MOVU16_REG_TO_ADDR: return OP_REG_TO_REG(U16_REG_TO_REGADDR);
        case OP_MOVU32_REG_TO_ADDR: return OP_REG_TO_REG(U32_REG_TO_REGADDR);
        case OP_MOVF32_REG_TO_ADDR: return OP_REG_TO_REG(F32_REG_TO_REGADDR);
        case OP_MOVWORD_REG_TO_ADDR: return OP_REG_TO_REG(WORD_REG_TO_REGADDR);

        case OP_MOVU8_ADDR_TO_REG: return OP_REG_TO_REG(U8_REGADDR_TO_REG);
        case OP_MOVU16_ADDR_TO_REG: return OP_REG_TO_REG(U16_REGADDR_TO_REG);
        case OP_MOVU32_ADDR_TO_REG: return OP_REG_TO_REG(U32_REGADDR_TO_REG);

        case OP_MOVS8_ADDR_TO_REG: return OP_REG_TO_REG(S8_REGADDR_TO_REG);
        case OP_MOVS16_ADDR_TO_REG: return OP_REG_TO_REG(S16_REGADDR_TO_REG);
        case OP_MOVS32_ADDR_TO_REG: return OP_REG_TO_REG(S32_REGADDR_TO_REG);

        case OP_MOVF32_ADDR_TO_REG: return OP_REG_TO_REG(F32_REGADDR_TO_REG);
        case OP_MOVWORD_ADDR_TO_REG: return OP_REG_TO_REG(WORD_REGADDR_TO_REG);

        case OP_MOV_REG_TO_REG: {
            return string_format("OP_MOV_REG_TO_REG(reg_source: %c, reg_destination: %c)", allocator,
                    (oru32)in.as.mov_reg_to_reg.reg_source,
                    (oru32)in.as.mov_reg_to_reg.reg_destination);
        }

        case OP_MEMCMP: {
            return string_format("MEMCMP(op1s: %c, op1e: %c, op2s: %c, r: %c)", allocator,
                    (oru32)in.as.memcmp.op1_start,
                    (oru32)in.as.memcmp.op1_end,
                    (oru32)in.as.memcmp.op2_start,
                    (oru32)in.as.memcmp.reg_result);
            break;
        }

        case OP_SUBU_IM: {
            return string_format("OP_SUBU_IM(reg_operand: %c, immediate: %lu, reg_result: %c)", allocator,
                    (oru32)in.as.binu_reg_immediate.reg_operand,
                    (oru32)in.as.binu_reg_immediate.immediate,
                    (oru32)in.as.binu_reg_immediate.reg_result);
        }

        case OP_ADDU_IM: {
            return string_format("OP_ADDU_IM(reg_operand: %c, immediate: %lu, reg_result: %c)", allocator,
                    (oru32)in.as.binu_reg_immediate.reg_operand,
                    (oru32)in.as.binu_reg_immediate.immediate,
                    (oru32)in.as.binu_reg_immediate.reg_result);
        }

        #define OP_BIN_REG_REG(type) string_format("OP_"#type"_REG_REG(reg_op1: %c, reg_op2: %c, reg_result: %c)", allocator,\
                (oru32)in.as.bin_reg_to_reg.reg_op1,\
                (oru32)in.as.bin_reg_to_reg.reg_op2,\
                (oru32)in.as.bin_reg_to_reg.reg_result)

        case OP_ADDI: return OP_BIN_REG_REG(ADDI);
        case OP_SUBI: return OP_BIN_REG_REG(SUBI);
        case OP_MULI: return OP_BIN_REG_REG(MULI);
        case OP_DIVI: return OP_BIN_REG_REG(DIVI);
        case OP_REMI: return OP_BIN_REG_REG(REMI);
        case OP_MODI: return OP_BIN_REG_REG(MODI);

        case OP_ADDU: return OP_BIN_REG_REG(ADDU);
        case OP_SUBU: return OP_BIN_REG_REG(SUBU);
        case OP_MULU: return OP_BIN_REG_REG(MULU);
        case OP_DIVU: return OP_BIN_REG_REG(DIVU);
        case OP_REMU: return OP_BIN_REG_REG(REMU);
        case OP_MODU: return OP_BIN_REG_REG(MODU);

        case OP_ADDD: return OP_BIN_REG_REG(ADDD);
        case OP_SUBD: return OP_BIN_REG_REG(SUBD);
        case OP_MULD: return OP_BIN_REG_REG(MULD);
        case OP_DIVD: return OP_BIN_REG_REG(DIVD);
        case OP_REMD: return OP_BIN_REG_REG(REMD);
        case OP_MODD: return OP_BIN_REG_REG(MODD);

        case OP_GTU: return OP_BIN_REG_REG(GTU);
        case OP_GEU: return OP_BIN_REG_REG(GEU);
        case OP_LTU: return OP_BIN_REG_REG(LTU);
        case OP_LEU: return OP_BIN_REG_REG(LEU);
        case OP_EQU: return OP_BIN_REG_REG(EQU);
        case OP_NQU: return OP_BIN_REG_REG(NQU);

        case OP_GTI: return OP_BIN_REG_REG(GTI);
        case OP_GEI: return OP_BIN_REG_REG(GEI);
        case OP_LTI: return OP_BIN_REG_REG(LTI);
        case OP_LEI: return OP_BIN_REG_REG(LEI);
        case OP_EQI: return OP_BIN_REG_REG(EQI);
        case OP_NQI: return OP_BIN_REG_REG(NQI);

        case OP_GTD: return OP_BIN_REG_REG(GTD);
        case OP_GED: return OP_BIN_REG_REG(GED);
        case OP_LTD: return OP_BIN_REG_REG(LTD);
        case OP_LED: return OP_BIN_REG_REG(LED);
        case OP_EQD: return OP_BIN_REG_REG(EQD);
        case OP_NQD: return OP_BIN_REG_REG(NQD);

        #undef OP_BIN_REG_REG

        #define OP_UNARY_REG_REG(type) string_format("OP_"#type"(reg_op: %c, reg_result: %c)", allocator,\
                (oru32)in.as.unary_reg_to_reg.reg_op,\
                (oru32)in.as.unary_reg_to_reg.reg_result)
        
        case OP_NOT: return OP_UNARY_REG_REG(NOT);

        case OP_NEGATEI: return OP_UNARY_REG_REG(NEGATEI);
        case OP_NEGATED: return OP_UNARY_REG_REG(NEGATED);

        #undef OP_UNARY_REG_REG

        case OP_CALL: return lit2str("OP_CALL");
        case OP_INTRINSIC_CALL: return string_format("OP_INTRINSIC(reg_arg_bot: %c, reg_op: %c, reg_result: %c, reg_result_addr: %c)", allocator,
                (oru32)in.as.call.reg_arg_bottom_memaddr,
                (oru32)in.as.call.reg_op,
                (oru32)in.as.call.reg_result,
                (oru32)in.as.call.reg_result_addr);
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

    arena_t *tmp = allocator_borrow();

    for (size_t i = low; i < high; ++i) {
        if (i == pc) {
            printf("-> ");
        }

        texloc_t location = function->locations.items[i];
        orstring_t as_string = disassemble_instruction(function->code.items[i], tmp);
        printf("%04zu:%04zu: %s\n", location.line+1, location.column+1, as_string.cstr);
    }

    allocator_return(tmp);
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
    orstring_t input = get_input(tmp);
    strings_t command_n_args = string_split(input.cstr, " \n", tmp);
    if (command_n_args.count == 0) {
        return true;
    }

    orstring_t command = command_n_args.items[0];

    if (cstr_eq(command.cstr, "quit") || cstr_eq(command.cstr, "q")) {
        return false;
    } else if (cstr_eq(command.cstr, "show")) {
        size_t amount = 0;
        if (command_n_args.count > 1) {
            orstring_t arg = command_n_args.items[1];
            amount = string2size(arg);
        }
        show_line(vm, amount);
    } else if (cstr_eq(command.cstr, "stack")) {
        bool has_stack_location = false;
        size_t stack_location = 0;
        if (command_n_args.count > 1) {
            has_stack_location = true;
            stack_location = string2size(command_n_args.items[1]);
        } 

        bool has_type = false;
        orstring_t type;
        if (command_n_args.count == 3) {
            has_type = true;
            type = command_n_args.items[2];
        } 

        if (command_n_args.count > 3) {
            println("expected 1 or 2 arguments (stack location and type)");
            return true;
        }

        void *stack_bottom = vm->registers[REG_STACK_BOTTOM].as.p;
        unless (has_stack_location) {
            void *stack_frame = vm->registers[REG_STACK_FRAME].as.p;

            size_t count = 0;
            while (stack_bottom < stack_frame) {
                oru8 m = *((oru8*)--stack_frame);
                printf("%02x ", m);
                ++count;
                if (count == 8) {
                    count = 0;
                    println("");
                }
            }
            println("");
        } else {
            void *loc = stack_bottom + stack_location;
            bool print_as_word = true;
            if (has_type) {
                print_as_word = false;
                if (string_eq(type, lit2str("f"))) {
                    orf32 f = *((orf32*)loc);
                    printf("%f\n", f);
                } else {
                    print_as_word = true;
                }
            }

            if (print_as_word) {
                orword_t reg = *((orword_t*)loc);
                printf("%02zu: %lld, %llu, %lf, %p\n", stack_location, reg.as.s, reg.as.u, reg.as.d, reg.as.p);
            }
        }

    } else if (cstr_eq(command.cstr, "breaks")) {
        printfln("breakpoint count: %zu", debugger->breakpoints.count);
        for (size_t i = 0; i < debugger->breakpoints.count; ++i) {
            source_location_t bp = debugger->breakpoints.items[i];
            printfln("breakpoint #%zu: %s:%zu", i, bp.file_path.cstr, bp.text_location.line);
        }
        println("");
    } else if (cstr_eq(command.cstr, "break")) {
        if (command_n_args.count != 2) {
            printfln("expected 1 arguments (line) but got %zu", command_n_args.count-1);
            return true;
        }

        orstring_t line_number = command_n_args.items[1];
        size_t line = string2size(line_number);

        while (try_vm_step(vm)) {
            source_location_t new_location = vm_find_source_location(vm);
            if (new_location.text_location.line == line) {
                show_line(vm, 3);
                break;
            }
        }

    } else if (cstr_eq(command.cstr, "stepo") || cstr_eq(command.cstr, "o")) {
        source_location_t bp = vm_find_source_location(vm);

        while (try_vm_step(vm)) {
            source_location_t new_location = vm_find_source_location(vm);
            if (new_location.text_location.line != bp.text_location.line || !string_eq(bp.file_path, new_location.file_path)) {
                show_line(vm, 3);
                break;
            }
        }
    } else if (cstr_eq(command.cstr, "br")) {
    } else if (cstr_eq(command.cstr, "stepi") || cstr_eq(command.cstr, "i")) { 
        if (try_vm_step(vm)) {
            show_line(vm, 3);
        }
    } else if (cstr_eq(command.cstr, "run") || cstr_eq(command.cstr, "r")) {
        while(try_vm_step(vm)) show_line(vm, 0);
    } else if (cstr_eq(command.cstr, "reg")) {
        size_t number = REGISTER_COUNT;
        if (command_n_args.count > 1) {
            orstring_t arg = command_n_args.items[1];
            number = (size_t)arg.cstr[0];
            number = number < REGISTER_COUNT ? number : REGISTER_COUNT-1;
        }

        if (number == REGISTER_COUNT) {
            for (size_t i = 0; i < REGISTER_COUNT; ++i) {
                orword_t reg = vm->registers[i];
                printf("%02zu: %lld, %llu, %lf, %p\n", i, reg.as.s, reg.as.u, reg.as.d, reg.as.p);
            }
            printf("\n");
        } else {
            orword_t reg = vm->registers[number];
            printf("%02zu: %lld, %llu, %lf, %p\n", number, reg.as.s, reg.as.u, reg.as.d, reg.as.p);
        }
    } else if (cstr_eq(command.cstr, "mem")) {
        if (command_n_args.count == 2) {
            orstring_t arg = command_n_args.items[1];
            size_t memaddr = string2size(arg);

            
            memarr_t *memarr = vm->call_frame.function->memory;
            if (memaddr + ORWORD_SIZE > memarr->count) {
                printf("memory location is too high max is: %zu\n", memarr->count - ORWORD_SIZE);
                return true;
            }

            orword_t data = *(orword_t*)(memarr->data + memaddr);
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

    texloc_t text_location = function->locations.items[index];
    orstring_t file_path = text_location.filepath;

    return (source_location_t){
        .file_path = file_path,
        .text_location = text_location,
    };
}