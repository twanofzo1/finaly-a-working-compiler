#include "codegen.hpp"
#include <fstream>
#include <algorithm>
#include <cmath>
#include <unordered_set>



/// @brief constructs the code generator from an IR program
Codegen::Codegen(const IR_Program& program)
    : m_program(program), m_stack_size(0), m_next_offset(0), m_float_label_counter(0), m_string_label_counter(0) {}


/// @brief generates the full x86-64 assembly as a string
/// @return the assembly source text
std::string Codegen::generate() {
    emit_program();
    return m_out.str();
}

/// @brief generates assembly and writes it to a file
/// @param path  output file path
void Codegen::write(const std::string& path) {
    std::string asm_code = generate();
    std::ofstream file(path);
    file << asm_code;
    file.close();
}




/// @brief emits a single indented assembly line
void Codegen::emit(const std::string& line) {
    m_out << "    " << line << "\n";
}

/// @brief emits a label on its own line
void Codegen::emit_label(const std::string& label) {
    m_out << label << ":\n";
}

/// @brief emits a comment line for debug readability in the output
void Codegen::emit_comment(const std::string& comment) {
    m_out << "    # " << comment << "\n";
}




/// @brief mangles a function name for the target platform (no-op on Linux)
std::string Codegen::mangle(const std::string& name) const {
    // Linux: no prefix. macOS would use underscore prefix.
    return name;
}




/// @brief returns the byte size of an IR type (1, 2, 4, or 8)
u32 Codegen::type_size(const IR_Type& type) const {
    if (type.kind == Datatype_kind::Void) return 0;
    if (type.kind == Datatype_kind::Bool) return 1;
    if (type.kind == Datatype_kind::Float) {
        return (type.bit_width <= 32) ? 4 : 8;
    }
    if (type.bit_width <= 8)  return 1;
    if (type.bit_width <= 16) return 2;
    if (type.bit_width <= 32) return 4;
    return 8;
}

/// @brief returns the AT&T size suffix for a given byte count (b, w, l, q)
std::string Codegen::size_suffix(u32 bytes) const {
    switch (bytes) {
        case 1: return "b";
        case 2: return "w";
        case 4: return "l";
        case 8: return "q";
        default: return "l";
    }
}

/// @brief returns the correctly sized register name for a given base and byte count
/// @param base   short name like "a", "b", "c", "d", "si", "di", "8", "9"
/// @param bytes  1, 2, 4, or 8
std::string Codegen::sized_reg(const std::string& base, u32 bytes) const {
    // base is one of: "a", "b", "c", "d", "si", "di"
    // Returns the register name for the given size

    if (base == "a") {
        switch (bytes) { case 1: return "%al"; case 2: return "%ax"; case 4: return "%eax"; default: return "%rax"; }
    } else if (base == "b") {
        switch (bytes) { case 1: return "%bl"; case 2: return "%bx"; case 4: return "%ebx"; default: return "%rbx"; }
    } else if (base == "c") {
        switch (bytes) { case 1: return "%cl"; case 2: return "%cx"; case 4: return "%ecx"; default: return "%rcx"; }
    } else if (base == "d") {
        switch (bytes) { case 1: return "%dl"; case 2: return "%dx"; case 4: return "%edx"; default: return "%rdx"; }
    } else if (base == "si") {
        switch (bytes) { case 1: return "%sil"; case 2: return "%si"; case 4: return "%esi"; default: return "%rsi"; }
    } else if (base == "di") {
        switch (bytes) { case 1: return "%dil"; case 2: return "%di"; case 4: return "%edi"; default: return "%rdi"; }
    } else if (base == "8") {
        switch (bytes) { case 1: return "%r8b"; case 2: return "%r8w"; case 4: return "%r8d"; default: return "%r8"; }
    } else if (base == "9") {
        switch (bytes) { case 1: return "%r9b"; case 2: return "%r9w"; case 4: return "%r9d"; default: return "%r9"; }
    }
    return "%rax";
}




/// @brief allocates a stack slot for a virtual register and returns the rbp offset
i32 Codegen::alloc_slot(IR_Reg reg, u32 size_bytes) {
    if (size_bytes < 8) size_bytes = 8; // align to 8 bytes minimum
    m_next_offset -= size_bytes;
    m_stack_offsets[reg] = m_next_offset;
    return m_next_offset;
}

/// @brief returns the rbp-relative offset of a virtual register's stack slot
i32 Codegen::slot_of(IR_Reg reg) const {
    auto it = m_stack_offsets.find(reg);
    ASSERT(it != m_stack_offsets.end(), "register %" << reg << " has no stack slot");
    return it->second;
}

/// @brief loads a virtual register's value from the stack into a hardware register
void Codegen::load_reg(IR_Reg vreg, const std::string& hw_reg) {
    i32 offset = slot_of(vreg);
    emit("movq " + std::to_string(offset) + "(%rbp), " + hw_reg);
}

/// @brief stores a hardware register's value into a virtual register's stack slot
void Codegen::store_reg(const std::string& hw_reg, IR_Reg vreg) {
    i32 offset = slot_of(vreg);
    emit("movq " + hw_reg + ", " + std::to_string(offset) + "(%rbp)");
}




/// @brief loads a float value from the stack into an XMM register
void Codegen::load_xmm(IR_Reg vreg, const std::string& xmm_reg) {
    i32 offset = slot_of(vreg);
    emit("movsd " + std::to_string(offset) + "(%rbp), " + xmm_reg);
}

/// @brief stores an XMM register's value into a virtual register's stack slot
void Codegen::store_xmm(const std::string& xmm_reg, IR_Reg vreg) {
    i32 offset = slot_of(vreg);
    emit("movsd " + xmm_reg + ", " + std::to_string(offset) + "(%rbp)");
}




/// @brief adds a float constant to the pool and returns its label id
u32 Codegen::add_float_constant(double value) {
    u32 id = m_float_label_counter++;
    m_float_pool.push_back({id, value});
    return id;
}

/// @brief emits the float constant pool (.rodata section with .quad entries)
void Codegen::emit_float_constants() {
    if (m_float_pool.empty()) return;

    m_out << "\n    .section .rodata\n";
    for (const auto& entry : m_float_pool) {
        m_out << "    .align 8\n";
        m_out << ".LC" << entry.label_id << ":\n";
        // Emit the double as a .quad (raw 64-bit representation)
        union { double d; uint64_t u; } conv;
        conv.d = entry.value;
        m_out << "    .quad " << conv.u << "\n";
    }
}

/// @brief adds a string constant to the pool and returns its label id
u32 Codegen::add_string_constant(const std::string& value) {
    u32 id = m_string_label_counter++;
    m_string_pool.push_back({id, value});
    return id;
}

/// @brief emits the string constant pool (.rodata section with .string entries)
void Codegen::emit_string_constants() {
    if (m_string_pool.empty()) return;

    m_out << "\n    .section .rodata\n";
    for (const auto& entry : m_string_pool) {
        m_out << ".LCS" << entry.label_id << ":\n";
        m_out << "    .string \"" << entry.value << "\"\n";
    }
}




/// @brief checks if an IR type is a floating-point type
bool Codegen::is_float_type(const IR_Type& type) const {
    return type.kind == Datatype_kind::Float;
}

/// @brief checks if a virtual register holds a float value
bool Codegen::is_float_reg(IR_Reg reg) const {
    auto it = m_reg_types.find(reg);
    if (it != m_reg_types.end()) {
        return is_float_type(it->second);
    }
    return false;
}




/// @brief emits the full program: .data globals, .text functions, .rodata float pool
void Codegen::emit_program() {
    // Emit global variables in .data section
    if (!m_program.globals.empty()) {
        m_out << "    .data\n";
        for (const auto& g : m_program.globals) {
            m_out << "    .globl " << g.name << "\n";
            if (g.is_const) {
                m_out << "    .section .rodata\n";
            }
            emit_label(g.name);
            if (g.has_string_init) {
                // Emit a pointer to a string constant in .rodata
                u32 lbl = add_string_constant(g.string_init);
                m_out << "    .quad .LCS" << lbl << "\n";
            } else if (g.has_float_init) {
                // Emit 64-bit float as .quad with raw bit pattern
                union { double d; u64 u; } conv;
                conv.d = g.float_init;
                m_out << "    .quad " << conv.u << "    # double " << g.float_init << "\n";
            } else {
                u32 size = 8;
                if (g.type.kind != Datatype_kind::Float) {
                    if (g.type.bit_width <= 8)       size = 1;
                    else if (g.type.bit_width <= 16) size = 2;
                    else if (g.type.bit_width <= 32) size = 4;
                    else                             size = 8;
                }
                switch (size) {
                    case 1: m_out << "    .byte "  << g.int_init << "\n"; break;
                    case 2: m_out << "    .short " << g.int_init << "\n"; break;
                    case 4: m_out << "    .long "  << g.int_init << "\n"; break;
                    case 8: m_out << "    .quad "  << g.int_init << "\n"; break;
                }
            }
            if (g.is_const) {
                m_out << "    .data\n"; // return to .data after .rodata
            }
        }
    }

    m_out << "    .text\n";

    for (const auto& func : m_program.functions) {
        emit_function(func);
    }

    // Emit float constant pool in .rodata
    emit_float_constants();

    // Emit string constant pool in .rodata
    emit_string_constants();
}




/// @brief emits a single function: prologue, params, instructions, epilogue
void Codegen::emit_function(const IR_Function& func) {
    // Reset per-function state
    m_stack_offsets.clear();
    m_reg_types.clear();
    m_next_offset = 0;

    std::string fname = mangle(func.name);

    // ── Pre-scan: allocate stack slots and collect register types ──
    for (const auto& inst : func.instructions) {
        // Track register types from instructions that define them
        if (inst.dst != IR_REG_NONE && inst.type.kind != Datatype_kind::Void) {
            m_reg_types[inst.dst] = inst.type;
        }

        if (inst.dst != IR_REG_NONE &&
            m_stack_offsets.find(inst.dst) == m_stack_offsets.end()) {
            alloc_slot(inst.dst, 8);
        }
        if (inst.src1 != IR_REG_NONE &&
            m_stack_offsets.find(inst.src1) == m_stack_offsets.end()) {
            alloc_slot(inst.src1, 8);
        }
        if (inst.src2 != IR_REG_NONE &&
            m_stack_offsets.find(inst.src2) == m_stack_offsets.end()) {
            alloc_slot(inst.src2, 8);
        }
        for (auto arg_reg : inst.args) {
            if (arg_reg != IR_REG_NONE &&
                m_stack_offsets.find(arg_reg) == m_stack_offsets.end()) {
                alloc_slot(arg_reg, 8);
            }
        }
    }

    // Round stack size up to 16-byte alignment
    m_stack_size = ((-m_next_offset) + 15) & ~15;

    // ── Function header ──
    m_out << "\n";
    m_out << "    .globl " << fname << "\n";
    m_out << "    .type " << fname << ", @function\n";
    emit_label(fname);

    // Prologue
    emit("pushq %rbp");
    emit("movq %rsp, %rbp");
    if (m_stack_size > 0)
        emit("subq $" + std::to_string(m_stack_size) + ", %rsp");

    // ── Store incoming parameters into their stack slots ──
    // System V ABI: int params in %rdi,%rsi,%rdx,%rcx,%r8,%r9
    //               float params in %xmm0-%xmm7
    {
        static const std::string int_arg_regs[] = {
            "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"
        };
        static const std::string float_arg_regs[] = {
            "%xmm0", "%xmm1", "%xmm2", "%xmm3",
            "%xmm4", "%xmm5", "%xmm6", "%xmm7"
        };

        // Build set of all registers that appear as dst in some instruction
        std::unordered_set<IR_Reg> defined_regs;
        for (const auto& inst : func.instructions) {
            if (inst.dst != IR_REG_NONE)
                defined_regs.insert(inst.dst);
        }

        // Find param value registers: used as src1 in Store, but never defined
        u64 param_idx = 0;
        u64 int_param_idx = 0;
        u64 float_param_idx = 0;
        for (const auto& inst : func.instructions) {
            if (inst.op == IR_Op::Store &&
                inst.src1 != IR_REG_NONE &&
                defined_regs.find(inst.src1) == defined_regs.end() &&
                param_idx < func.params.size()) {

                bool is_float = is_float_type(func.params[param_idx].second);

                if (is_float && float_param_idx < 8) {
                    emit_comment("float param " + std::to_string(param_idx) + " -> %" + std::to_string(inst.src1));
                    // Store the XMM register value to the param's stack slot
                    store_xmm(float_arg_regs[float_param_idx], inst.src1);
                    m_reg_types[inst.src1] = func.params[param_idx].second;
                    ++float_param_idx;
                } else if (!is_float && int_param_idx < 6) {
                    emit_comment("param " + std::to_string(param_idx) + " -> %" + std::to_string(inst.src1));
                    store_reg(int_arg_regs[int_param_idx], inst.src1);
                    ++int_param_idx;
                }
                ++param_idx;
            }
        }
    }

    // ── Emit instructions ──
    for (const auto& inst : func.instructions) {
        emit_instruction(inst);
    }

    // Safety: if function falls through without ret, emit one
    emit("leave");
    emit("ret");

    m_out << "    .size " << fname << ", .-" << fname << "\n";
}




/// @brief emits x86-64 assembly for a single IR instruction
void Codegen::emit_instruction(const IR_Instruction& inst) {
    switch (inst.op) {

    // ── Const ──
    case IR_Op::Const_int: {
        emit_comment("const " + std::to_string(inst.imm) + " -> %" + std::to_string(inst.dst));
        emit("movq $" + std::to_string(inst.imm) + ", %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Const_float: {
        emit_comment("const_float " + std::to_string(inst.fimm) + " -> %" + std::to_string(inst.dst));
        u32 lbl = add_float_constant(inst.fimm);
        emit("movsd .LC" + std::to_string(lbl) + "(%rip), %xmm0");
        store_xmm("%xmm0", inst.dst);
        break;
    }

    case IR_Op::Const_string: {
        emit_comment("const_string \"" + inst.str_value + "\" -> %" + std::to_string(inst.dst));
        u32 lbl = add_string_constant(inst.str_value);
        emit("leaq .LCS" + std::to_string(lbl) + "(%rip), %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    // ── Alloca ──
    case IR_Op::Alloca: {
        emit_comment("alloca -> %" + std::to_string(inst.dst));
        // The slot is already allocated during pre-scan.
        // Nothing to emit — the slot is just a reserved stack location.
        break;
    }

    // ── Load ──
    case IR_Op::Load: {
        emit_comment("load %" + std::to_string(inst.src1) + " -> %" + std::to_string(inst.dst));
        i32 src_off = slot_of(inst.src1);
        if (is_float_type(inst.type)) {
            emit("movsd " + std::to_string(src_off) + "(%rbp), %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            emit("movq " + std::to_string(src_off) + "(%rbp), %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    // ── Store ──
    case IR_Op::Store: {
        emit_comment("store %" + std::to_string(inst.src1) + " -> %" + std::to_string(inst.src2));
        if (is_float_reg(inst.src1)) {
            load_xmm(inst.src1, "%xmm0");
            i32 dst_off = slot_of(inst.src2);
            emit("movsd %xmm0, " + std::to_string(dst_off) + "(%rbp)");
        } else {
            load_reg(inst.src1, "%rax");
            i32 dst_off = slot_of(inst.src2);
            emit("movq %rax, " + std::to_string(dst_off) + "(%rbp)");
        }
        break;
    }

    // ── Binary arithmetic ──
    case IR_Op::Add: {
        emit_comment("add");
        if (is_float_type(inst.type)) {
            load_xmm(inst.src1, "%xmm0");
            load_xmm(inst.src2, "%xmm1");
            emit("addsd %xmm1, %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            load_reg(inst.src2, "%rcx");
            emit("addq %rcx, %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    case IR_Op::Sub: {
        emit_comment("sub");
        if (is_float_type(inst.type)) {
            load_xmm(inst.src1, "%xmm0");
            load_xmm(inst.src2, "%xmm1");
            emit("subsd %xmm1, %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            load_reg(inst.src2, "%rcx");
            emit("subq %rcx, %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    case IR_Op::Mul: {
        emit_comment("mul");
        if (is_float_type(inst.type)) {
            load_xmm(inst.src1, "%xmm0");
            load_xmm(inst.src2, "%xmm1");
            emit("mulsd %xmm1, %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            load_reg(inst.src2, "%rcx");
            emit("imulq %rcx, %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    case IR_Op::Div: {
        emit_comment("div");
        if (is_float_type(inst.type)) {
            load_xmm(inst.src1, "%xmm0");
            load_xmm(inst.src2, "%xmm1");
            emit("divsd %xmm1, %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            emit("cqto");
            load_reg(inst.src2, "%rcx");
            emit("idivq %rcx");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    case IR_Op::Mod: {
        emit_comment("mod");
        load_reg(inst.src1, "%rax");
        emit("cqto");
        load_reg(inst.src2, "%rcx");
        emit("idivq %rcx");
        store_reg("%rdx", inst.dst);     // remainder is in rdx
        break;
    }

    // ── Unary ──
    case IR_Op::Neg: {
        emit_comment("neg");
        if (is_float_type(inst.type)) {
            // Negate by XOR-ing the sign bit (bit 63)
            // Load the value, move to GP reg, flip sign bit, move back
            load_xmm(inst.src1, "%xmm0");
            // Create -0.0 constant (sign bit mask) and XOR
            u32 lbl = add_float_constant(-0.0);
            emit("movsd .LC" + std::to_string(lbl) + "(%rip), %xmm1");
            emit("xorpd %xmm1, %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            emit("negq %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    case IR_Op::Bitwise_not: {
        emit_comment("bitwise not");
        load_reg(inst.src1, "%rax");
        emit("notq %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Logical_not: {
        emit_comment("logical not");
        load_reg(inst.src1, "%rax");
        emit("testq %rax, %rax");
        emit("sete %al");
        emit("movzbq %al, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    // ── Comparisons ──
    case IR_Op::Eq: case IR_Op::Neq:
    case IR_Op::Lt: case IR_Op::Gt:
    case IR_Op::Le: case IR_Op::Ge: {
        emit_comment("compare");

        // Check if operands are float by checking src1's type
        if (is_float_reg(inst.src1)) {
            load_xmm(inst.src1, "%xmm0");
            load_xmm(inst.src2, "%xmm1");
            emit("ucomisd %xmm1, %xmm0");

            // ucomisd sets CF and ZF (unsigned comparison flags)
            std::string set_inst;
            switch (inst.op) {
                case IR_Op::Eq:  set_inst = "sete";  break;  // ZF=1
                case IR_Op::Neq: set_inst = "setne"; break;  // ZF=0
                case IR_Op::Lt:  set_inst = "setb";  break;  // CF=1
                case IR_Op::Gt:  set_inst = "seta";  break;  // CF=0 && ZF=0
                case IR_Op::Le:  set_inst = "setbe"; break;  // CF=1 || ZF=1
                case IR_Op::Ge:  set_inst = "setae"; break;  // CF=0
                default: set_inst = "sete"; break;
            }
            emit(set_inst + " %al");
            emit("movzbq %al, %rax");
            store_reg("%rax", inst.dst);
        } else {
            load_reg(inst.src1, "%rax");
            load_reg(inst.src2, "%rcx");
            emit("cmpq %rcx, %rax");

            std::string set_inst;
            switch (inst.op) {
                case IR_Op::Eq:  set_inst = "sete";  break;
                case IR_Op::Neq: set_inst = "setne"; break;
                case IR_Op::Lt:  set_inst = "setl";  break;
                case IR_Op::Gt:  set_inst = "setg";  break;
                case IR_Op::Le:  set_inst = "setle"; break;
                case IR_Op::Ge:  set_inst = "setge"; break;
                default: set_inst = "sete"; break;
            }
            emit(set_inst + " %al");
            emit("movzbq %al, %rax");
            store_reg("%rax", inst.dst);
        }
        break;
    }

    // ── Bitwise binary ──
    case IR_Op::And: {
        emit_comment("bitwise and");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("andq %rcx, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Or: {
        emit_comment("bitwise or");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("orq %rcx, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Xor: {
        emit_comment("xor");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("xorq %rcx, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Shl: {
        emit_comment("shift left");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("shlq %cl, %rax");         // shift amount must be in %cl
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Shr: {
        emit_comment("shift right (arithmetic)");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("sarq %cl, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    // ── Logical binary ──
    case IR_Op::Logical_and: {
        emit_comment("logical and");
        load_reg(inst.src1, "%rax");
        emit("testq %rax, %rax");
        emit("setne %al");
        emit("movzbq %al, %rax");
        load_reg(inst.src2, "%rcx");
        emit("testq %rcx, %rcx");
        emit("setne %cl");
        emit("movzbq %cl, %rcx");
        emit("andq %rcx, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    case IR_Op::Logical_or: {
        emit_comment("logical or");
        load_reg(inst.src1, "%rax");
        load_reg(inst.src2, "%rcx");
        emit("orq %rcx, %rax");
        emit("testq %rax, %rax");
        emit("setne %al");
        emit("movzbq %al, %rax");
        store_reg("%rax", inst.dst);
        break;
    }

    // ── Labels ──
    case IR_Op::Label: {
        emit_label(".L" + std::to_string(inst.label_id));
        break;
    }

    // ── Jump ──
    case IR_Op::Jump: {
        emit("jmp .L" + std::to_string(inst.label_id));
        break;
    }

    // ── Branch ──
    case IR_Op::Branch: {
        emit_comment("branch");
        load_reg(inst.src1, "%rax");
        emit("testq %rax, %rax");
        emit("jne .L" + std::to_string(inst.true_label));
        emit("jmp .L" + std::to_string(inst.false_label));
        break;
    }

    // ── Return ──
    case IR_Op::Ret: {
        emit_comment("return");
        if (inst.src1 != IR_REG_NONE) {
            if (is_float_reg(inst.src1)) {
                load_xmm(inst.src1, "%xmm0");  // float return in %xmm0
            } else {
                load_reg(inst.src1, "%rax");    // int return in %rax
            }
        } else {
            emit("xorq %rax, %rax");     // return 0 / void
        }
        emit("leave");
        emit("ret");
        break;
    }

    // ── Call ──
    case IR_Op::Call: {
        emit_comment("call " + inst.func_name);

        // ── Built-in: print ──
        if (inst.func_name == "print" && inst.args.size() == 1) {
            IR_Reg arg = inst.args[0];
            auto it = m_reg_types.find(arg);

            if (it != m_reg_types.end() && it->second.kind == Datatype_kind::Float) {
                // Float: printf("%f\n", value)
                u32 fmt_lbl = add_string_constant("%f\\n");
                load_xmm(arg, "%xmm0");
                emit("leaq .LCS" + std::to_string(fmt_lbl) + "(%rip), %rdi");
                emit("movq $1, %rax");  // 1 vector register used (variadic ABI)
                emit("call printf");
            } else if (it != m_reg_types.end() && it->second.kind == Datatype_kind::String) {
                // String: puts(str)
                load_reg(arg, "%rdi");
                emit("call puts");
            } else {
                // Integer: printf("%ld\n", value)
                u32 fmt_lbl = add_string_constant("%ld\\n");
                load_reg(arg, "%rsi");
                emit("leaq .LCS" + std::to_string(fmt_lbl) + "(%rip), %rdi");
                emit("xorl %eax, %eax");  // no vector registers
                emit("call printf");
            }
            break;
        }

        // System V ABI: int args in %rdi,%rsi,%rdx,%rcx,%r8,%r9
        //               float args in %xmm0-%xmm7
        static const std::string int_call_regs[] = {
            "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"
        };
        static const std::string float_call_regs[] = {
            "%xmm0", "%xmm1", "%xmm2", "%xmm3",
            "%xmm4", "%xmm5", "%xmm6", "%xmm7"
        };

        // Classify arguments and load into appropriate registers
        u64 int_arg_idx = 0;
        u64 float_arg_idx = 0;
        for (u64 i = 0; i < inst.args.size(); ++i) {
            if (is_float_reg(inst.args[i])) {
                if (float_arg_idx < 8) {
                    load_xmm(inst.args[i], float_call_regs[float_arg_idx]);
                    ++float_arg_idx;
                }
            } else {
                if (int_arg_idx < 6) {
                    load_reg(inst.args[i], int_call_regs[int_arg_idx]);
                    ++int_arg_idx;
                } else {
                    // Stack argument
                    load_reg(inst.args[i], "%rax");
                    emit("pushq %rax");
                }
            }
        }

        emit("call " + mangle(inst.func_name));

        // Clean up stack args (only int args overflow to stack)
        if (int_arg_idx >= 6) {
            u64 stack_int_args = 0;
            u64 cnt = 0;
            for (u64 i = 0; i < inst.args.size(); ++i) {
                if (!is_float_reg(inst.args[i])) {
                    cnt++;
                    if (cnt > 6) stack_int_args++;
                }
            }
            if (stack_int_args > 0) {
                emit("addq $" + std::to_string(stack_int_args * 8) + ", %rsp");
            }
        }

        // Store return value
        if (inst.dst != IR_REG_NONE) {
            if (is_float_type(inst.type)) {
                store_xmm("%xmm0", inst.dst);  // float return in %xmm0
            } else {
                store_reg("%rax", inst.dst);    // int return in %rax
            }
        }
        break;
    }

    // ── Global load ──
    case IR_Op::Global_load: {
        emit_comment("global_load @" + inst.func_name + " -> %" + std::to_string(inst.dst));
        if (is_float_type(inst.type)) {
            emit("movsd " + inst.func_name + "(%rip), %xmm0");
            store_xmm("%xmm0", inst.dst);
        } else {
            u32 bytes = type_size(inst.type);
            if (bytes <= 4) {
                // movl zero-extends into the full 64-bit register on x86-64
                emit("movl " + inst.func_name + "(%rip), %eax");
            } else {
                emit("movq " + inst.func_name + "(%rip), %rax");
            }
            store_reg("%rax", inst.dst);
        }
        break;
    }

    // ── Global store ──
    case IR_Op::Global_store: {
        emit_comment("global_store %" + std::to_string(inst.src1) + " -> @" + inst.func_name);
        if (is_float_type(inst.type)) {
            load_xmm(inst.src1, "%xmm0");
            emit("movsd %xmm0, " + inst.func_name + "(%rip)");
        } else {
            load_reg(inst.src1, "%rax");
            u32 bytes = type_size(inst.type);
            std::string suffix = size_suffix(bytes);
            std::string reg = sized_reg("a", bytes);
            emit("mov" + suffix + " " + reg + ", " + inst.func_name + "(%rip)");
        }
        break;
    }

    // ── String concatenation ──
    case IR_Op::String_concat: {
        emit_comment("string_concat");
        i32 dst_off = slot_of(inst.dst);

        // strlen(src1) — save len1 in dst slot temporarily
        load_reg(inst.src1, "%rdi");
        emit("call strlen");
        emit("movq %rax, " + std::to_string(dst_off) + "(%rbp)");

        // strlen(src2)
        load_reg(inst.src2, "%rdi");
        emit("call strlen");

        // total = len1 + len2 + 1
        emit("addq " + std::to_string(dst_off) + "(%rbp), %rax");
        emit("addq $1, %rax");

        // malloc(total)
        emit("movq %rax, %rdi");
        emit("call malloc");
        emit("movq %rax, " + std::to_string(dst_off) + "(%rbp)");  // save buf ptr

        // strcpy(buf, src1)
        emit("movq " + std::to_string(dst_off) + "(%rbp), %rdi");
        load_reg(inst.src1, "%rsi");
        emit("call strcpy");

        // strcat(buf, src2)
        emit("movq " + std::to_string(dst_off) + "(%rbp), %rdi");
        load_reg(inst.src2, "%rsi");
        emit("call strcat");
        // Result pointer is already in dst slot from malloc save
        break;
    }

    } // end switch
}
