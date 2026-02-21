#pragma once
#include "ir.hpp"
#include <sstream>
#include <unordered_map>
#include <vector>



struct Float_pool_entry {
    u32 label_id;
    double value;
};

struct String_pool_entry {
    u32 label_id;
    std::string value;
};

// ─────────────────────────────────────────────
// x86-64 code generator  (Linux, System V ABI)
// Emits AT&T-syntax assembly (.s)
// ─────────────────────────────────────────────

class Codegen {
public:
    Codegen(const IR_Program& program);

    // Generate assembly and return it as a string
    std::string generate();

    // Write the assembly to a file
    void write(const std::string& path);

private:
    const IR_Program& m_program;
    std::ostringstream m_out;

    // Per-function state
    std::unordered_map<IR_Reg, i32> m_stack_offsets;  // vreg → stack offset from %rbp
    std::unordered_map<IR_Reg, IR_Type> m_reg_types;  // vreg → type (for float dispatch)
    i32 m_stack_size;       // total stack bytes allocated (positive, rounded to 16)
    i32 m_next_offset;      // next available stack slot (negative offset from %rbp)

    // Float constant pool (global, shared across functions)
    std::vector<Float_pool_entry> m_float_pool;
    u32 m_float_label_counter;

    // String constant pool (global, shared across functions)
    std::vector<String_pool_entry> m_string_pool;
    u32 m_string_label_counter;


    void emit_program();
    void emit_function(const IR_Function& func);
    void emit_float_constants();              // emit .rodata section for float pool
    void emit_string_constants();             // emit .rodata section for string pool


    void emit_instruction(const IR_Instruction& inst);


    void emit(const std::string& line);       // indented instruction
    void emit_label(const std::string& label); // label line (no indent)
    void emit_comment(const std::string& comment);


    i32  alloc_slot(IR_Reg reg, u32 size_bytes = 8);
    i32  slot_of(IR_Reg reg) const;
    void load_reg(IR_Reg vreg, const std::string& hw_reg);   // load vreg from stack into hw_reg
    void store_reg(const std::string& hw_reg, IR_Reg vreg);  // store hw_reg into vreg's stack slot


    void load_xmm(IR_Reg vreg, const std::string& xmm_reg);  // movsd offset(%rbp), %xmmN
    void store_xmm(const std::string& xmm_reg, IR_Reg vreg); // movsd %xmmN, offset(%rbp)


    u32  add_float_constant(double value);     // returns label id


    u32  add_string_constant(const std::string& value); // returns label id


    bool is_float_type(const IR_Type& type) const;
    bool is_float_reg(IR_Reg reg) const;       // check if vreg has float type


    u32  type_size(const IR_Type& type) const;                // size in bytes
    std::string size_suffix(u32 bytes) const;                 // q, l, w, b
    std::string sized_reg(const std::string& base, u32 bytes) const; // rax→eax→ax→al


    std::string mangle(const std::string& name) const;
};
