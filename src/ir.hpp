#pragma once
#include <vector>
#include <string>
#include <unordered_map>
#include <iostream>
#include "ast.hpp"
#include "log.hpp"

// ─────────────────────────────────────────────
// IR types
// ─────────────────────────────────────────────

struct IR_Type {
    Datatype_kind kind;
    u32 bit_width;

    IR_Type();
    IR_Type(Datatype_kind kind, u32 bit_width);
    std::string to_string() const;
};

using IR_Reg = u32;
constexpr IR_Reg IR_REG_NONE = UINT32_MAX;

// ─────────────────────────────────────────────
// IR opcodes
// ─────────────────────────────────────────────

enum class IR_Op {
    // Constants
    Const_int,      // %dst = const <imm>
    Const_float,    // %dst = const <fimm>
    Const_string,   // %dst = const_str <str_value>  — pointer to string literal

    // Memory (local variables)
    Alloca,         // %dst = alloca <type>      — allocate a stack slot
    Load,           // %dst = load %src          — load from slot
    Store,          // store %src1 -> %src2      — store value into slot

    // Arithmetic
    Add, Sub, Mul, Div, Mod,

    // Unary
    Neg,            // %dst = neg %src1
    Bitwise_not,    // %dst = ~%src1

    // Comparison  (result is i1 / bool)
    Eq, Neq, Lt, Gt, Le, Ge,

    // Bitwise
    And, Or, Xor, Shl, Shr,

    // Logical  (result is i1 / bool)
    Logical_and, Logical_or, Logical_not,

    // Control flow
    Label,          // label L<label_id>:
    Jump,           // jump L<label_id>
    Branch,         // branch %src1, L<true_label>, L<false_label>

    // Functions
    Ret,            // ret %src1   or   ret void
    Call,           // %dst = call <func_name>(args...)

    // String
    String_concat,  // %dst = strcat %src1, %src2  — concatenate two strings

    // Globals
    Global_load,    // %dst = global_load <name>   — load a global variable
    Global_store,   // global_store %src1, <name>  — store into a global variable
};

std::ostream& operator<<(std::ostream& os, const IR_Op& op);

// ─────────────────────────────────────────────
// IR instruction
// ─────────────────────────────────────────────

struct IR_Instruction {
    IR_Op op;

    IR_Reg dst;                 // destination register
    IR_Reg src1, src2;          // source operands

    i64 imm;                    // immediate value (Const_int)
    double fimm;                 // float immediate value (Const_float)
    std::string str_value;       // string value (Const_string)

    u32 label_id;               // Label / Jump target
    u32 true_label;             // Branch true target
    u32 false_label;            // Branch false target

    std::string func_name;      // Call target
    std::vector<IR_Reg> args;   // Call arguments

    IR_Type type;               // result type

    Token source_token;          // source location for warnings/errors

    IR_Instruction();

    void print() const;
};

// ─────────────────────────────────────────────
// IR function & program
// ─────────────────────────────────────────────

struct IR_Function {
    std::string name;
    IR_Type return_type;
    std::vector<std::pair<std::string, IR_Type>> params;   // (name, type)
    std::vector<IR_Instruction> instructions;

    void print() const;
};

// ─────────────────────────────────────────────
// Global variable
// ─────────────────────────────────────────────

struct IR_GlobalVar {
    std::string name;
    IR_Type type;
    i64 int_init;          // initial value for integer globals
    double float_init;     // initial value for float globals
    std::string string_init; // initial value for string globals
    bool is_const;
    bool has_float_init;   // true if the initialiser is a float
    bool has_string_init;  // true if the initialiser is a string

    void print() const;
};

struct IR_Program {
    std::vector<IR_Function> functions;
    std::vector<IR_GlobalVar> globals;

    void print() const;
};

// ─────────────────────────────────────────────
// IR generator — walks the AST, emits IR
// ─────────────────────────────────────────────

class IR_Generator {
private:
    AST& ast;
    IR_Program m_program;

    // Current function being emitted
    IR_Function* m_current_func;

    // Virtual register counter (per function)
    IR_Reg m_next_reg;

    // Label counter (global, monotonic)
    u32 m_next_label;

    // Current source token for source location tracking
    Token m_current_source_token;

    // Variable name → alloca register mapping (scoped)
    std::vector<std::unordered_map<std::string, IR_Reg>> m_var_scopes;

    // Global variable name → type mapping
    std::unordered_map<std::string, IR_Type> m_global_types;

    // Register → type tracking (for type propagation in binary/unary)
    std::unordered_map<IR_Reg, IR_Type> m_reg_types;

public:
    IR_Generator(AST& ast);
    IR_Program generate();

private:
    // ── register / label helpers ──
    IR_Reg new_reg();
    u32    new_label();
    void   emit(const IR_Instruction& inst);

    // ── scope helpers ──
    void   push_var_scope();
    void   pop_var_scope();
    IR_Reg lookup_var(const std::string& name);
    void   declare_var(const std::string& name, IR_Reg alloca_reg);

    // ── type helpers ──
    IR_Type resolve_type(const AST_index& dt_index);

    // ── code generation ──
    void   gen_program();
    void   gen_global_var(u32 var_index);
    void   gen_function(u32 func_index);
    void   gen_block(u32 block_index);
    void   gen_statement(const AST_index& node);
    void   gen_variable_decl(u32 var_index);
    void   gen_if(u32 if_index);
    void   gen_for(u32 for_index);
    void   gen_return(u32 ret_index);

    // Expression generation — returns the register holding the result
    IR_Reg gen_expression(const AST_index& node);
    IR_Reg gen_binary(u32 bin_index);
    IR_Reg gen_unary(u32 un_index);
    IR_Reg gen_assignment(u32 assign_index);
    IR_Reg gen_call(u32 call_index);
};
