#include "ir.hpp"
#include <sstream>

// ─────────────────────────────────────────────
// IR_Type
// ─────────────────────────────────────────────

/// @brief constructs an empty IR type (void, 0 bits)
IR_Type::IR_Type() : kind(Datatype_kind::Void), bit_width(0) {}

/// @brief constructs an IR type from a kind and bit width
IR_Type::IR_Type(Datatype_kind kind, u32 bit_width) : kind(kind), bit_width(bit_width) {}

/// @brief constructs an IR type for a struct
IR_Type::IR_Type(Datatype_kind kind, u32 bit_width, const std::string& struct_name)
    : kind(kind), bit_width(bit_width), struct_name(struct_name) {}

/// @brief converts the IR type to a human-readable string (e.g. "i32", "f64")
std::string IR_Type::to_string() const {
    switch (kind) {
        case Datatype_kind::Signed_int:   return "i" + std::to_string(bit_width);
        case Datatype_kind::Unsigned_int: return "u" + std::to_string(bit_width);
        case Datatype_kind::Float:        return "f" + std::to_string(bit_width);
        case Datatype_kind::Bool:         return "bool";
        case Datatype_kind::Void:         return "void";
        case Datatype_kind::String:       return "str";
        case Datatype_kind::Struct:       return "struct." + struct_name;
    }
    return "?";
}

// ─────────────────────────────────────────────
// IR_Op printing
// ─────────────────────────────────────────────

/// @brief prints IR_Op as a short mnemonic (e.g. "add", "store", "gload")
std::ostream& operator<<(std::ostream& os, const IR_Op& op) {
    switch (op) {
        case IR_Op::Const_int:    os << "const";   break;
        case IR_Op::Const_float:  os << "fconst";  break;
        case IR_Op::Const_string: os << "sconst";  break;
        case IR_Op::Alloca:       os << "alloca";  break;
        case IR_Op::Load:         os << "load";    break;
        case IR_Op::Store:        os << "store";   break;
        case IR_Op::Add:          os << "add";     break;
        case IR_Op::Sub:          os << "sub";     break;
        case IR_Op::Mul:          os << "mul";     break;
        case IR_Op::Div:          os << "div";     break;
        case IR_Op::Mod:          os << "mod";     break;
        case IR_Op::Neg:          os << "neg";     break;
        case IR_Op::Bitwise_not:  os << "bnot";   break;
        case IR_Op::Eq:           os << "eq";      break;
        case IR_Op::Neq:          os << "neq";     break;
        case IR_Op::Lt:           os << "lt";      break;
        case IR_Op::Gt:           os << "gt";      break;
        case IR_Op::Le:           os << "le";      break;
        case IR_Op::Ge:           os << "ge";      break;
        case IR_Op::And:          os << "and";     break;
        case IR_Op::Or:           os << "or";      break;
        case IR_Op::Xor:          os << "xor";     break;
        case IR_Op::Shl:          os << "shl";     break;
        case IR_Op::Shr:          os << "shr";     break;
        case IR_Op::Logical_and:  os << "land";    break;
        case IR_Op::Logical_or:   os << "lor";     break;
        case IR_Op::Logical_not:  os << "lnot";    break;
        case IR_Op::Label:        os << "label";   break;
        case IR_Op::Jump:         os << "jump";    break;
        case IR_Op::Branch:       os << "branch";  break;
        case IR_Op::Ret:          os << "ret";     break;
        case IR_Op::Call:         os << "call";    break;
        case IR_Op::Global_load:  os << "gload";   break;
        case IR_Op::Global_store: os << "gstore";  break;
        case IR_Op::String_concat: os << "strcat";  break;
        case IR_Op::Member_load:   os << "mload";   break;
        case IR_Op::Member_store:  os << "mstore";  break;
    }
    return os;
}

// ─────────────────────────────────────────────
// IR_Instruction
// ─────────────────────────────────────────────

/// @brief constructs a default IR instruction (const_int, all fields zeroed)
IR_Instruction::IR_Instruction()
    : op(IR_Op::Const_int), dst(IR_REG_NONE), src1(IR_REG_NONE), src2(IR_REG_NONE),
      imm(0), fimm(0.0), label_id(0), true_label(0), false_label(0) {}

/// @brief debug prints a single IR instruction in a readable text format
void IR_Instruction::print() const {
    switch (op) {
        case IR_Op::Const_int:
            std::cout << "    %" << dst << " = const " << type.to_string() << " " << imm << std::endl;
            break;

        case IR_Op::Const_float:
            std::cout << "    %" << dst << " = fconst " << type.to_string() << " " << fimm << std::endl;
            break;

        case IR_Op::Const_string:
            std::cout << "    %" << dst << " = sconst str \"" << str_value << "\"" << std::endl;
            break;

        case IR_Op::Alloca:
            std::cout << "    %" << dst << " = alloca " << type.to_string() << std::endl;
            break;

        case IR_Op::Load:
            std::cout << "    %" << dst << " = load " << type.to_string() << " %" << src1 << std::endl;
            break;

        case IR_Op::Store:
            std::cout << "    store " << type.to_string() << " %" << src1 << " -> %" << src2 << std::endl;
            break;

        case IR_Op::Add: case IR_Op::Sub: case IR_Op::Mul:
        case IR_Op::Div: case IR_Op::Mod:
        case IR_Op::Eq:  case IR_Op::Neq: case IR_Op::Lt:
        case IR_Op::Gt:  case IR_Op::Le:  case IR_Op::Ge:
        case IR_Op::And: case IR_Op::Or:  case IR_Op::Xor:
        case IR_Op::Shl: case IR_Op::Shr:
        case IR_Op::Logical_and: case IR_Op::Logical_or:
            std::cout << "    %" << dst << " = " << op << " " << type.to_string()
                      << " %" << src1 << ", %" << src2 << std::endl;
            break;

        case IR_Op::Neg: case IR_Op::Bitwise_not: case IR_Op::Logical_not:
            std::cout << "    %" << dst << " = " << op << " " << type.to_string()
                      << " %" << src1 << std::endl;
            break;

        case IR_Op::Label:
            std::cout << "  L" << label_id << ":" << std::endl;
            break;

        case IR_Op::Jump:
            std::cout << "    jump L" << label_id << std::endl;
            break;

        case IR_Op::Branch:
            std::cout << "    branch %" << src1 << ", L" << true_label << ", L" << false_label << std::endl;
            break;

        case IR_Op::Ret:
            if (src1 != IR_REG_NONE)
                std::cout << "    ret " << type.to_string() << " %" << src1 << std::endl;
            else
                std::cout << "    ret void" << std::endl;
            break;

        case IR_Op::Call:
            if (dst != IR_REG_NONE)
                std::cout << "    %" << dst << " = call " << func_name << "(";
            else
                std::cout << "    call " << func_name << "(";
            for (u64 i = 0; i < args.size(); ++i) {
                if (i > 0) std::cout << ", ";
                std::cout << "%" << args[i];
            }
            std::cout << ")" << std::endl;
            break;

        case IR_Op::Global_load:
            std::cout << "    %" << dst << " = gload " << type.to_string() << " @" << func_name << std::endl;
            break;

        case IR_Op::Global_store:
            std::cout << "    gstore " << type.to_string() << " %" << src1 << " -> @" << func_name << std::endl;
            break;

        case IR_Op::String_concat:
            std::cout << "    %" << dst << " = strcat str %" << src1 << ", %" << src2 << std::endl;
            break;

        case IR_Op::Member_load:
            std::cout << "    %" << dst << " = mload " << type.to_string()
                      << " %" << src1 << ", offset " << imm << std::endl;
            break;

        case IR_Op::Member_store:
            std::cout << "    mstore " << type.to_string() << " %" << src1
                      << " -> %" << src2 << ", offset " << imm << std::endl;
            break;
    }
}

// ─────────────────────────────────────────────
// IR_Function / IR_Program printing
// ─────────────────────────────────────────────

/// @brief debug prints a full IR function with params, return type, and all instructions
void IR_Function::print() const {
    std::cout << "fn " << name << "(";
    for (u64 i = 0; i < params.size(); ++i) {
        if (i > 0) std::cout << ", ";
        std::cout << params[i].second.to_string() << " " << params[i].first;
    }
    std::cout << ") : " << return_type.to_string() << " {" << std::endl;

    for (const auto& inst : instructions) {
        inst.print();
    }

    std::cout << "}" << std::endl;
}

/// @brief debug prints the whole IR program (globals then functions)
void IR_Program::print() const {
    // Print globals
    if (!globals.empty()) {
        for (const auto& g : globals) {
            g.print();
        }
        std::cout << std::endl;
    }
    for (u64 i = 0; i < functions.size(); ++i) {
        if (i > 0) std::cout << std::endl;
        functions[i].print();
    }
}

/// @brief debug prints a global variable declaration
void IR_GlobalVar::print() const {
    std::cout << (is_const ? "const" : "var") << " @" << name << " : " << type.to_string();
    if (has_string_init)
        std::cout << " = \"" << string_init << "\"";
    else if (has_float_init)
        std::cout << " = " << float_init;
    else
        std::cout << " = " << int_init;
    std::cout << std::endl;
}

// ─────────────────────────────────────────────
// IR_Generator — construction
// ─────────────────────────────────────────────

/// @brief constructs an IR generator from an AST
IR_Generator::IR_Generator(AST& ast)
    : ast(ast), m_current_func(nullptr), m_next_reg(0), m_next_label(0) {}

/// @brief generates the full IR program and returns it
IR_Program IR_Generator::generate() {
    gen_program();
    return m_program;
}

// ─────────────────────────────────────────────
// Register / label / emit helpers
// ─────────────────────────────────────────────

/// @brief allocates a new virtual register number
IR_Reg IR_Generator::new_reg() {
    return m_next_reg++;
}

/// @brief allocates a new label number
u32 IR_Generator::new_label() {
    return m_next_label++;
}

/// @brief appends an instruction to the current function and tracks register types
void IR_Generator::emit(const IR_Instruction& inst) {
    ASSERT(m_current_func != nullptr, "emitting instruction outside of a function");
    // Track register types for type propagation
    if (inst.dst != IR_REG_NONE && inst.type.kind != Datatype_kind::Void) {
        m_reg_types[inst.dst] = inst.type;
    }
    m_current_func->instructions.push_back(inst);
    // Attach current source location to the emitted instruction
    m_current_func->instructions.back().source_token = m_current_source_token;
}

// ─────────────────────────────────────────────
// Variable scope helpers
// ─────────────────────────────────────────────

/// @brief pushes a new variable scope for tracking local alloca slots
void IR_Generator::push_var_scope() {
    m_var_scopes.push_back({});
}

/// @brief pops the innermost variable scope
void IR_Generator::pop_var_scope() {
    ASSERT(!m_var_scopes.empty(), "tried to pop empty var scope");
    m_var_scopes.pop_back();
}

/// @brief looks up a local variable's alloca register by name
/// @return the register holding the alloca slot
IR_Reg IR_Generator::lookup_var(const std::string& name) {
    for (auto it = m_var_scopes.rbegin(); it != m_var_scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) return found->second;
    }
    ASSERT(false, "variable '" << name << "' not found in IR var scopes");
    return IR_REG_NONE;
}

/// @brief declares a local variable in the current scope
/// @param name        variable name
/// @param alloca_reg  register holding the alloca slot
void IR_Generator::declare_var(const std::string& name, IR_Reg alloca_reg) {
    ASSERT(!m_var_scopes.empty(), "no var scope to declare into");
    m_var_scopes.back()[name] = alloca_reg;
}

// ─────────────────────────────────────────────
// Type helpers
// ─────────────────────────────────────────────

/// @brief converts an AST datatype index to an IR_Type
IR_Type IR_Generator::resolve_type(const AST_index& dt_index) {
    if (dt_index.type == AST_index_type::Datatype) {
        const Datatype& dt = ast.datatypes[dt_index.index];
        if (dt.kind == Datatype_kind::Struct) {
            // Look up total size from struct layout
            auto it = m_program.struct_layouts.find(dt.struct_name);
            u32 sz = (it != m_program.struct_layouts.end()) ? it->second.total_size : 0;
            return IR_Type(Datatype_kind::Struct, sz, dt.struct_name);
        }
        return IR_Type(dt.kind, dt.bit_width);
    }
    return IR_Type(); // void
}

// ─────────────────────────────────────────────
// Top-level generation
// ─────────────────────────────────────────────

/// @brief generates the full program: first globals, then functions
void IR_Generator::gen_program() {
    const Block_statement& root = ast.block_statements[0];

    // First pass: collect struct declarations (compute layouts)
    for (const AST_index& stmt : root.statements) {
        if (stmt.type == AST_index_type::Struct_declaration) {
            gen_struct_decl(stmt.index);
        }
    }

    // Second pass: collect global variable declarations
    for (const AST_index& stmt : root.statements) {
        if (stmt.type == AST_index_type::Variable_declaration) {
            gen_global_var(stmt.index);
        }
    }

    // Third pass: generate functions
    for (const AST_index& stmt : root.statements) {
        if (stmt.type == AST_index_type::Function_declaration) {
            gen_function(stmt.index);
        }
    }
}

// ─────────────────────────────────────────────
// Global variable generation
// ─────────────────────────────────────────────

/// @brief generates an IR global variable with its initial value
void IR_Generator::gen_global_var(u32 var_index) {
    const Variable_declaration& decl = ast.variable_declarations[var_index];
    const std::string& name = ast.identifiers[decl.name.index];

    // Determine type
    IR_Type var_type;
    if (decl.datatype.type == AST_index_type::Datatype) {
        var_type = resolve_type(decl.datatype);
    } else {
        // Inferred from initialiser
        if (decl.value.type == AST_index_type::Float_literal) {
            var_type = IR_Type(Datatype_kind::Float, 64);
        } else if (decl.value.type == AST_index_type::String_literal) {
            var_type = IR_Type(Datatype_kind::String, 64);
        } else {
            var_type = IR_Type(Datatype_kind::Signed_int, 32);
        }
    }

    IR_GlobalVar global;
    global.name = name;
    global.type = var_type;
    global.is_const = decl.is_const;
    global.int_init = 0;
    global.float_init = 0.0;
    global.has_float_init = false;
    global.has_string_init = false;

    // Extract constant initial value
    if (decl.value.type == AST_index_type::Integer) {
        global.int_init = ast.integers[decl.value.index];
    } else if (decl.value.type == AST_index_type::Float_literal) {
        global.float_init = ast.floats[decl.value.index];
        global.has_float_init = true;
    } else if (decl.value.type == AST_index_type::String_literal) {
        global.string_init = ast.string_literals[decl.value.index];
        global.has_string_init = true;
    }

    m_global_types[name] = var_type;
    m_program.globals.push_back(global);
}

// ─────────────────────────────────────────────
// Function generation
// ─────────────────────────────────────────────

/// @brief generates an IR function: params, body, and all instructions
void IR_Generator::gen_function(u32 func_index) {
    const Function_declaration& func = ast.function_declarations[func_index];
    m_current_source_token = ast.identifier_tokens[func.identifier.index];

    IR_Function ir_func;
    ir_func.name = ast.identifiers[func.identifier.index];
    ir_func.return_type = resolve_type(func.return_type);

    // Collect parameters
    for (u64 i = 0; i < func.names.size(); ++i) {
        std::string pname = ast.identifiers[func.names[i].index];
        IR_Type ptype = (i < func.datatypes.size()) ? resolve_type(func.datatypes[i]) : IR_Type();
        ir_func.params.push_back({pname, ptype});
    }

    m_program.functions.push_back(std::move(ir_func));
    m_current_func = &m_program.functions.back();
    m_next_reg = 0;
    m_reg_types.clear();

    push_var_scope();

    // Create alloca slots for parameters and store the param values
    for (u64 i = 0; i < m_current_func->params.size(); ++i) {
        const auto& p = m_current_func->params[i];

        // alloca for the param
        IR_Reg slot = new_reg();
        {
            IR_Instruction inst;
            inst.op   = IR_Op::Alloca;
            inst.dst  = slot;
            inst.type = p.second;
            emit(inst);
        }

        // The parameter value itself occupies a virtual register (param index)
        IR_Reg param_val = new_reg();
        {
            // We represent the incoming param as a const_int placeholder
            // A real compiler would have a Param op, but we use the register directly
            // by emitting a store of the "param register" into the slot.
            IR_Instruction store_inst;
            store_inst.op   = IR_Op::Store;
            store_inst.src1 = param_val;
            store_inst.src2 = slot;
            store_inst.type = p.second;
            emit(store_inst);
        }

        declare_var(p.first, slot);
    }

    // Generate the function body
    gen_block(func.block.index);

    pop_var_scope();
    m_current_func = nullptr;
}

// ─────────────────────────────────────────────
// Block / statement generation
// ─────────────────────────────────────────────

/// @brief generates IR for all statements in a block
void IR_Generator::gen_block(u32 block_index) {
    const Block_statement& block = ast.block_statements[block_index];

    push_var_scope();

    for (const AST_index& stmt : block.statements) {
        gen_statement(stmt);
    }

    pop_var_scope();
}

/// @brief dispatches IR generation for any statement or expression node
void IR_Generator::gen_statement(const AST_index& node) {
    switch (node.type) {
        case AST_index_type::Variable_declaration:
            gen_variable_decl(node.index);
            break;

        case AST_index_type::If_statement:
            gen_if(node.index);
            break;

        case AST_index_type::For_statement:
            gen_for(node.index);
            break;

        case AST_index_type::Return_statement:
            gen_return(node.index);
            break;

        case AST_index_type::Block_statement:
            gen_block(node.index);
            break;

        case AST_index_type::Function_declaration:
            // Nested functions — not supported, skip
            break;

        case AST_index_type::Struct_declaration:
            // Struct declarations don't emit code, layout already collected
            break;

        // Expression statements
        case AST_index_type::Integer:
        case AST_index_type::Float_literal:
        case AST_index_type::Identifier:
        case AST_index_type::Binary_expression:
        case AST_index_type::Unary_expression:
        case AST_index_type::Assignment_expression:
            gen_expression(node);
            break;

        case AST_index_type::Call_expression:
            gen_expression(node);
            break;

        default:
            break;
    }
}

// ─────────────────────────────────────────────
// Variable declaration
// ─────────────────────────────────────────────

/// @brief generates IR for a local variable declaration: alloca, optional store
void IR_Generator::gen_variable_decl(u32 var_index) {
    const Variable_declaration& decl = ast.variable_declarations[var_index];
    const std::string& name = ast.identifiers[decl.name.index];
    m_current_source_token = ast.identifier_tokens[decl.name.index];

    // Determine the type
    IR_Type var_type;
    if (decl.datatype.type == AST_index_type::Datatype) {
        var_type = resolve_type(decl.datatype);
    } else {
        // Type inferred from initialiser — generate the value first isn't possible here,
        // so we heuristically determine the type from the AST node
        if (decl.value.type == AST_index_type::Float_literal) {
            var_type = IR_Type(Datatype_kind::Float, 64);
        } else if (decl.value.type == AST_index_type::String_literal) {
            var_type = IR_Type(Datatype_kind::String, 64);
        } else if (decl.value.type == AST_index_type::Call_expression) {
            // Infer type from the callee's return type
            const Call_expression& call = ast.call_expressions[decl.value.index];
            const std::string& callee_name = ast.identifiers[call.callee.index];
            var_type = IR_Type(Datatype_kind::Signed_int, 32); // default
            for (const auto& f : m_program.functions) {
                if (f.name == callee_name) {
                    var_type = f.return_type;
                    break;
                }
            }
        } else {
            var_type = IR_Type(Datatype_kind::Signed_int, 32);
        }
    }

    // Emit alloca
    IR_Reg slot = new_reg();
    {
        IR_Instruction inst;
        inst.op   = IR_Op::Alloca;
        inst.dst  = slot;
        inst.type = var_type;
        emit(inst);
    }

    declare_var(name, slot);

    // If there's an initialiser, generate it and store
    if (decl.value.type != AST_index_type::Invalid) {
        IR_Reg val = gen_expression(decl.value);

        // Guard: don't store if the expression produced no value (void call)
        if (val != IR_REG_NONE) {
            IR_Instruction store_inst;
            store_inst.op   = IR_Op::Store;
            store_inst.src1 = val;
            store_inst.src2 = slot;
            store_inst.type = var_type;
            emit(store_inst);
        }
    } else {
        // No initialiser — zero-initialise the variable
        // For structs, we zero-fill each 8-byte chunk; for scalars, store a single zero
        if (var_type.kind == Datatype_kind::Struct) {
            u32 total = var_type.bit_width; // total_size in bytes
            for (u32 off = 0; off < total; off += 8) {
                IR_Reg zero = new_reg();
                IR_Instruction ci;
                ci.op   = IR_Op::Const_int;
                ci.dst  = zero;
                ci.imm  = 0;
                ci.type = IR_Type(Datatype_kind::Signed_int, 64);
                emit(ci);

                IR_Instruction ms;
                ms.op   = IR_Op::Member_store;
                ms.src1 = zero;
                ms.src2 = slot;
                ms.imm  = off;
                ms.type = IR_Type(Datatype_kind::Signed_int, 64);
                emit(ms);
            }
        } else if (var_type.kind == Datatype_kind::Float) {
            IR_Reg zero = new_reg();
            IR_Instruction ci;
            ci.op   = IR_Op::Const_float;
            ci.dst  = zero;
            ci.fimm = 0.0;
            ci.type = var_type;
            emit(ci);

            IR_Instruction store_inst;
            store_inst.op   = IR_Op::Store;
            store_inst.src1 = zero;
            store_inst.src2 = slot;
            store_inst.type = var_type;
            emit(store_inst);
        } else {
            // Integer, bool, string pointer — store 0
            IR_Reg zero = new_reg();
            IR_Instruction ci;
            ci.op   = IR_Op::Const_int;
            ci.dst  = zero;
            ci.imm  = 0;
            ci.type = var_type;
            emit(ci);

            IR_Instruction store_inst;
            store_inst.op   = IR_Op::Store;
            store_inst.src1 = zero;
            store_inst.src2 = slot;
            store_inst.type = var_type;
            emit(store_inst);
        }
    }
}

// ─────────────────────────────────────────────
// If statement
// ─────────────────────────────────────────────

/// @brief generates IR for an if statement with branch, then, optional else, and end labels
void IR_Generator::gen_if(u32 if_index) {
    const If_statement& stmt = ast.if_statements[if_index];

    IR_Reg cond = gen_expression(stmt.condition);

    u32 then_label = new_label();
    u32 end_label  = new_label();

    bool has_else = (stmt.false_condition.type != AST_index_type::Invalid);
    u32 else_label = has_else ? new_label() : end_label;

    // branch cond, then_label, else_label
    {
        IR_Instruction inst;
        inst.op          = IR_Op::Branch;
        inst.src1        = cond;
        inst.true_label  = then_label;
        inst.false_label = else_label;
        emit(inst);
    }

    // then block
    {
        IR_Instruction lbl;
        lbl.op       = IR_Op::Label;
        lbl.label_id = then_label;
        emit(lbl);
    }
    if (stmt.true_condition.type == AST_index_type::Block_statement)
        gen_block(stmt.true_condition.index);
    else
        gen_statement(stmt.true_condition);
    {
        IR_Instruction jmp;
        jmp.op       = IR_Op::Jump;
        jmp.label_id = end_label;
        emit(jmp);
    }

    // else block
    if (has_else) {
        {
            IR_Instruction lbl;
            lbl.op       = IR_Op::Label;
            lbl.label_id = else_label;
            emit(lbl);
        }
        if (stmt.false_condition.type == AST_index_type::Block_statement)
            gen_block(stmt.false_condition.index);
        else
            gen_statement(stmt.false_condition);
        {
            IR_Instruction jmp;
            jmp.op       = IR_Op::Jump;
            jmp.label_id = end_label;
            emit(jmp);
        }
    }

    // end label
    {
        IR_Instruction lbl;
        lbl.op       = IR_Op::Label;
        lbl.label_id = end_label;
        emit(lbl);
    }
}

// ─────────────────────────────────────────────
// For statement
//   for init condition post { block }
//   →  init; L_cond: branch cond L_body L_end;
//      L_body: block; post; jump L_cond;
//      L_end:
// ─────────────────────────────────────────────

/// @brief generates IR for a for loop: init, cond label, branch, body, post, jump back
void IR_Generator::gen_for(u32 for_index) {
    const For_statement& stmt = ast.for_statements[for_index];

    u32 cond_label = new_label();
    u32 body_label = new_label();
    u32 end_label  = new_label();

    push_var_scope();

    // init
    if (stmt.init.type != AST_index_type::Invalid)
        gen_statement(stmt.init);

    // cond label
    {
        IR_Instruction lbl;
        lbl.op       = IR_Op::Label;
        lbl.label_id = cond_label;
        emit(lbl);
    }

    // condition
    if (stmt.condition.type != AST_index_type::Invalid) {
        IR_Reg cond = gen_expression(stmt.condition);
        IR_Instruction br;
        br.op          = IR_Op::Branch;
        br.src1        = cond;
        br.true_label  = body_label;
        br.false_label = end_label;
        emit(br);
    } else {
        // infinite loop
        IR_Instruction jmp;
        jmp.op       = IR_Op::Jump;
        jmp.label_id = body_label;
        emit(jmp);
    }

    // body label
    {
        IR_Instruction lbl;
        lbl.op       = IR_Op::Label;
        lbl.label_id = body_label;
        emit(lbl);
    }

    // body
    if (stmt.block.type == AST_index_type::Block_statement)
        gen_block(stmt.block.index);

    // post
    if (stmt.post.type != AST_index_type::Invalid)
        gen_statement(stmt.post);

    // jump back to condition
    {
        IR_Instruction jmp;
        jmp.op       = IR_Op::Jump;
        jmp.label_id = cond_label;
        emit(jmp);
    }

    // end label
    {
        IR_Instruction lbl;
        lbl.op       = IR_Op::Label;
        lbl.label_id = end_label;
        emit(lbl);
    }

    pop_var_scope();
}

// ─────────────────────────────────────────────
// Return statement
// ─────────────────────────────────────────────

/// @brief generates IR for a return statement with optional value
void IR_Generator::gen_return(u32 ret_index) {
    const Return_statement& ret = ast.return_statements[ret_index];

    IR_Instruction inst;
    inst.op = IR_Op::Ret;

    if (ret.value.type != AST_index_type::Invalid) {
        IR_Reg val = gen_expression(ret.value);
        inst.src1 = val;

        // If the function has no explicit return type (void), infer it from the returned value
        if (m_current_func->return_type.kind == Datatype_kind::Void) {
            auto type_it = m_reg_types.find(val);
            if (type_it != m_reg_types.end()) {
                m_current_func->return_type = type_it->second;
            }
        }

        inst.type = m_current_func->return_type;
    }

    emit(inst);
}

// ─────────────────────────────────────────────
// Expression generation — returns result register
// ─────────────────────────────────────────────

/// @brief generates IR for any expression node and returns the result register
IR_Reg IR_Generator::gen_expression(const AST_index& node) {
    switch (node.type) {
        case AST_index_type::Integer: {
            m_current_source_token = ast.integer_tokens[node.index];
            i64 val = ast.integers[node.index];
            IR_Reg dst = new_reg();
            IR_Instruction inst;
            inst.op   = IR_Op::Const_int;
            inst.dst  = dst;
            inst.imm  = val;
            inst.type = IR_Type(Datatype_kind::Signed_int, 32);
            emit(inst);
            return dst;
        }

        case AST_index_type::Float_literal: {
            m_current_source_token = ast.float_tokens[node.index];
            double val = ast.floats[node.index];
            IR_Reg dst = new_reg();
            IR_Instruction inst;
            inst.op   = IR_Op::Const_float;
            inst.dst  = dst;
            inst.fimm = val;
            inst.type = IR_Type(Datatype_kind::Float, 64);
            emit(inst);
            return dst;
        }

        case AST_index_type::String_literal: {
            m_current_source_token = ast.string_tokens[node.index];
            const std::string& val = ast.string_literals[node.index];
            IR_Reg dst = new_reg();
            IR_Instruction inst;
            inst.op        = IR_Op::Const_string;
            inst.dst       = dst;
            inst.str_value = val;
            inst.type      = IR_Type(Datatype_kind::String, 64);
            emit(inst);
            return dst;
        }

        case AST_index_type::Identifier: {
            m_current_source_token = ast.identifier_tokens[node.index];
            const std::string& name = ast.identifiers[node.index];

            // Check if this is a global variable
            auto global_it = m_global_types.find(name);
            if (global_it != m_global_types.end()) {
                IR_Reg dst = new_reg();
                IR_Instruction inst;
                inst.op        = IR_Op::Global_load;
                inst.dst       = dst;
                inst.func_name = name;  // reuse func_name field for global name
                inst.type      = global_it->second;
                emit(inst);
                return dst;
            }

            // Local variable — load from alloca slot
            IR_Reg slot = lookup_var(name);
            IR_Reg dst = new_reg();
            IR_Instruction inst;
            inst.op   = IR_Op::Load;
            inst.dst  = dst;
            inst.src1 = slot;
            // Propagate type from the alloca slot
            auto type_it = m_reg_types.find(slot);
            inst.type = (type_it != m_reg_types.end()) ? type_it->second
                        : IR_Type(Datatype_kind::Signed_int, 32);
            emit(inst);
            return dst;
        }

        case AST_index_type::Binary_expression:
            return gen_binary(node.index);

        case AST_index_type::Unary_expression:
            return gen_unary(node.index);

        case AST_index_type::Assignment_expression:
            return gen_assignment(node.index);

        case AST_index_type::Call_expression:
            return gen_call(node.index);

        case AST_index_type::Member_access:
            return gen_member_access(node.index);

        default:
            return IR_REG_NONE;
    }
}

// ─────────────────────────────────────────────
// Binary expression
// ─────────────────────────────────────────────

/// @brief maps a Token_type to the corresponding IR binary operation
static IR_Op token_to_binop(Token_type t) {
    switch (t) {
        case Token_type::Plus:          return IR_Op::Add;
        case Token_type::Minus:         return IR_Op::Sub;
        case Token_type::Multiply:      return IR_Op::Mul;
        case Token_type::Divide:        return IR_Op::Div;
        case Token_type::Modulus:       return IR_Op::Mod;
        case Token_type::Equal:         return IR_Op::Eq;
        case Token_type::Not_equal:     return IR_Op::Neq;
        case Token_type::Less_than:     return IR_Op::Lt;
        case Token_type::Greater_than:  return IR_Op::Gt;
        case Token_type::Less_equal:    return IR_Op::Le;
        case Token_type::Greater_equal: return IR_Op::Ge;
        case Token_type::Bitwise_and:   return IR_Op::And;
        case Token_type::Bitwise_or:    return IR_Op::Or;
        case Token_type::Bitwise_xor:   return IR_Op::Xor;
        case Token_type::Left_shift:    return IR_Op::Shl;
        case Token_type::Right_shift:   return IR_Op::Shr;
        case Token_type::Logical_and:   return IR_Op::Logical_and;
        case Token_type::Logical_or:    return IR_Op::Logical_or;
        default:                        return IR_Op::Add;
    }
}

/// @brief generates IR for a binary expression (lhs op rhs)
IR_Reg IR_Generator::gen_binary(u32 bin_index) {
    const Binary_expression& bin = ast.binary_expressions[bin_index];

    IR_Reg lhs = gen_expression(bin.lhs);
    IR_Reg rhs = gen_expression(bin.rhs);

    // Determine the operand type (from lhs register)
    IR_Type operand_type = IR_Type(Datatype_kind::Signed_int, 32);
    auto it = m_reg_types.find(lhs);
    if (it != m_reg_types.end()) {
        operand_type = it->second;
    }

    // String concatenation: string + string → String_concat
    if (operand_type.kind == Datatype_kind::String && bin.opp == Token_type::Plus) {
        IR_Reg dst = new_reg();
        IR_Instruction inst;
        inst.op   = IR_Op::String_concat;
        inst.dst  = dst;
        inst.src1 = lhs;
        inst.src2 = rhs;
        inst.type = IR_Type(Datatype_kind::String, 64);
        emit(inst);
        return dst;
    }

    IR_Op ir_op = token_to_binop(bin.opp);

    IR_Reg dst = new_reg();

    IR_Instruction inst;
    inst.op   = ir_op;
    inst.dst  = dst;
    inst.src1 = lhs;
    inst.src2 = rhs;

    // Comparisons and logical ops produce bool
    switch (ir_op) {
        case IR_Op::Eq:  case IR_Op::Neq: case IR_Op::Lt:
        case IR_Op::Gt:  case IR_Op::Le:  case IR_Op::Ge:
        case IR_Op::Logical_and: case IR_Op::Logical_or:
            inst.type = IR_Type(Datatype_kind::Bool, 1);
            break;
        default:
            inst.type = operand_type;  // propagate from operands
            break;
    }

    emit(inst);
    return dst;
}

// ─────────────────────────────────────────────
// Unary expression
// ─────────────────────────────────────────────

/// @brief generates IR for a unary expression (negation, bitwise not, logical not)
IR_Reg IR_Generator::gen_unary(u32 un_index) {
    const Unary_expression& un = ast.unary_expressions[un_index];

    IR_Reg operand = gen_expression(un.operand);
    IR_Reg dst = new_reg();

    // Get operand type for propagation
    IR_Type operand_type = IR_Type(Datatype_kind::Signed_int, 32);
    auto it = m_reg_types.find(operand);
    if (it != m_reg_types.end()) {
        operand_type = it->second;
    }

    IR_Instruction inst;
    inst.dst  = dst;
    inst.src1 = operand;

    switch (un.opp) {
        case Token_type::Minus:
            inst.op   = IR_Op::Neg;
            inst.type = operand_type;
            break;
        case Token_type::Bitwise_not:
            inst.op   = IR_Op::Bitwise_not;
            inst.type = operand_type;
            break;
        case Token_type::Logical_not:
            inst.op   = IR_Op::Logical_not;
            inst.type = IR_Type(Datatype_kind::Bool, 1);
            break;
        default:
            inst.op   = IR_Op::Neg;
            inst.type = operand_type;
            break;
    }

    emit(inst);
    return dst;
}

// ─────────────────────────────────────────────
// Call expression
// ─────────────────────────────────────────────

/// @brief generates IR for a function call and returns the result register
IR_Reg IR_Generator::gen_call(u32 call_index) {
    const Call_expression& call = ast.call_expressions[call_index];
    const std::string& func_name = ast.identifiers[call.callee.index];
    m_current_source_token = ast.identifier_tokens[call.callee.index];

    // Generate argument values
    std::vector<IR_Reg> arg_regs;
    for (const auto& arg : call.arguments) {
        arg_regs.push_back(gen_expression(arg));
    }

    // Look up the return type from the function declarations
    IR_Type ret_type(Datatype_kind::Signed_int, 32); // default
    bool is_builtin = (func_name == "print");
    if (is_builtin) {
        ret_type = IR_Type(Datatype_kind::Void, 0);
    } else {
        for (const auto& func : m_program.functions) {
            if (func.name == func_name) {
                ret_type = func.return_type;
                break;
            }
        }
    }

    IR_Reg dst = (ret_type.kind != Datatype_kind::Void) ? new_reg() : IR_REG_NONE;

    IR_Instruction inst;
    inst.op        = IR_Op::Call;
    inst.dst       = dst;
    inst.func_name = func_name;
    inst.args      = arg_regs;
    inst.type      = ret_type;
    emit(inst);

    return dst;
}

// ─────────────────────────────────────────────
// Assignment expression
// ─────────────────────────────────────────────

/// @brief generates IR for an assignment (simple or compound, local or global)
IR_Reg IR_Generator::gen_assignment(u32 assign_index) {
    const Assignment_expression& assign = ast.assignment_expressions[assign_index];

    // Member access assignment (e.g. p.x = 5)
    if (assign.target.type == AST_index_type::Member_access) {
        IR_Reg val = gen_expression(assign.value);

        // For compound assignments on member access, load first, compute, then store
        if (assign.opp != Token_type::Assign) {
            IR_Reg current = gen_member_access(assign.target.index);

            IR_Op compound_op;
            switch (assign.opp) {
                case Token_type::Plus_assign:          compound_op = IR_Op::Add; break;
                case Token_type::Minus_assign:         compound_op = IR_Op::Sub; break;
                case Token_type::Multiply_assign:      compound_op = IR_Op::Mul; break;
                case Token_type::Divide_assign:        compound_op = IR_Op::Div; break;
                case Token_type::Modulus_assign:        compound_op = IR_Op::Mod; break;
                case Token_type::Bitwise_and_assign:   compound_op = IR_Op::And; break;
                case Token_type::Bitwise_or_assign:    compound_op = IR_Op::Or;  break;
                case Token_type::Bitwise_xor_assign:   compound_op = IR_Op::Xor; break;
                case Token_type::Left_shift_assign:    compound_op = IR_Op::Shl; break;
                case Token_type::Right_shift_assign:   compound_op = IR_Op::Shr; break;
                default:                               compound_op = IR_Op::Add; break;
            }

            IR_Reg result = new_reg();
            IR_Type field_type;
            auto it = m_reg_types.find(current);
            field_type = (it != m_reg_types.end()) ? it->second : IR_Type(Datatype_kind::Signed_int, 32);

            IR_Instruction binop;
            binop.op   = compound_op;
            binop.dst  = result;
            binop.src1 = current;
            binop.src2 = val;
            binop.type = field_type;
            emit(binop);
            val = result;
        }

        gen_member_store(assign.target, val);
        return val;
    }

    ASSERT(assign.target.type == AST_index_type::Identifier,
           "assignment target must be an identifier in IR gen");
    const std::string& name = ast.identifiers[assign.target.index];
    m_current_source_token = ast.identifier_tokens[assign.target.index];

    // Check if this is a global variable
    auto global_it = m_global_types.find(name);
    if (global_it != m_global_types.end()) {
        IR_Reg val = gen_expression(assign.value);

        // Compound assignments: global_load, op, global_store
        if (assign.opp != Token_type::Assign) {
            IR_Reg current = new_reg();
            {
                IR_Instruction load;
                load.op        = IR_Op::Global_load;
                load.dst       = current;
                load.func_name = name;
                load.type      = global_it->second;
                emit(load);
            }

            IR_Op compound_op;
            switch (assign.opp) {
                case Token_type::Plus_assign:          compound_op = IR_Op::Add; break;
                case Token_type::Minus_assign:         compound_op = IR_Op::Sub; break;
                case Token_type::Multiply_assign:      compound_op = IR_Op::Mul; break;
                case Token_type::Divide_assign:        compound_op = IR_Op::Div; break;
                case Token_type::Modulus_assign:        compound_op = IR_Op::Mod; break;
                case Token_type::Bitwise_and_assign:   compound_op = IR_Op::And; break;
                case Token_type::Bitwise_or_assign:    compound_op = IR_Op::Or;  break;
                case Token_type::Bitwise_xor_assign:   compound_op = IR_Op::Xor; break;
                case Token_type::Left_shift_assign:    compound_op = IR_Op::Shl; break;
                case Token_type::Right_shift_assign:   compound_op = IR_Op::Shr; break;
                default:                               compound_op = IR_Op::Add; break;
            }

            IR_Reg result = new_reg();
            {
                IR_Instruction binop;
                binop.op   = compound_op;
                binop.dst  = result;
                binop.src1 = current;
                binop.src2 = val;
                binop.type = global_it->second;
                emit(binop);
            }
            val = result;
        }

        // Global store
        {
            IR_Instruction store;
            store.op        = IR_Op::Global_store;
            store.src1      = val;
            store.func_name = name;  // reuse func_name for global name
            store.type      = global_it->second;
            emit(store);
        }

        return val;
    }

    // Local variable — existing logic
    IR_Reg slot = lookup_var(name);

    // Get the slot's type
    IR_Type slot_type = IR_Type(Datatype_kind::Signed_int, 32);
    auto type_it = m_reg_types.find(slot);
    if (type_it != m_reg_types.end()) {
        slot_type = type_it->second;
    }

    IR_Reg val = gen_expression(assign.value);

    // Compound assignments: load, op, store
    if (assign.opp != Token_type::Assign) {
        IR_Reg current = new_reg();
        {
            IR_Instruction load;
            load.op   = IR_Op::Load;
            load.dst  = current;
            load.src1 = slot;
            load.type = slot_type;
            emit(load);
        }

        IR_Op compound_op;
        switch (assign.opp) {
            case Token_type::Plus_assign:          compound_op = IR_Op::Add; break;
            case Token_type::Minus_assign:         compound_op = IR_Op::Sub; break;
            case Token_type::Multiply_assign:      compound_op = IR_Op::Mul; break;
            case Token_type::Divide_assign:        compound_op = IR_Op::Div; break;
            case Token_type::Modulus_assign:        compound_op = IR_Op::Mod; break;
            case Token_type::Bitwise_and_assign:   compound_op = IR_Op::And; break;
            case Token_type::Bitwise_or_assign:    compound_op = IR_Op::Or;  break;
            case Token_type::Bitwise_xor_assign:   compound_op = IR_Op::Xor; break;
            case Token_type::Left_shift_assign:    compound_op = IR_Op::Shl; break;
            case Token_type::Right_shift_assign:   compound_op = IR_Op::Shr; break;
            default:                               compound_op = IR_Op::Add; break;
        }

        IR_Reg result = new_reg();
        {
            IR_Instruction binop;
            binop.op   = compound_op;
            binop.dst  = result;
            binop.src1 = current;
            binop.src2 = val;
            binop.type = slot_type;
            emit(binop);
        }

        val = result;
    }

    // Store the value
    {
        IR_Instruction store;
        store.op   = IR_Op::Store;
        store.src1 = val;
        store.src2 = slot;
        store.type = slot_type;
        emit(store);
    }

    return val;
}
// ─────────────────────────────────────────────
// Struct declaration — compute layout
// ─────────────────────────────────────────────

/// @brief computes the memory layout for a struct (field offsets and total size)
void IR_Generator::gen_struct_decl(u32 struct_index) {
    const Struct_declaration& decl = ast.struct_declarations[struct_index];
    const std::string& name = ast.identifiers[decl.name.index];

    IR_StructLayout layout;
    layout.name = name;
    u32 offset = 0;

    for (u64 i = 0; i < decl.field_names.size(); ++i) {
        layout.field_names.push_back(ast.identifiers[decl.field_names[i].index]);

        IR_Type ft = resolve_type(decl.field_types[i]);
        layout.field_types.push_back(ft);
        layout.field_offsets.push_back(offset);

        // Compute field size (round up to bytes, minimum 1)
        u32 field_bytes;
        if (ft.kind == Datatype_kind::Struct) {
            field_bytes = ft.bit_width; // bit_width holds total_size for struct types
        } else {
            field_bytes = (ft.bit_width + 7) / 8;
            if (field_bytes == 0) field_bytes = 8; // pointers / void*
        }

        // Align to natural alignment (min of field_bytes and 8)
        u32 align = std::min(field_bytes, (u32)8);
        if (align > 0) {
            offset = (offset + align - 1) & ~(align - 1);
            layout.field_offsets.back() = offset;
        }
        offset += field_bytes;
    }

    // Align total size to 8 bytes
    layout.total_size = (offset + 7) & ~7u;

    m_program.struct_layouts[name] = layout;
}

// ─────────────────────────────────────────────
// Member access expression (rvalue)
// ─────────────────────────────────────────────

/// @brief helper to walk a member access chain and return (base_slot_reg, byte_offset, field_type)
static void resolve_member_access(
    const AST& ast,
    const std::unordered_map<std::string, IR_StructLayout>& layouts,
    const std::unordered_map<IR_Reg, IR_Type>& reg_types,
    const std::vector<std::unordered_map<std::string, IR_Reg>>& var_scopes,
    const std::unordered_map<std::string, IR_Type>& global_types,
    const AST_index& node,
    IR_Reg& out_base, u32& out_offset, IR_Type& out_field_type)
{
    if (node.type == AST_index_type::Member_access) {
        const Member_access_expression& ma = ast.member_access_expressions[node.index];
        const std::string& member = ast.identifiers[ma.member.index];

        // Recursively resolve the object
        IR_Reg base;
        u32 base_offset;
        IR_Type obj_type;
        resolve_member_access(ast, layouts, reg_types, var_scopes, global_types,
                              ma.object, base, base_offset, obj_type);

        // obj_type should be a struct
        ASSERT(obj_type.kind == Datatype_kind::Struct,
               "member access on non-struct type: " << obj_type.to_string());

        auto it = layouts.find(obj_type.struct_name);
        ASSERT(it != layouts.end(), "unknown struct: " << obj_type.struct_name);

        const IR_StructLayout& layout = it->second;
        for (u64 i = 0; i < layout.field_names.size(); ++i) {
            if (layout.field_names[i] == member) {
                out_base = base;
                out_offset = base_offset + layout.field_offsets[i];
                out_field_type = layout.field_types[i];
                return;
            }
        }
        ASSERT(false, "struct '" << obj_type.struct_name << "' has no field '" << member << "'");
    } else if (node.type == AST_index_type::Identifier) {
        const std::string& name = ast.identifiers[node.index];

        // Look up in local var scopes
        for (auto it = var_scopes.rbegin(); it != var_scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) {
                out_base = found->second;
                out_offset = 0;
                // Get the type from reg_types
                auto type_it = reg_types.find(found->second);
                out_field_type = (type_it != reg_types.end()) ? type_it->second
                                : IR_Type(Datatype_kind::Signed_int, 32);
                return;
            }
        }
        ASSERT(false, "variable '" << name << "' not found for member access");
    } else {
        ASSERT(false, "unexpected node type in member access chain");
    }
}

/// @brief generates IR for a member access expression (rvalue — loads the field)
IR_Reg IR_Generator::gen_member_access(u32 ma_index) {
    const Member_access_expression& ma = ast.member_access_expressions[ma_index];

    IR_Reg base;
    u32 offset;
    IR_Type field_type;
    AST_index node(AST_index_type::Member_access, ma_index);
    resolve_member_access(ast, m_program.struct_layouts, m_reg_types, m_var_scopes, m_global_types,
                          node, base, offset, field_type);

    IR_Reg dst = new_reg();
    IR_Instruction inst;
    inst.op   = IR_Op::Member_load;
    inst.dst  = dst;
    inst.src1 = base;
    inst.imm  = offset;
    inst.type = field_type;
    emit(inst);
    return dst;
}

/// @brief generates IR for storing a value into a member access target (lvalue)
void IR_Generator::gen_member_store(const AST_index& target, IR_Reg val) {
    IR_Reg base;
    u32 offset;
    IR_Type field_type;
    resolve_member_access(ast, m_program.struct_layouts, m_reg_types, m_var_scopes, m_global_types,
                          target, base, offset, field_type);

    IR_Instruction inst;
    inst.op   = IR_Op::Member_store;
    inst.src1 = val;
    inst.src2 = base;
    inst.imm  = offset;
    inst.type = field_type;
    emit(inst);
}