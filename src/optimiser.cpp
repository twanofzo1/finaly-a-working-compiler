#include "optimiser.hpp"
#include "log.hpp"
#include <algorithm>



/// @brief constructs the optimiser for an IR program
IR_Optimiser::IR_Optimiser(IR_Program& program, const std::string& source)
    : m_program(program), m_source(source) {}


/// @brief emits a Zig-style compiler warning with source location and the offending line
void IR_Optimiser::compiler_warning(const std::string& msg, const IR_Instruction& inst) {
    // Only emit if the instruction has a valid source token
    if (inst.source_token.view.empty()) return;

    Token tok = inst.source_token;
    set_terminal_color(Terminal_color::Yellow);
    std::cerr << "warning";
    set_terminal_color(Terminal_color::Default);
    std::cerr << " on line " << tok.get_line(m_source)
              << " pos " << tok.get_pos(m_source)
              << ": " << msg << std::endl;
    tok.print_line(m_source);
}


/// @brief runs all optimisation passes on every function until nothing changes
void IR_Optimiser::optimise() {
    for (auto& func : m_program.functions) {
        // Keep running passes until nothing changes
        i32 iteration = 0;
        while (true) {
            if (run_pass(func)){
                iteration = 0; // reset iteration count if we made a change
            }
            iteration++;
            // if after 10 iterations of no change, we probably converged (or are in a weird infinite loop, but that would be a bug)
            if (iteration > 10) break; 
        }

        #ifndef NDEBUG
        LOG("Optimiser: function '" << func.name << "' converged after " << iteration << " iterations");
        #endif
    }
}

/// @brief runs all 8 optimisation passes once and returns true if anything changed
bool IR_Optimiser::run_pass(IR_Function& func) {
    bool changed = false;
    changed |= constant_folding(func);
    changed |= constant_propagation(func);
    changed |= store_load_forwarding(func);
    changed |= dead_store_elimination(func);
    changed |= branch_folding(func);
    changed |= unreachable_code_elimination(func);
    changed |= redundant_jump_elimination(func);
    changed |= dead_code_elimination(func);
    return changed;
}



/// @brief checks if an IR op is an arithmetic or bitwise binary operation
bool IR_Optimiser::is_arithmetic(IR_Op op) const {
    switch (op) {
        case IR_Op::Add: case IR_Op::Sub: case IR_Op::Mul:
        case IR_Op::Div: case IR_Op::Mod:
        case IR_Op::And: case IR_Op::Or:  case IR_Op::Xor:
        case IR_Op::Shl: case IR_Op::Shr:
            return true;
        default:
            return false;
    }
}

/// @brief checks if an IR op is a comparison or logical binary operation
bool IR_Optimiser::is_comparison(IR_Op op) const {
    switch (op) {
        case IR_Op::Eq:  case IR_Op::Neq:
        case IR_Op::Lt:  case IR_Op::Gt:
        case IR_Op::Le:  case IR_Op::Ge:
        case IR_Op::Logical_and: case IR_Op::Logical_or:
            return true;
        default:
            return false;
    }
}

/// @brief checks if an IR op is a unary operation (neg, bitwise not, logical not)
bool IR_Optimiser::is_unary(IR_Op op) const {
    switch (op) {
        case IR_Op::Neg: case IR_Op::Bitwise_not: case IR_Op::Logical_not:
            return true;
        default:
            return false;
    }
}

/// @brief checks if an instruction has side effects (can't be safely removed)
bool IR_Optimiser::has_side_effects(const IR_Instruction& inst) const {
    switch (inst.op) {
        case IR_Op::Store:
        case IR_Op::Global_store:
        case IR_Op::Ret:
        case IR_Op::Jump:
        case IR_Op::Branch:
        case IR_Op::Label:
        case IR_Op::Call:
            return true;
        default:
            return false;
    }
}

/// @brief collects the set of all registers that are read by any instruction
std::unordered_set<IR_Reg> IR_Optimiser::collect_used_regs(const IR_Function& func) const {
    /*
    check which registers are used across the function, so we can identify dead stores (stores to slots that are never loaded from again)
    loop over all instructions, and for each instruction, 
    add src1 and src2 to the set of used registers if they are not IR_REG_NONE. 
    Also check call arguments.
    */

    std::unordered_set<IR_Reg> used;
    for (const auto& inst : func.instructions) {
        if (inst.src1 != IR_REG_NONE) used.insert(inst.src1);
        if (inst.src2 != IR_REG_NONE) used.insert(inst.src2);
        // Call arguments
        for (IR_Reg arg : inst.args) {
            used.insert(arg);
        }
    }
    return used;
}

// ─────────────────────────────────────────────
// Pass 1: Constant folding
//   const a; const b; add %a, %b  →  const (a+b)
// ─────────────────────────────────────────────

/// @brief folds operations on two constants into a single constant instruction
bool IR_Optimiser::constant_folding(IR_Function& func) {

    /*
    For each instruction, if it's a binary op and both src operands are known constants
    check the type of the constants (int or float) and the specific op, 
    and if it's supported for that type, 
    compute the result and replace the instruction with a const instruction with the computed value.
    */

    bool changed = false;

    // Map register → known constant value
    std::unordered_map<IR_Reg, i64> constants;
    std::unordered_map<IR_Reg, double> float_constants;

    for (auto& inst : func.instructions) {
        if (inst.op == IR_Op::Const_int && inst.dst != IR_REG_NONE) {
            constants[inst.dst] = inst.imm;
        }
        if (inst.op == IR_Op::Const_float && inst.dst != IR_REG_NONE) {
            float_constants[inst.dst] = inst.fimm;
        }

        // Binary: both operands are constants → fold
        if (is_arithmetic(inst.op) || is_comparison(inst.op)) {
            // Try float folding first
            auto fi1 = float_constants.find(inst.src1);
            auto fi2 = float_constants.find(inst.src2);
            if (fi1 != float_constants.end() && fi2 != float_constants.end()) {
                double a = fi1->second;
                double b = fi2->second;

                // Comparisons produce integer results
                if (is_comparison(inst.op)) {
                    i64 result = 0;
                    switch (inst.op) {
                        case IR_Op::Eq:  result = (a == b) ? 1 : 0; break;
                        case IR_Op::Neq: result = (a != b) ? 1 : 0; break;
                        case IR_Op::Lt:  result = (a < b)  ? 1 : 0; break;
                        case IR_Op::Gt:  result = (a > b)  ? 1 : 0; break;
                        case IR_Op::Le:  result = (a <= b) ? 1 : 0; break;
                        case IR_Op::Ge:  result = (a >= b) ? 1 : 0; break;
                        case IR_Op::Logical_and: result = (a && b) ? 1 : 0; break;
                        case IR_Op::Logical_or:  result = (a || b) ? 1 : 0; break;
                        default: continue;
                    }
                    inst.op   = IR_Op::Const_int;
                    inst.imm  = result;
                    inst.src1 = IR_REG_NONE;
                    inst.src2 = IR_REG_NONE;
                    constants[inst.dst] = result;
                    changed = true;
                } else {
                    // Arithmetic — only Add/Sub/Mul/Div for floats
                    double result = 0.0;
                    switch (inst.op) {
                        case IR_Op::Add: result = a + b; break;
                        case IR_Op::Sub: result = a - b; break;
                        case IR_Op::Mul: result = a * b; break;
                        case IR_Op::Div: result = (b != 0.0) ? a / b : 0.0; break;
                        default: continue; // Skip Mod, bitwise ops for floats
                    }
                    inst.op   = IR_Op::Const_float;
                    inst.fimm = result;
                    inst.src1 = IR_REG_NONE;
                    inst.src2 = IR_REG_NONE;
                    float_constants[inst.dst] = result;
                    changed = true;
                }
            }
            // Integer folding
            else {
                auto it1 = constants.find(inst.src1);
                auto it2 = constants.find(inst.src2);
                if (it1 != constants.end() && it2 != constants.end()) {
                    i64 a = it1->second;
                    i64 b = it2->second;
                    i64 result = 0;

                    switch (inst.op) {
                        case IR_Op::Add: result = a + b; break;
                        case IR_Op::Sub: result = a - b; break;
                        case IR_Op::Mul: result = a * b; break;
                        case IR_Op::Div: result = (b != 0) ? a / b : 0; break;
                        case IR_Op::Mod: result = (b != 0) ? a % b : 0; break;
                        case IR_Op::And: result = a & b; break;
                        case IR_Op::Or:  result = a | b; break;
                        case IR_Op::Xor: result = a ^ b; break;
                        case IR_Op::Shl: result = a << b; break;
                        case IR_Op::Shr: result = a >> b; break;
                        case IR_Op::Eq:  result = (a == b) ? 1 : 0; break;
                        case IR_Op::Neq: result = (a != b) ? 1 : 0; break;
                        case IR_Op::Lt:  result = (a < b)  ? 1 : 0; break;
                        case IR_Op::Gt:  result = (a > b)  ? 1 : 0; break;
                        case IR_Op::Le:  result = (a <= b) ? 1 : 0; break;
                        case IR_Op::Ge:  result = (a >= b) ? 1 : 0; break;
                        case IR_Op::Logical_and: result = (a && b) ? 1 : 0; break;
                        case IR_Op::Logical_or:  result = (a || b) ? 1 : 0; break;
                        default: continue;
                    }

                    inst.op   = IR_Op::Const_int;
                    inst.imm  = result;
                    inst.src1 = IR_REG_NONE;
                    inst.src2 = IR_REG_NONE;
                    constants[inst.dst] = result;
                    changed = true;
                }
            }
        }

        // Unary: operand is constant → fold
        if (is_unary(inst.op)) {
            // Float unary
            auto fi = float_constants.find(inst.src1);
            if (fi != float_constants.end()) {
                double a = fi->second;
                if (inst.op == IR_Op::Neg) {
                    inst.op   = IR_Op::Const_float;
                    inst.fimm = -a;
                    inst.src1 = IR_REG_NONE;
                    float_constants[inst.dst] = -a;
                    changed = true;
                } else if (inst.op == IR_Op::Logical_not) {
                    inst.op   = IR_Op::Const_int;
                    inst.imm  = (a == 0.0) ? 1 : 0;
                    inst.src1 = IR_REG_NONE;
                    constants[inst.dst] = inst.imm;
                    changed = true;
                }
                continue;
            }
            // Integer unary
            auto it = constants.find(inst.src1);
            if (it != constants.end()) {
                i64 a = it->second;
                i64 result = 0;

                switch (inst.op) {
                    case IR_Op::Neg:         result = -a; break;
                    case IR_Op::Bitwise_not: result = ~a; break;
                    case IR_Op::Logical_not: result = !a ? 1 : 0; break;
                    default: continue;
                }

                inst.op   = IR_Op::Const_int;
                inst.imm  = result;
                inst.src1 = IR_REG_NONE;
                constants[inst.dst] = result;
                changed = true;
            }
        }
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 2: Constant propagation
//   If %r = const K, replace uses of %r in
//   arithmetic src operands with a fresh const
// ─────────────────────────────────────────────

/// @brief propagates known constant values through store/load chains
bool IR_Optimiser::constant_propagation(IR_Function& func) {
    // This pass is handled implicitly by constant folding
    // tracking constants through the map. The main extra thing
    // we do here is propagate through loads: if we know a slot
    // was last stored with a constant, a load from that slot
    // yields that constant.

    bool changed = false;

    // slot register → last stored constant value (if known)
    std::unordered_map<IR_Reg, i64> slot_constants;
    std::unordered_map<IR_Reg, double> slot_float_constants;
    // register → known constant
    std::unordered_map<IR_Reg, i64> reg_constants;
    std::unordered_map<IR_Reg, double> reg_float_constants;

    for (u64 i = 0; i < func.instructions.size(); ++i) {
        auto& inst = func.instructions[i];

        if (inst.op == IR_Op::Const_int && inst.dst != IR_REG_NONE) {
            reg_constants[inst.dst] = inst.imm;
        }
        if (inst.op == IR_Op::Const_float && inst.dst != IR_REG_NONE) {
            reg_float_constants[inst.dst] = inst.fimm;
        }

        // Track stores of known constants to slots
        if (inst.op == IR_Op::Store) {
            // Float constant?
            auto fi = reg_float_constants.find(inst.src1);
            if (fi != reg_float_constants.end()) {
                slot_float_constants[inst.src2] = fi->second;
                slot_constants.erase(inst.src2);
            } else {
                auto it = reg_constants.find(inst.src1);
                if (it != reg_constants.end()) {
                    slot_constants[inst.src2] = it->second;
                    slot_float_constants.erase(inst.src2);
                } else {
                    // Unknown value stored → invalidate
                    slot_constants.erase(inst.src2);
                    slot_float_constants.erase(inst.src2);
                }
            }
        }

        // Propagate through loads
        if (inst.op == IR_Op::Load) {
            // Float constant in slot?
            auto fi = slot_float_constants.find(inst.src1);
            if (fi != slot_float_constants.end()) {
                inst.op   = IR_Op::Const_float;
                inst.fimm = fi->second;
                inst.src1 = IR_REG_NONE;
                reg_float_constants[inst.dst] = fi->second;
                changed = true;
            } else {
                auto it = slot_constants.find(inst.src1);
                if (it != slot_constants.end()) {
                    inst.op   = IR_Op::Const_int;
                    inst.imm  = it->second;
                    inst.src1 = IR_REG_NONE;
                    reg_constants[inst.dst] = it->second;
                    changed = true;
                }
            }
        }

        // Labels / branches invalidate slot tracking (control flow merge)
        if (inst.op == IR_Op::Label || inst.op == IR_Op::Branch ||
            inst.op == IR_Op::Jump) {
            slot_constants.clear();
            slot_float_constants.clear();
        }

        // Calls may have side effects — invalidate
        if (inst.op == IR_Op::Call) {
            slot_constants.clear();
            slot_float_constants.clear();
        }
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 3: Store-load forwarding
//   store %v -> %slot; ... load %slot  →  use %v
//   (only if no intervening store to %slot or label)
// ─────────────────────────────────────────────

/// @brief replaces a load from a slot with the value that was last stored into it
bool IR_Optimiser::store_load_forwarding(IR_Function& func) {
    /*
    For each load instruction, check if the slot it loads from was last stored with a known value (register).
    If so, we can replace the load with a "copy" of that register. Since we don't have a copy/move instruction, 
    we'll handle this by rewriting all uses of the load's dst register to point to the stored value register directly.
    We also need to make sure there are no intervening stores to the same slot, 
    or any control flow merges (labels/branches) that would invalidate our knowledge of the slot's value.
    */

    bool changed = false;

    // slot → last stored value register
    std::unordered_map<IR_Reg, IR_Reg> last_store;

    for (u64 i = 0; i < func.instructions.size(); ++i) {
        auto& inst = func.instructions[i];

        if (inst.op == IR_Op::Store && inst.src1 != IR_REG_NONE) {
            last_store[inst.src2] = inst.src1;
        }

        if (inst.op == IR_Op::Load) {
            auto it = last_store.find(inst.src1);
            if (it != last_store.end() && it->second != IR_REG_NONE) {
                // Replace load with a "copy" — we just make this register
                // point to the stored value directly. Since we don't have a
                // copy/move instruction, we'll handle this by rewriting
                // all uses of inst.dst → it->second in subsequent instructions.
                IR_Reg old_reg = inst.dst;
                IR_Reg new_reg = it->second;

                // Rewrite future uses
                for (u64 j = i + 1; j < func.instructions.size(); ++j) {
                    auto& future = func.instructions[j];
                    if (future.src1 == old_reg) future.src1 = new_reg;
                    if (future.src2 == old_reg) future.src2 = new_reg;
                    if (future.dst == old_reg) break; // redefined, stop

                    for (auto& arg : future.args) {
                        if (arg == old_reg) arg = new_reg;
                    }
                }

                // Mark load for removal (make it a no-op const that DCE will remove)
                inst.op   = IR_Op::Const_int;
                inst.imm  = 0;
                inst.src1 = IR_REG_NONE;
                // Keep dst so it can be eliminated later
                changed = true;
            }
        }

        // Control flow invalidates tracking
        if (inst.op == IR_Op::Label || inst.op == IR_Op::Branch ||
            inst.op == IR_Op::Jump  || inst.op == IR_Op::Call) {
            last_store.clear();
        }
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 4: Dead store elimination
//   If a store to a slot is followed by another
//   store to the same slot with no intervening
//   load, the first store is dead.
// ─────────────────────────────────────────────

/// @brief removes stores to a slot that are overwritten before being read
bool IR_Optimiser::dead_store_elimination(IR_Function& func) {
    /*
    Track the last store to each stack slot. When we see a second store to the same slot
    without an intervening load from it, the earlier store is dead and can be removed.
    Control flow merges (labels, branches, jumps, calls) invalidate tracking because
    the slot might have been read along a different path.
    */

    bool changed = false;

    // slot → index of last store to that slot
    std::unordered_map<IR_Reg, u64> last_store_idx;
    // slots that have been read since last store
    std::unordered_set<IR_Reg> read_slots;

    std::vector<bool> to_remove(func.instructions.size(), false);

    for (u64 i = 0; i < func.instructions.size(); ++i) {
        const auto& inst = func.instructions[i];

        // A load reads from a slot
        if (inst.op == IR_Op::Load) {
            read_slots.insert(inst.src1);
        }

        // A store writes to a slot
        if (inst.op == IR_Op::Store) {
            IR_Reg slot = inst.src2;
            auto it = last_store_idx.find(slot);
            if (it != last_store_idx.end() && read_slots.find(slot) == read_slots.end()) {
                // Previous store to same slot was never read → dead
                to_remove[it->second] = true;
                changed = true;
            }
            last_store_idx[slot] = i;
            read_slots.erase(slot);
        }

        // Control flow merges invalidate everything
        if (inst.op == IR_Op::Label || inst.op == IR_Op::Branch ||
            inst.op == IR_Op::Jump  || inst.op == IR_Op::Call) {
            last_store_idx.clear();
            read_slots.clear();
        }
    }

    if (changed) {
        std::vector<IR_Instruction> new_insts;
        new_insts.reserve(func.instructions.size());
        for (u64 i = 0; i < func.instructions.size(); ++i) {
            if (!to_remove[i]) {
                new_insts.push_back(std::move(func.instructions[i]));
            }
        }
        func.instructions = std::move(new_insts);
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 5: Dead code elimination
//   Remove instructions whose dst is never used
//   (unless they have side effects).
// ─────────────────────────────────────────────

/// @brief removes instructions whose result register is never used
bool IR_Optimiser::dead_code_elimination(IR_Function& func) {
    /*
    Collect the set of all registers that are read anywhere in the function.
    Then walk every instruction: if it writes to a register that nobody reads
    and it has no side effects, it is dead and can be safely removed.
    */

    bool changed = false;

    auto used = collect_used_regs(func);

    std::vector<IR_Instruction> new_insts;
    new_insts.reserve(func.instructions.size());

    for (auto& inst : func.instructions) {
        // Keep instructions that have side effects
        if (has_side_effects(inst)) {
            new_insts.push_back(std::move(inst));
            continue;
        }

        // Keep instructions whose result is used
        if (inst.dst != IR_REG_NONE && used.find(inst.dst) != used.end()) {
            new_insts.push_back(std::move(inst));
            continue;
        }

        // Alloca is needed if the slot is used
        if (inst.op == IR_Op::Alloca && inst.dst != IR_REG_NONE &&
            used.find(inst.dst) != used.end()) {
            new_insts.push_back(std::move(inst));
            continue;
        }

        // Otherwise, dead — remove
        changed = true;
    }

    func.instructions = std::move(new_insts);
    return changed;
}

// ─────────────────────────────────────────────
// Pass 6: Unreachable code elimination
//   Remove instructions after an unconditional
//   jump or ret, until the next label.
// ─────────────────────────────────────────────

/// @brief removes instructions after an unconditional jump or ret until the next label
bool IR_Optimiser::unreachable_code_elimination(IR_Function& func) {
    /*
    Walk through all instructions linearly. After seeing an unconditional jump or ret,
    mark everything as unreachable until we hit the next label (which could be the
    target of another jump). Marked instructions are stripped out.
    */

    bool changed = false;
    std::vector<bool> to_remove(func.instructions.size(), false);

    bool unreachable = false;

    for (u64 i = 0; i < func.instructions.size(); ++i) {
        const auto& inst = func.instructions[i];

        if (unreachable) {
            if (inst.op == IR_Op::Label) {
                unreachable = false;
            } else {
                to_remove[i] = true;
                compiler_warning("unreachable code", inst);
                changed = true;
            }
        }

        if (inst.op == IR_Op::Jump || inst.op == IR_Op::Ret) {
            unreachable = true;
        }
    }

    if (changed) {
        std::vector<IR_Instruction> new_insts;
        new_insts.reserve(func.instructions.size());
        for (u64 i = 0; i < func.instructions.size(); ++i) {
            if (!to_remove[i])
                new_insts.push_back(std::move(func.instructions[i]));
        }
        func.instructions = std::move(new_insts);
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 7: Redundant jump elimination
//   jump L followed immediately by L: → remove jump
// ─────────────────────────────────────────────

/// @brief removes a jump instruction that targets the label immediately after it
bool IR_Optimiser::redundant_jump_elimination(IR_Function& func) {
    bool changed = false;
    std::vector<bool> to_remove(func.instructions.size(), false);

    for (u64 i = 0; i + 1 < func.instructions.size(); ++i) {
        const auto& inst = func.instructions[i];
        const auto& next = func.instructions[i + 1];

        if (inst.op == IR_Op::Jump && next.op == IR_Op::Label &&
            inst.label_id == next.label_id) {
            to_remove[i] = true;
            changed = true;
        }
    }

    if (changed) {
        std::vector<IR_Instruction> new_insts;
        new_insts.reserve(func.instructions.size());
        for (u64 i = 0; i < func.instructions.size(); ++i) {
            if (!to_remove[i])
                new_insts.push_back(std::move(func.instructions[i]));
        }
        func.instructions = std::move(new_insts);
    }

    return changed;
}

// ─────────────────────────────────────────────
// Pass 8: Branch folding
//   If a branch condition is a known constant,
//   convert to an unconditional jump.
// ─────────────────────────────────────────────

/// @brief converts conditional branches on known constants into unconditional jumps
bool IR_Optimiser::branch_folding(IR_Function& func) {
    /*
    Track which registers hold known integer constants. When we see a branch
    whose condition register is a known constant, we know which side is taken
    and can replace the branch with an unconditional jump to that target.
    */

    bool changed = false;

    std::unordered_map<IR_Reg, i64> constants;

    for (auto& inst : func.instructions) {
        if (inst.op == IR_Op::Const_int && inst.dst != IR_REG_NONE) {
            constants[inst.dst] = inst.imm;
        }

        if (inst.op == IR_Op::Branch) {
            auto it = constants.find(inst.src1);
            if (it != constants.end()) {
                u32 target = (it->second != 0) ? inst.true_label : inst.false_label;
                inst.op       = IR_Op::Jump;
                inst.label_id = target;
                inst.src1     = IR_REG_NONE;
                changed = true;
            }
        }

        // Labels reset constant tracking for safety
        if (inst.op == IR_Op::Label) {
            constants.clear();
        }
    }

    return changed;
}
