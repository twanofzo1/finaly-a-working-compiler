/*
Author: Twan Roodenburg
Date: 22/02/2026
File: optimiser.cpp
Description: 
    the optimiser module, which defines the IR optimisation passes and helper functions.
*/


#pragma once
#include "ir.hpp"
#include "modern_types.h"
#include "print_colors.hpp"
#include <unordered_map>
#include <unordered_set>


/// @brief performs various optimisations on the IR program, such as constant folding, dead code elimination, and unreachable code elimination
class IR_Optimiser {
public:
    IR_Optimiser(IR_Program& program, const std::string& source);
    void optimise();
private:
    IR_Program& m_program;
    const std::string& m_source;

    void compiler_warning(const std::string& msg, const IR_Instruction& inst);
    bool run_pass(IR_Function& func);
    bool constant_folding(IR_Function& func);
    bool constant_propagation(IR_Function& func);
    bool store_load_forwarding(IR_Function& func);
    bool dead_store_elimination(IR_Function& func);
    bool dead_code_elimination(IR_Function& func);
    bool unreachable_code_elimination(IR_Function& func);
    bool redundant_jump_elimination(IR_Function& func);
    bool branch_folding(IR_Function& func);

    bool is_arithmetic(IR_Op op) const;
    bool is_comparison(IR_Op op) const;
    bool is_unary(IR_Op op) const;
    bool has_side_effects(const IR_Instruction& inst) const;

    std::unordered_set<IR_Reg> collect_used_regs(const IR_Function& func) const;
};
