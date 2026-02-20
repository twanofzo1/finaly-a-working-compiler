#pragma once
#include <unordered_map>
#include <string>
#include <vector>
#include <iostream>
#include "ast.hpp"
#include "log.hpp"

/// @brief the symbol type (variable or function)
enum class Symbol_type {
    Variable,
    Function,
};

/// @brief information about a declared struct type (field names and types)
struct StructInfo {
    std::string name;
    std::vector<std::string> field_names;
    std::vector<AST_index> field_types;
};



/// @brief  a symbol in the symbol table, representing a variable or function with its type and other attributes
struct Symbol {
    Symbol() : type(Symbol_type::Variable), datatype(AST_index()), is_const(false), used(false), param_types(), return_type(), param_count(0) {}

    Symbol_type type;
    AST_index datatype;       // resolved type (Datatype node) for variables
    bool is_const;
    bool used;                // whether this symbol has been referenced
    Token decl_token;         // source token of the declaration (for warnings)

    // Function-specific
    std::vector<AST_index> param_types;
    AST_index return_type;
    u32 param_count;
};



/// @brief a scope containing declared symbols
struct Scope {
    std::unordered_map<std::string, Symbol> symbols;
};




/// @brief the semantic analyser, which performs semantic checks and type resolution on the AST
class Semantic_analyser {
private:
    std::vector<Scope> scopes;       //< the stack of scopes for name resolution                                                                  
    AST& ast;                        //< reference to the AST being analysed                                                                  
    const std::string& m_source;     //< the original source code, used for error messages                                                                 
    bool m_has_errors;               //< tracks whether any semantic errors were encountered                                                                 
    AST_index m_current_return_type; //< tracks the return type of the function currently being analyse 
    bool m_inside_function;          //< tracks whether we're currently inside a function (for return statement checks)
    std::unordered_map<std::string, StructInfo> m_structs; //< registry of declared struct types                                                                 

public:
    Semantic_analyser(AST& ast, const std::string& source);
    void analyse();
    bool has_errors() const;

private:
    void push_scope();
    void pop_scope();
    bool declare(const std::string& name, const Symbol& symbol);
    bool declare(const std::string& name, const Symbol& symbol, Token token);
    Symbol* lookup(const std::string& name);

    void semantic_error(const std::string& msg);
    void semantic_error(const std::string& msg, Token token);
    void semantic_warning(const std::string& msg, Token token);

    Token get_token(const AST_index& node);

    void pass1_collect_functions();
    void pass2_analyse();

    AST_index analyse_node(const AST_index& node);
    AST_index analyse_block(u32 block_index, bool new_scope = true);
    void analyse_function(u32 func_index);
    void analyse_variable_decl(u32 var_index);
    void analyse_if(u32 if_index);
    void analyse_for(u32 for_index);
    void analyse_return(u32 ret_index);

    AST_index analyse_expression(const AST_index& node);
    AST_index analyse_binary(u32 bin_index);
    AST_index analyse_unary(u32 un_index);
    AST_index analyse_assignment(u32 assign_index);
    AST_index analyse_call(u32 call_index);
    void analyse_struct_decl(u32 struct_index);
    AST_index analyse_member_access(u32 ma_index);
};




