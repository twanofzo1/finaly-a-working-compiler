#include "semantics.hpp"
#include "print_colors.h"
#include "lexer.hpp"
#include "parser.hpp"
#include <fstream>
#include <algorithm>

//_________________________________________ Public methods _________________________________________


/// @brief constructs the semantic analyser with an AST and source text
Semantic_analyser::Semantic_analyser(AST& ast, const std::string& source, const std::string& file_dir)
    : ast(ast), m_source(source), m_has_errors(false), m_inside_function(false), m_analysing_imported_code(false), m_file_dir(file_dir) {}



/// @brief runs all semantic analysis passes and returns true if no errors
/// @return true if the program is semantically valid
void Semantic_analyser::analyse() {
    push_scope(); // global scope
    pass1_collect_functions();
    pass2_analyse(); 
    pop_scope();
}

/// @brief returns whether any semantic errors were encountered during analysis
/// @return  true if errors were encountered, false otherwise
bool Semantic_analyser::has_errors() const {
    return m_has_errors;
}

//_________________________________________ Private methods _________________________________________




/// @brief pushes a new empty scope onto the scope stack
void Semantic_analyser::push_scope() {
    scopes.push_back(Scope{});
}

/// @brief pops the innermost scope from the scope stack
void Semantic_analyser::pop_scope() {
    ASSERT(!scopes.empty(), "tried to pop an empty scope stack");
    // Warn about unused symbols in this scope
    for (const auto& [name, sym] : scopes.back().symbols) {
        if (!sym.used && !sym.decl_token.view.empty() && !sym.is_imported_private) {
            if (sym.type == Symbol_type::Variable) {
                semantic_warning("unused variable '" + name + "'", sym.decl_token);
            } else if (sym.type == Symbol_type::Function && name != "main" && name != "print") {
                semantic_warning("unused function '" + name + "'", sym.decl_token);
            }
        }
    }
    scopes.pop_back();
}

/// @brief declares a symbol in the current scope, errors on redeclaration
/// @param name    the symbol name
/// @param symbol  the symbol info (type, datatype, etc.)
/// @return true if successfully declared
bool Semantic_analyser::declare(const std::string& name, const Symbol& symbol) {
    ASSERT(!scopes.empty(), "no scope to declare into");
    auto& current = scopes.back().symbols;
    if (current.find(name) != current.end()) {
        semantic_error("redeclaration of '" + name + "'");
        return false;
    }
    current[name] = symbol;
    return true;
}

/// @brief declares a symbol in the current scope with a token for error location
/// @param name    the symbol name
/// @param symbol  the symbol info
/// @param token   source token used for error reporting
/// @return true if successfully declared
bool Semantic_analyser::declare(const std::string& name, const Symbol& symbol, Token token) {
    ASSERT(!scopes.empty(), "no scope to declare into");
    auto& current = scopes.back().symbols;
    if (current.find(name) != current.end()) {
        semantic_error("redeclaration of '" + name + "'", token);
        return false;
    }
    Symbol sym = symbol;
    sym.decl_token = token;
    current[name] = sym;
    return true;
}

/// @brief looks up a symbol by name, searching from innermost to outermost scope
/// @return pointer to the symbol, or nullptr if not found
Symbol* Semantic_analyser::lookup(const std::string& name) {
    // walk from innermost scope outward
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->symbols.find(name);
        if (found != it->symbols.end()) {
            return &found->second;
        }
    }
    return nullptr;
}



/// @brief reports a semantic error (no source location)
void Semantic_analyser::semantic_error(const std::string& msg) {
    set_terminal_color(Terminal_color::Red);
    std::cerr << "Semantic error: " << msg  << std::endl;
    set_terminal_color(Terminal_color::Default);
    m_has_errors = true;
}

/// @brief reports a semantic error with source location from a token
void Semantic_analyser::semantic_error(const std::string& msg, Token token) {
    set_terminal_color(Terminal_color::Red);
    std::cerr << "Semantic error on line " << token.get_line(m_source)
              << " pos " << token.get_pos(m_source)
              << ": " << msg << std::endl;
    set_terminal_color(Terminal_color::Default);
    token.print_line(m_source);
    m_has_errors = true;
}

void Semantic_analyser::semantic_warning(const std::string& msg, Token token) {
    set_terminal_color(Terminal_color::Yellow);
    std::cerr << "Semantic warning on line " << token.get_line(m_source)
              << " pos " << token.get_pos(m_source)
              << ": " << msg << std::endl;
    set_terminal_color(Terminal_color::Default);
    token.print_line(m_source);
}



/// @brief tries to find the best source token for an AST node (for error messages)
Token Semantic_analyser::get_token(const AST_index& node) {
    switch (node.type) {
        case AST_index_type::Identifier:
            if (node.index < ast.identifier_tokens.size())
                return ast.identifier_tokens[node.index];
            break;
        case AST_index_type::Integer:
            if (node.index < ast.integer_tokens.size())
                return ast.integer_tokens[node.index];
            break;
        case AST_index_type::Float_literal:
            if (node.index < ast.float_tokens.size())
                return ast.float_tokens[node.index];
            break;
        case AST_index_type::Binary_expression: {
            const Binary_expression& b = ast.binary_expressions[node.index];
            return get_token(b.lhs); // point to left operand
        }
        case AST_index_type::Unary_expression: {
            const Unary_expression& u = ast.unary_expressions[node.index];
            return get_token(u.operand);
        }
        case AST_index_type::Assignment_expression: {
            const Assignment_expression& a = ast.assignment_expressions[node.index];
            return get_token(a.target);
        }
        case AST_index_type::Call_expression: {
            const Call_expression& c = ast.call_expressions[node.index];
            return get_token(c.callee);
        }
        case AST_index_type::Variable_declaration: {
            const Variable_declaration& v = ast.variable_declarations[node.index];
            return get_token(v.name);
        }
        case AST_index_type::Function_declaration: {
            const Function_declaration& f = ast.function_declarations[node.index];
            return get_token(f.identifier);
        }
        case AST_index_type::Return_statement: {
            const Return_statement& r = ast.return_statements[node.index];
            if (r.value.type != AST_index_type::Invalid)
                return get_token(r.value);
            break;
        }
        default:
            break;
    }
    return Token(); // fallback – no location
}



/// @brief collects all top-level function and global variable declarations
void Semantic_analyser::pass1_collect_functions() {
    /* 
    for each statement in the global block (index 0):
         if it's a function declaration: 
            add it to the symbol table for forward reference

         if it's a variable declaration: 
            add it to the symbol table for forward reference 
    */

    // ── Register built-in functions ──
    {
        // print(value) — prints any value to stdout followed by a newline
        Symbol print_sym{};
        print_sym.type        = Symbol_type::Function;
        print_sym.is_const    = true;
        print_sym.param_count = 1;
        // param_types left empty → accepts any type
        // return_type left as Invalid → void
        declare("print", print_sym);
    }

    const Block_statement& root = ast.block_statements[0];

    // First pass: collect import indices (we can't iterate and modify at the same time)
    std::vector<u32> import_indices;
    for (const AST_index& stmt : root.statements) {
        if (stmt.type == AST_index_type::Import_declaration) {
            import_indices.push_back(stmt.index);
        }
    }
    // Process all imports (this adds new statements to the root block)
    for (u32 idx : import_indices) {
        process_import(idx);
    }

    // Re-read root since process_import may have grown the block_statements vector
    const Block_statement& root2 = ast.block_statements[0];

    for (const AST_index& stmt : root2.statements) {

        // Collect struct declarations first so they can be used as types
        if (stmt.type == AST_index_type::Struct_declaration) {
            const Struct_declaration& sdecl = ast.struct_declarations[stmt.index];
            const std::string& sname = ast.identifiers[sdecl.name.index];
            if (m_structs.count(sname)) continue; // already imported
            analyse_struct_decl(stmt.index);
            continue;
        }

        if (stmt.type == AST_index_type::Function_declaration) {
            /*
                get the function declaration node from ast
                extract the function name
                extract the parameter types 
                extract the return type
                then declare a symbol for this function with this info.
            */

            const Function_declaration& func = ast.function_declarations[stmt.index];
            const std::string& name = ast.identifiers[func.identifier.index];
            Token tok = get_token(func.identifier);

            // Skip if already declared (e.g. from an import)
            if (lookup(name)) continue;

            Symbol sym{};
            sym.type        = Symbol_type::Function;
            sym.is_const    = true;  // functions are implicitly const
            sym.return_type = func.return_type;
            sym.param_count = static_cast<u32>(func.datatypes.size());
            sym.param_types = func.datatypes;

            declare(name, sym, tok);
            continue;
        }
       
        if (stmt.type == AST_index_type::Variable_declaration) {

            /*
                get the variable declaration node from ast
                extract the variable name
                check if it has an explicit type or if we need to infer it
                infer the variable type if needed 
                    check the initialiser expression's type (if any) and use that as the variable's type
                extract const vs var info
                then declare a symbol for this variable with this info.
            */

            const Variable_declaration& decl = ast.variable_declarations[stmt.index];
            const std::string& name = ast.identifiers[decl.name.index];
            Token tok = get_token(decl.name);

            // Skip if already declared (e.g. from an import)
            if (lookup(name)) continue;

            AST_index resolved_type = decl.datatype;

            // Infer type from initialiser if needed
            if (resolved_type.type == AST_index_type::Invalid) {
                if (decl.value.type == AST_index_type::Integer) {
                    // Default integer literal type is i32
                    ast.datatypes.push_back(Datatype(Datatype_kind::Signed_int, 32));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                } else if (decl.value.type == AST_index_type::Float_literal) {
                    ast.datatypes.push_back(Datatype(Datatype_kind::Float, 64));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                } else if (decl.value.type == AST_index_type::String_literal) {
                    ast.datatypes.push_back(Datatype(Datatype_kind::String, 64));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                }
            }

            Symbol sym{};
            sym.type     = Symbol_type::Variable;
            sym.datatype = resolved_type;
            sym.is_const = decl.is_const;
            declare(name, sym, tok);
            continue;
        }

    }
}

void Semantic_analyser::pass2_analyse(){
    // global block is at index 0
    analyse_block(0,false); 
}


/// @brief dispatches analysis for any AST node, returns the resolved type for expressions
AST_index Semantic_analyser::analyse_node(const AST_index& node) {
    switch (node.type) {
        case AST_index_type::Block_statement:
            analyse_block(node.index);
            return AST_index();

        case AST_index_type::Function_declaration:
            analyse_function(node.index);
            return AST_index();

        case AST_index_type::Variable_declaration:
            analyse_variable_decl(node.index);
            return AST_index();

        case AST_index_type::If_statement:
            analyse_if(node.index);
            return AST_index();

        case AST_index_type::For_statement:
            analyse_for(node.index);
            return AST_index();

        case AST_index_type::Return_statement:
            analyse_return(node.index);
            return AST_index();

        // Expressions – delegate to expression analyser which returns a type
        case AST_index_type::Integer:
        case AST_index_type::Float_literal:
        case AST_index_type::Identifier:
        case AST_index_type::Binary_expression:
        case AST_index_type::Unary_expression:
        case AST_index_type::Assignment_expression:
            return analyse_expression(node);

        case AST_index_type::Call_expression:
            return analyse_expression(node);

        case AST_index_type::Datatype:
            return node; // datatypes resolve to themselves

        case AST_index_type::Struct_declaration:
            // Already handled in pass1, skip
            return AST_index();

        case AST_index_type::Member_access:
            return analyse_expression(node);

        case AST_index_type::Import_declaration:
            // Already handled in pass1, skip
            return AST_index();

        case AST_index_type::Invalid:
        default:
            return AST_index();
    }
}

// ── Block ──

/// @brief analyses all statements in a block, optionally pushing a new scope
/// @param block_index  index into ast.block_statements
/// @param new_scope    if true, creates a new scope for this block
AST_index Semantic_analyser::analyse_block(u32 block_index, bool new_scope) {
    const Block_statement& block = ast.block_statements[block_index];

    if (new_scope) push_scope();

    for (const AST_index& stmt : block.statements) {
        analyse_node(stmt);
    }

    if (new_scope) pop_scope();
    return AST_index();
}

// ── Function declaration ──

/// @brief analyses a function declaration: declares params, checks body
void Semantic_analyser::analyse_function(u32 func_index) {
    const Function_declaration& func = ast.function_declarations[func_index];

    // The function itself was already registered in pass 1.
    // Now analyse its body in a new scope.
    push_scope();

    // Declare parameters as local variables
    for (u64 i = 0; i < func.names.size(); ++i) {
        const std::string& param_name = ast.identifiers[func.names[i].index];
        Token tok = get_token(func.names[i]);
        Symbol param{};
        param.type     = Symbol_type::Variable;
        param.datatype = (i < func.datatypes.size()) ? func.datatypes[i] : AST_index();
        param.is_const = false;
        declare(param_name, param, tok);
    }

    // Track which function we're inside for return-type checks
    AST_index prev_return = m_current_return_type;
    bool prev_inside      = m_inside_function;
    bool prev_imported    = m_analysing_imported_code;
    m_current_return_type  = func.return_type;
    m_inside_function      = true;

    // If this function came from an import, allow access to imported-private symbols
    const std::string& func_name = ast.identifiers[func.identifier.index];
    if (m_imported_function_names.count(func_name)) {
        m_analysing_imported_code = true;
    }

    // Analyse the function body (don't push another scope – we just did)
    analyse_block(func.block.index, false);

    m_current_return_type     = prev_return;
    m_inside_function         = prev_inside;
    m_analysing_imported_code = prev_imported;

    pop_scope();
}

// ── Variable declaration ──

/// @brief analyses a variable declaration: type-checks initialiser, infers type if needed
void Semantic_analyser::analyse_variable_decl(u32 var_index) {
    const Variable_declaration& decl = ast.variable_declarations[var_index];
    const std::string& name = ast.identifiers[decl.name.index];
    Token tok = get_token(decl.name);

    AST_index resolved_type = decl.datatype; // explicit type if any

    // Analyse the initialiser expression
    if (decl.value.type != AST_index_type::Invalid) {
        AST_index init_type = analyse_expression(decl.value);

        // If explicit type given, check compatibility
        if (resolved_type.type == AST_index_type::Datatype &&
            init_type.type == AST_index_type::Datatype) {
            const Datatype& declared = ast.datatypes[resolved_type.index];
            const Datatype& inferred = ast.datatypes[init_type.index];
            if (declared.kind != inferred.kind) {
                semantic_error("type mismatch in declaration of '" + name +
                               "': declared type kind differs from initialiser type kind", tok);
            } else if (inferred.bit_width > declared.bit_width) {
                semantic_warning("initialiser for '" + name +
                               "' may lose precision (wider -> narrower)", tok);
            }
        }

        // If type was inferred (:= syntax), adopt the initialiser type
        if (resolved_type.type == AST_index_type::Invalid) {
            resolved_type = init_type;
        }
    } else if (resolved_type.type == AST_index_type::Invalid) {
        semantic_error("variable '" + name + "' has no type and no initialiser", tok);
    }

    Symbol sym{};
    sym.type     = Symbol_type::Variable;
    sym.datatype = resolved_type;
    sym.is_const = decl.is_const;

    // If already declared in this scope (e.g. top-level global from pass1), skip redeclaration
    auto& current_scope = scopes.back().symbols;
    if (current_scope.find(name) != current_scope.end()) {
        return; // already in this scope, skip
    }

    // Check outer scopes for shadowing rules:
    // only imported-private symbols may be shadowed by local variables
    Symbol* existing = lookup(name);
    if (existing && !existing->is_imported_private) {
        // This is a regular (own-file or pub-imported) global — cannot shadow it
        if (scopes.size() > 1) { // only error if we're in a nested scope
            semantic_error("cannot shadow global variable '" + name + "'", tok);
            return;
        }
    }

    declare(name, sym, tok);
}

// ── If statement ──

/// @brief analyses an if statement: checks condition and both branches
void Semantic_analyser::analyse_if(u32 if_index) {
    const If_statement& stmt = ast.if_statements[if_index];

    analyse_expression(stmt.condition);

    if (stmt.true_condition.type != AST_index_type::Invalid)
        analyse_node(stmt.true_condition);

    if (stmt.false_condition.type != AST_index_type::Invalid)
        analyse_node(stmt.false_condition);
}

// ── For statement ──

/// @brief analyses a for loop: checks init, condition, post, and body in a shared scope
void Semantic_analyser::analyse_for(u32 for_index) {
    const For_statement& stmt = ast.for_statements[for_index];

    push_scope(); // init variable lives inside the for scope

    if (stmt.init.type != AST_index_type::Invalid)
        analyse_node(stmt.init);

    if (stmt.condition.type != AST_index_type::Invalid)
        analyse_expression(stmt.condition);

    if (stmt.post.type != AST_index_type::Invalid)
        analyse_node(stmt.post);

    if (stmt.block.type != AST_index_type::Invalid)
        analyse_block(stmt.block.index, false); // shares the for scope

    pop_scope();
}

// ── Return statement ──

/// @brief analyses a return statement: checks value against the function return type
void Semantic_analyser::analyse_return(u32 ret_index) {
    const Return_statement& ret = ast.return_statements[ret_index];
    Token tok = (ret.value.type != AST_index_type::Invalid)
                    ? get_token(ret.value) : Token();

    if (!m_inside_function) {
        semantic_error("'return' used outside of a function", tok);
        return;
    }

    if (ret.value.type != AST_index_type::Invalid) {
        AST_index val_type = analyse_expression(ret.value);

        // Check against the declared return type
        if (m_current_return_type.type == AST_index_type::Datatype &&
            val_type.type == AST_index_type::Datatype) {
            const Datatype& expected = ast.datatypes[m_current_return_type.index];
            const Datatype& actual   = ast.datatypes[val_type.index];
            if (expected.kind != actual.kind) {
                semantic_error("return type mismatch: function expects a different type kind", tok);
            }
        } else if (m_current_return_type.type == AST_index_type::Datatype) {
            const Datatype& expected = ast.datatypes[m_current_return_type.index];
            if (expected.kind == Datatype_kind::Void) {
                semantic_error("void function should not return a value", tok);
            }
        }
    } else {
        // bare return – only valid for void functions
        if (m_current_return_type.type == AST_index_type::Datatype) {
            const Datatype& expected = ast.datatypes[m_current_return_type.index];
            if (expected.kind != Datatype_kind::Void) {
                semantic_error("non-void function must return a value");
            }
        }
    }
}



/// @brief analyses an expression and returns its resolved data type
AST_index Semantic_analyser::analyse_expression(const AST_index& node) {
    switch (node.type) {
        case AST_index_type::Integer: {
            // Integer literals default to i32
            ast.datatypes.push_back(Datatype(Datatype_kind::Signed_int, 32));
            return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
        }

        case AST_index_type::Float_literal: {
            // Float literals default to f64
            ast.datatypes.push_back(Datatype(Datatype_kind::Float, 64));
            return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
        }

        case AST_index_type::String_literal: {
            // String literals are pointers (64-bit)
            ast.datatypes.push_back(Datatype(Datatype_kind::String, 64));
            return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
        }

        case AST_index_type::Identifier: {
            const std::string& name = ast.identifiers[node.index];
            Token tok = get_token(node);
            Symbol* sym = lookup(name);
            if (!sym) {
                semantic_error("use of undeclared identifier '" + name + "'", tok);
                return AST_index();
            }
            // Reject access to imported-private symbols from non-imported code
            if (sym->is_imported_private && !m_analysing_imported_code) {
                semantic_error("'" + name + "' is private in its module and cannot be accessed here", tok);
                return AST_index();
            }
            sym->used = true;
            if (sym->type == Symbol_type::Function)
                return sym->return_type;
            return sym->datatype;
        }

        case AST_index_type::Binary_expression:
            return analyse_binary(node.index);

        case AST_index_type::Unary_expression:
            return analyse_unary(node.index);

        case AST_index_type::Assignment_expression:
            return analyse_assignment(node.index);

        case AST_index_type::Call_expression:
            return analyse_call(node.index);

        case AST_index_type::Member_access:
            return analyse_member_access(node.index);

        default:
            return AST_index();
    }
}


/// @brief analyses a binary expression: checks operand types, returns result type
AST_index Semantic_analyser::analyse_binary(u32 bin_index) {
    const Binary_expression& bin = ast.binary_expressions[bin_index];

    AST_index lhs_type = analyse_expression(bin.lhs);
    AST_index rhs_type = analyse_expression(bin.rhs);

    // Comparison / logical operators always yield bool
    switch (bin.opp) {
        case Token_type::Equal:
        case Token_type::Not_equal:
        case Token_type::Less_than:
        case Token_type::Greater_than:
        case Token_type::Less_equal:
        case Token_type::Greater_equal:
        case Token_type::Logical_and:
        case Token_type::Logical_or: {
            ast.datatypes.push_back(Datatype(Datatype_kind::Bool, 1));
            return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
        }
        default:
            break;
    }

    // For arithmetic / bitwise, check kind compatibility and use the wider type
    if (lhs_type.type == AST_index_type::Datatype &&
        rhs_type.type == AST_index_type::Datatype) {
        const Datatype& l = ast.datatypes[lhs_type.index];
        const Datatype& r = ast.datatypes[rhs_type.index];

        if (l.kind != r.kind) {
            Token tok = get_token(bin.lhs);
            semantic_error("binary operator applied to incompatible types", tok);
        }

        // Result is the wider of the two
        u32 width = std::max(l.bit_width, r.bit_width);
        ast.datatypes.push_back(Datatype(l.kind, width));
        return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
    }

    return lhs_type.type != AST_index_type::Invalid ? lhs_type : rhs_type;
}


/// @brief analyses a unary expression: checks operand type, returns result type
AST_index Semantic_analyser::analyse_unary(u32 un_index) {
    const Unary_expression& un = ast.unary_expressions[un_index];
    AST_index operand_type = analyse_expression(un.operand);

    if (un.opp == Token_type::Logical_not) {
        ast.datatypes.push_back(Datatype(Datatype_kind::Bool, 1));
        return AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
    }

    // Negation / bitwise-not preserve the operand type
    return operand_type;
}


/// @brief analyses an assignment: checks lvalue, const-ness, and type compatibility
AST_index Semantic_analyser::analyse_assignment(u32 assign_index) {
    const Assignment_expression& assign = ast.assignment_expressions[assign_index];

    // The target must be an lvalue (identifier or member access)
    if (assign.target.type == AST_index_type::Member_access) {
        // Member access assignment (e.g. p.x = 5)
        AST_index member_type = analyse_member_access(assign.target.index);
        AST_index val_type = analyse_expression(assign.value);

        if (member_type.type == AST_index_type::Datatype &&
            val_type.type == AST_index_type::Datatype) {
            const Datatype& target_dt = ast.datatypes[member_type.index];
            const Datatype& value_dt  = ast.datatypes[val_type.index];
            if (target_dt.kind != value_dt.kind) {
                Token tok = get_token(assign.target);
                semantic_error("type mismatch in member assignment", tok);
            }
        }
        return member_type;
    }

    if (assign.target.type != AST_index_type::Identifier) {
        Token tok = get_token(assign.target);
        semantic_error("assignment target is not an lvalue", tok);
        return AST_index();
    }

    const std::string& name = ast.identifiers[assign.target.index];
    Token tok = get_token(assign.target);
    Symbol* sym = lookup(name);
    if (!sym) {
        semantic_error("assignment to undeclared variable '" + name + "'", tok);
        return AST_index();
    }
    // Reject access to imported-private symbols from non-imported code
    if (sym->is_imported_private && !m_analysing_imported_code) {
        semantic_error("'" + name + "' is private in its module and cannot be accessed here", tok);
        return AST_index();
    }
    sym->used = true;
    if (sym->is_const) {
        semantic_error("cannot assign to const variable '" + name + "'", tok);
    }
    if (sym->type == Symbol_type::Function) {
        semantic_error("cannot assign to function '" + name + "'", tok);
    }

    AST_index val_type = analyse_expression(assign.value);

    // Type-check the value against the variable's declared type
    if (sym->datatype.type == AST_index_type::Datatype &&
        val_type.type == AST_index_type::Datatype) {
        const Datatype& target_dt = ast.datatypes[sym->datatype.index];
        const Datatype& value_dt  = ast.datatypes[val_type.index];
        if (target_dt.kind != value_dt.kind) {
            semantic_error("type mismatch in assignment to '" + name + "'", tok);
        }
    }

    return sym->datatype;
}


/// @brief analyses a function call: checks callee exists, arg count and types match
AST_index Semantic_analyser::analyse_call(u32 call_index) {
    const Call_expression& call = ast.call_expressions[call_index];

    // The callee must be an identifier that resolves to a function
    if (call.callee.type != AST_index_type::Identifier) {
        Token tok = get_token(call.callee);
        semantic_error("call target is not a function name", tok);
        return AST_index();
    }

    const std::string& name = ast.identifiers[call.callee.index];
    Token tok = get_token(call.callee);
    Symbol* sym = lookup(name);
    if (!sym) {
        semantic_error("call to undeclared function '" + name + "'", tok);
        return AST_index();
    }
    // Reject access to imported-private functions from non-imported code
    if (sym->is_imported_private && !m_analysing_imported_code) {
        semantic_error("'" + name + "' is private in its module and cannot be called here", tok);
        return AST_index();
    }
    sym->used = true;
    if (sym->type != Symbol_type::Function) {
        semantic_error("'" + name + "' is not a function", tok);
        return AST_index();
    }

    // Check argument count
    if (call.arguments.size() != sym->param_count) {
        semantic_error("function '" + name + "' expects " +
                       std::to_string(sym->param_count) + " arguments but got " +
                       std::to_string(call.arguments.size()), tok);
    }

    // Analyse each argument
    for (u64 i = 0; i < call.arguments.size(); ++i) {
        AST_index arg_type = analyse_expression(call.arguments[i]);

        // Type-check against parameter type if available
        if (i < sym->param_types.size() &&
            sym->param_types[i].type == AST_index_type::Datatype &&
            arg_type.type == AST_index_type::Datatype) {
            const Datatype& expected = ast.datatypes[sym->param_types[i].index];
            const Datatype& actual   = ast.datatypes[arg_type.index];
            if (expected.kind != actual.kind) {
                Token arg_tok = get_token(call.arguments[i]);
                semantic_error("type mismatch for argument " + std::to_string(i + 1) +
                               " of '" + name + "'", arg_tok);
            }
        }
    }

    return sym->return_type;
}


/// @brief analyses a struct declaration: registers the struct type with its fields
void Semantic_analyser::analyse_struct_decl(u32 struct_index) {
    const Struct_declaration& decl = ast.struct_declarations[struct_index];
    const std::string& name = ast.identifiers[decl.name.index];

    if (m_structs.count(name)) {
        Token tok = get_token(decl.name);
        semantic_error("redeclaration of struct '" + name + "'", tok);
        return;
    }

    StructInfo info;
    info.name = name;
    for (u64 i = 0; i < decl.field_names.size(); ++i) {
        info.field_names.push_back(ast.identifiers[decl.field_names[i].index]);
        info.field_types.push_back(decl.field_types[i]);
    }
    m_structs[name] = info;
}


/// @brief analyses a member access expression (e.g. p.x) and returns the member's type
AST_index Semantic_analyser::analyse_member_access(u32 ma_index) {
    const Member_access_expression& ma = ast.member_access_expressions[ma_index];

    // Analyse the object to get its type
    AST_index obj_type = analyse_expression(ma.object);

    if (obj_type.type != AST_index_type::Datatype) {
        Token tok = get_token(ma.object);
        semantic_error("cannot access member on expression with unknown type", tok);
        return AST_index();
    }

    const Datatype& dt = ast.datatypes[obj_type.index];
    if (dt.kind != Datatype_kind::Struct) {
        Token tok = get_token(ma.object);
        semantic_error("member access on non-struct type", tok);
        return AST_index();
    }

    // Look up the struct info
    auto it = m_structs.find(dt.struct_name);
    if (it == m_structs.end()) {
        Token tok = get_token(ma.object);
        semantic_error("unknown struct type '" + dt.struct_name + "'", tok);
        return AST_index();
    }

    const std::string& member_name = ast.identifiers[ma.member.index];
    const StructInfo& info = it->second;

    for (u64 i = 0; i < info.field_names.size(); ++i) {
        if (info.field_names[i] == member_name) {
            return info.field_types[i];
        }
    }

    Token tok = get_token(ma.member);
    semantic_error("struct '" + dt.struct_name + "' has no member '" + member_name + "'", tok);
    return AST_index();
}


/// @brief processes an @import declaration: lexes and parses the imported file,
///        then injects only pub-marked declarations into the current AST and symbol table
void Semantic_analyser::process_import(u32 import_index) {
    const Import_declaration& imp = ast.import_declarations[import_index];
    
    // Resolve file path relative to the current source file's directory
    std::string resolved_path = imp.file_path;
    if (!m_file_dir.empty() && resolved_path[0] != '/') {
        resolved_path = m_file_dir + "/" + resolved_path;
    }

    // Check for circular imports
    if (m_imported_files.count(resolved_path)) {
        return; // already imported, skip
    }
    m_imported_files.insert(resolved_path);

    // Read the imported file
    std::ifstream stream(resolved_path);
    if (!stream.is_open()) {
        semantic_error("cannot open imported file '" + imp.file_path + "'", imp.token);
        return;
    }
    std::string imported_source((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
    imported_source.erase(std::remove(imported_source.begin(), imported_source.end(), '\r'), imported_source.end());

    // Lex the imported file
    Lexer lexer(imported_source);
    lexer.lex();
    if (lexer.has_error()) {
        semantic_error("lexer errors in imported file '" + imp.file_path + "'", imp.token);
        return;
    }

    // Parse the imported file
    Parser parser(lexer.get_tokens(), imported_source);
    parser.parse();
    if (parser.has_error()) {
        semantic_error("parser errors in imported file '" + imp.file_path + "'", imp.token);
        return;
    }
    AST imported_ast = parser.get_AST();

    // Walk the imported file's top-level block and merge only pub declarations
    if (imported_ast.block_statements.empty()) return;
    const Block_statement& imported_root = imported_ast.block_statements[0];

    for (const AST_index& stmt : imported_root.statements) {

        // ── Imported struct declarations ──
        if (stmt.type == AST_index_type::Struct_declaration) {
            const Struct_declaration& sdecl = imported_ast.struct_declarations[stmt.index];

            // Copy the struct into the main AST
            AST_index new_name;
            {
                const std::string& sname = imported_ast.identifiers[sdecl.name.index];
                ast.identifiers.push_back(sname);
                ast.identifier_tokens.push_back(Token());
                new_name = AST_index(AST_index_type::Identifier, ast.identifiers.size() - 1);
            }

            std::vector<AST_index> new_field_types;
            std::vector<AST_index> new_field_names;
            for (u64 i = 0; i < sdecl.field_types.size(); ++i) {
                // Copy field type
                const Datatype& ft = imported_ast.datatypes[sdecl.field_types[i].index];
                ast.datatypes.push_back(ft);
                new_field_types.push_back(AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1));

                // Copy field name
                const std::string& fname = imported_ast.identifiers[sdecl.field_names[i].index];
                ast.identifiers.push_back(fname);
                ast.identifier_tokens.push_back(Token());
                new_field_names.push_back(AST_index(AST_index_type::Identifier, ast.identifiers.size() - 1));
            }

            ast.struct_declarations.push_back(Struct_declaration(new_name, new_field_types, new_field_names, true));
            u32 new_idx = ast.struct_declarations.size() - 1;

            // Register the struct in the semantic analyser
            analyse_struct_decl(new_idx);

            // Add it to the global block so IR/codegen can find it
            ast.block_statements[0].statements.push_back(
                AST_index(AST_index_type::Struct_declaration, new_idx));
            continue;
        }

        // ── Imported function declarations ──
        if (stmt.type == AST_index_type::Function_declaration) {
            const Function_declaration& func = imported_ast.function_declarations[stmt.index];

            // We need to deep-copy the entire function into the main AST
            // This is a recursive process since functions reference blocks, expressions, etc.
            // Use a helper to remap all AST indices from imported_ast to ast

            // For now, we do a full merge of the imported AST data needed by this function
            // Strategy: copy all referenced data from imported_ast into ast, remapping indices

            // Step 1: copy the function name
            const std::string& fname = imported_ast.identifiers[func.identifier.index];
            ast.identifiers.push_back(fname);
            ast.identifier_tokens.push_back(Token());
            AST_index new_name = AST_index(AST_index_type::Identifier, ast.identifiers.size() - 1);

            // Step 2: copy parameter types and names
            std::vector<AST_index> new_param_types;
            std::vector<AST_index> new_param_names;
            for (u64 i = 0; i < func.datatypes.size(); ++i) {
                const Datatype& pdt = imported_ast.datatypes[func.datatypes[i].index];
                ast.datatypes.push_back(pdt);
                new_param_types.push_back(AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1));
            }
            for (u64 i = 0; i < func.names.size(); ++i) {
                const std::string& pname = imported_ast.identifiers[func.names[i].index];
                ast.identifiers.push_back(pname);
                ast.identifier_tokens.push_back(Token());
                new_param_names.push_back(AST_index(AST_index_type::Identifier, ast.identifiers.size() - 1));
            }

            // Step 3: copy return type
            AST_index new_return_type;
            if (func.return_type.type == AST_index_type::Datatype) {
                const Datatype& rdt = imported_ast.datatypes[func.return_type.index];
                ast.datatypes.push_back(rdt);
                new_return_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
            }

            // Step 4: deep-copy the function body block
            // We need a recursive copy function for this
            struct ASTCopier {
                const AST& src;
                AST& dst;

                ASTCopier(const AST& src, AST& dst) : src(src), dst(dst) {}

                AST_index copy(const AST_index& idx) {
                    switch (idx.type) {
                        case AST_index_type::Invalid:
                            return AST_index();

                        case AST_index_type::Integer: {
                            dst.integers.push_back(src.integers[idx.index]);
                            dst.integer_tokens.push_back(Token());
                            return AST_index(AST_index_type::Integer, dst.integers.size() - 1);
                        }
                        case AST_index_type::Float_literal: {
                            dst.floats.push_back(src.floats[idx.index]);
                            dst.float_tokens.push_back(Token());
                            return AST_index(AST_index_type::Float_literal, dst.floats.size() - 1);
                        }
                        case AST_index_type::String_literal: {
                            dst.string_literals.push_back(src.string_literals[idx.index]);
                            dst.string_tokens.push_back(Token());
                            return AST_index(AST_index_type::String_literal, dst.string_literals.size() - 1);
                        }
                        case AST_index_type::Identifier: {
                            dst.identifiers.push_back(src.identifiers[idx.index]);
                            dst.identifier_tokens.push_back(Token());
                            return AST_index(AST_index_type::Identifier, dst.identifiers.size() - 1);
                        }
                        case AST_index_type::Datatype: {
                            dst.datatypes.push_back(src.datatypes[idx.index]);
                            return AST_index(AST_index_type::Datatype, dst.datatypes.size() - 1);
                        }
                        case AST_index_type::Binary_expression: {
                            const Binary_expression& b = src.binary_expressions[idx.index];
                            AST_index lhs = copy(b.lhs);
                            AST_index rhs = copy(b.rhs);
                            dst.binary_expressions.push_back(Binary_expression(lhs, b.opp, rhs));
                            return AST_index(AST_index_type::Binary_expression, dst.binary_expressions.size() - 1);
                        }
                        case AST_index_type::Unary_expression: {
                            const Unary_expression& u = src.unary_expressions[idx.index];
                            AST_index operand = copy(u.operand);
                            dst.unary_expressions.push_back(Unary_expression(u.opp, operand));
                            return AST_index(AST_index_type::Unary_expression, dst.unary_expressions.size() - 1);
                        }
                        case AST_index_type::Assignment_expression: {
                            const Assignment_expression& a = src.assignment_expressions[idx.index];
                            AST_index target = copy(a.target);
                            AST_index value = copy(a.value);
                            dst.assignment_expressions.push_back(Assignment_expression(target, a.opp, value));
                            return AST_index(AST_index_type::Assignment_expression, dst.assignment_expressions.size() - 1);
                        }
                        case AST_index_type::Block_statement: {
                            const Block_statement& blk = src.block_statements[idx.index];
                            std::vector<AST_index> new_stmts;
                            for (const auto& s : blk.statements) {
                                new_stmts.push_back(copy(s));
                            }
                            dst.block_statements.push_back(Block_statement(new_stmts));
                            return AST_index(AST_index_type::Block_statement, dst.block_statements.size() - 1);
                        }
                        case AST_index_type::If_statement: {
                            const If_statement& ifs = src.if_statements[idx.index];
                            AST_index cond = copy(ifs.condition);
                            AST_index t = copy(ifs.true_condition);
                            AST_index f = copy(ifs.false_condition);
                            dst.if_statements.push_back(If_statement(cond, t, f));
                            return AST_index(AST_index_type::If_statement, dst.if_statements.size() - 1);
                        }
                        case AST_index_type::For_statement: {
                            const For_statement& fs = src.for_statements[idx.index];
                            AST_index init = copy(fs.init);
                            AST_index cond = copy(fs.condition);
                            AST_index post = copy(fs.post);
                            AST_index blk = copy(fs.block);
                            dst.for_statements.push_back(For_statement(init, cond, post, blk));
                            return AST_index(AST_index_type::For_statement, dst.for_statements.size() - 1);
                        }
                        case AST_index_type::Function_declaration: {
                            const Function_declaration& fd = src.function_declarations[idx.index];
                            AST_index id = copy(fd.identifier);
                            std::vector<AST_index> dts, nms;
                            for (const auto& d : fd.datatypes) dts.push_back(copy(d));
                            for (const auto& n : fd.names) nms.push_back(copy(n));
                            AST_index blk = copy(fd.block);
                            AST_index rt = copy(fd.return_type);
                            dst.function_declarations.push_back(Function_declaration(id, dts, nms, blk, rt, fd.is_public));
                            return AST_index(AST_index_type::Function_declaration, dst.function_declarations.size() - 1);
                        }
                        case AST_index_type::Return_statement: {
                            const Return_statement& rs = src.return_statements[idx.index];
                            AST_index val = copy(rs.value);
                            dst.return_statements.push_back(Return_statement(val));
                            return AST_index(AST_index_type::Return_statement, dst.return_statements.size() - 1);
                        }
                        case AST_index_type::Variable_declaration: {
                            const Variable_declaration& vd = src.variable_declarations[idx.index];
                            AST_index nm = copy(vd.name);
                            AST_index dt = copy(vd.datatype);
                            AST_index val = copy(vd.value);
                            dst.variable_declarations.push_back(Variable_declaration(nm, dt, val, vd.is_const, vd.is_public));
                            return AST_index(AST_index_type::Variable_declaration, dst.variable_declarations.size() - 1);
                        }
                        case AST_index_type::Call_expression: {
                            const Call_expression& ce = src.call_expressions[idx.index];
                            AST_index callee = copy(ce.callee);
                            std::vector<AST_index> args;
                            for (const auto& a : ce.arguments) args.push_back(copy(a));
                            dst.call_expressions.push_back(Call_expression(callee, args));
                            return AST_index(AST_index_type::Call_expression, dst.call_expressions.size() - 1);
                        }
                        case AST_index_type::Struct_declaration: {
                            const Struct_declaration& sd = src.struct_declarations[idx.index];
                            AST_index nm = copy(sd.name);
                            std::vector<AST_index> fts, fns;
                            for (const auto& ft : sd.field_types) fts.push_back(copy(ft));
                            for (const auto& fn : sd.field_names) fns.push_back(copy(fn));
                            dst.struct_declarations.push_back(Struct_declaration(nm, fts, fns, sd.is_public));
                            return AST_index(AST_index_type::Struct_declaration, dst.struct_declarations.size() - 1);
                        }
                        case AST_index_type::Member_access: {
                            const Member_access_expression& ma = src.member_access_expressions[idx.index];
                            AST_index obj = copy(ma.object);
                            AST_index mem = copy(ma.member);
                            dst.member_access_expressions.push_back(Member_access_expression(obj, mem));
                            return AST_index(AST_index_type::Member_access, dst.member_access_expressions.size() - 1);
                        }
                        default:
                            return AST_index();
                    }
                }
            };

            ASTCopier copier(imported_ast, ast);
            AST_index new_block = copier.copy(func.block);

            // Create the function declaration in the main AST
            Function_declaration new_func(new_name, new_param_types, new_param_names, new_block, new_return_type, true);
            ast.function_declarations.push_back(new_func);
            u32 func_idx = ast.function_declarations.size() - 1;

            // Register the function symbol
            Symbol sym{};
            sym.type        = Symbol_type::Function;
            sym.is_const    = true;
            sym.return_type = new_return_type;
            sym.param_count = static_cast<u32>(new_param_types.size());
            sym.param_types = new_param_types;
            sym.is_imported_private = !func.is_public;
            declare(fname, sym);

            // Track this as an imported function (for visibility enforcement)
            m_imported_function_names.insert(fname);

            // Add to the global block so IR/codegen can find it
            ast.block_statements[0].statements.push_back(
                AST_index(AST_index_type::Function_declaration, func_idx));
            continue;
        }

        // ── Imported variable declarations ──
        if (stmt.type == AST_index_type::Variable_declaration) {
            const Variable_declaration& vdecl = imported_ast.variable_declarations[stmt.index];

            struct ASTCopier {
                const AST& src;
                AST& dst;
                ASTCopier(const AST& src, AST& dst) : src(src), dst(dst) {}
                AST_index copy(const AST_index& idx) {
                    switch (idx.type) {
                        case AST_index_type::Invalid: return AST_index();
                        case AST_index_type::Integer: {
                            dst.integers.push_back(src.integers[idx.index]);
                            dst.integer_tokens.push_back(Token());
                            return AST_index(AST_index_type::Integer, dst.integers.size() - 1);
                        }
                        case AST_index_type::Float_literal: {
                            dst.floats.push_back(src.floats[idx.index]);
                            dst.float_tokens.push_back(Token());
                            return AST_index(AST_index_type::Float_literal, dst.floats.size() - 1);
                        }
                        case AST_index_type::String_literal: {
                            dst.string_literals.push_back(src.string_literals[idx.index]);
                            dst.string_tokens.push_back(Token());
                            return AST_index(AST_index_type::String_literal, dst.string_literals.size() - 1);
                        }
                        case AST_index_type::Identifier: {
                            dst.identifiers.push_back(src.identifiers[idx.index]);
                            dst.identifier_tokens.push_back(Token());
                            return AST_index(AST_index_type::Identifier, dst.identifiers.size() - 1);
                        }
                        case AST_index_type::Datatype: {
                            dst.datatypes.push_back(src.datatypes[idx.index]);
                            return AST_index(AST_index_type::Datatype, dst.datatypes.size() - 1);
                        }
                        case AST_index_type::Binary_expression: {
                            const Binary_expression& b = src.binary_expressions[idx.index];
                            AST_index lhs = copy(b.lhs);
                            AST_index rhs = copy(b.rhs);
                            dst.binary_expressions.push_back(Binary_expression(lhs, b.opp, rhs));
                            return AST_index(AST_index_type::Binary_expression, dst.binary_expressions.size() - 1);
                        }
                        case AST_index_type::Unary_expression: {
                            const Unary_expression& u = src.unary_expressions[idx.index];
                            AST_index operand = copy(u.operand);
                            dst.unary_expressions.push_back(Unary_expression(u.opp, operand));
                            return AST_index(AST_index_type::Unary_expression, dst.unary_expressions.size() - 1);
                        }
                        case AST_index_type::Call_expression: {
                            const Call_expression& ce = src.call_expressions[idx.index];
                            AST_index callee = copy(ce.callee);
                            std::vector<AST_index> args;
                            for (const auto& a : ce.arguments) args.push_back(copy(a));
                            dst.call_expressions.push_back(Call_expression(callee, args));
                            return AST_index(AST_index_type::Call_expression, dst.call_expressions.size() - 1);
                        }
                        case AST_index_type::Member_access: {
                            const Member_access_expression& ma = src.member_access_expressions[idx.index];
                            AST_index obj = copy(ma.object);
                            AST_index mem = copy(ma.member);
                            dst.member_access_expressions.push_back(Member_access_expression(obj, mem));
                            return AST_index(AST_index_type::Member_access, dst.member_access_expressions.size() - 1);
                        }
                        default: return AST_index();
                    }
                }
            };

            ASTCopier copier(imported_ast, ast);

            const std::string& vname = imported_ast.identifiers[vdecl.name.index];
            ast.identifiers.push_back(vname);
            ast.identifier_tokens.push_back(Token());
            AST_index new_name = AST_index(AST_index_type::Identifier, ast.identifiers.size() - 1);

            AST_index new_datatype = copier.copy(vdecl.datatype);
            AST_index new_value = copier.copy(vdecl.value);

            ast.variable_declarations.push_back(Variable_declaration(new_name, new_datatype, new_value, vdecl.is_const, true));
            u32 var_idx = ast.variable_declarations.size() - 1;

            // Resolve type for the symbol
            AST_index resolved_type = new_datatype;
            if (resolved_type.type == AST_index_type::Invalid) {
                if (new_value.type == AST_index_type::Integer) {
                    ast.datatypes.push_back(Datatype(Datatype_kind::Signed_int, 32));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                } else if (new_value.type == AST_index_type::Float_literal) {
                    ast.datatypes.push_back(Datatype(Datatype_kind::Float, 64));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                } else if (new_value.type == AST_index_type::String_literal) {
                    ast.datatypes.push_back(Datatype(Datatype_kind::String, 64));
                    resolved_type = AST_index(AST_index_type::Datatype, ast.datatypes.size() - 1);
                }
            }

            Symbol sym{};
            sym.type     = Symbol_type::Variable;
            sym.datatype = resolved_type;
            sym.is_const = vdecl.is_const;
            sym.is_imported_private = !vdecl.is_public;
            declare(vname, sym);

            // Add to global block
            ast.block_statements[0].statements.push_back(
                AST_index(AST_index_type::Variable_declaration, var_idx));
            continue;
        }
    }
}