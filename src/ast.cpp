/*
Author: Twan Roodenburg
Date: 19/02/2026
File: ast.cpp
Description: 
    The abstract syntax tree (AST) for the program, 
    used for representing the structure of the program in a way that is easy to traverse 
    and manipulate during semantic analysis and code generation.
    The AST contains vectors for each type of node in the AST, and the nodes reference each other using AST_index structs
    none of the structs have any functionality beyond storing their data and printing themselves for debugging purposes
*/


#include "ast.hpp"

/// @brief prints the AST_index_type as a human-readable string (for debug output)
std::ostream& operator<<(std::ostream& os, const AST_index_type& type) {
    switch (type) {
        case AST_index_type::Invalid:
            os << "Invalid";
            break;
        case AST_index_type::Integer:
            os << "Integer";
            break;
        case AST_index_type::Float_literal:
            os << "Float_literal";
            break;
        case AST_index_type::String_literal:
            os << "String_literal";
            break;
        case AST_index_type::Identifier:
            os << "Identifier";
            break;
        case AST_index_type::Binary_expression:
            os << "Expression";
            break;
        case AST_index_type::Unary_expression:
            os << "Unary_expression";
            break;
        case AST_index_type::Assignment_expression:
            os << "Assignment_expression";
            break;
        case AST_index_type::Block_statement:
            os << "Block_statement";
            break;
        case AST_index_type::If_statement:
            os << "if_statement";
            break;
        case AST_index_type::For_statement:
            os << "for_statement";
            break;
        case AST_index_type::Function_declaration:
            os << "function_declaration";
            break;
        case AST_index_type::Return_statement:
            os << "return_statement";
            break;
        case AST_index_type::Variable_declaration:
            os << "variable_declaration";
            break;
        case AST_index_type::Datatype:
            os << "Datatype";
            break;
        case AST_index_type::Call_expression:
            os << "Call_expression";
            break;
        case AST_index_type::Struct_declaration:
            os << "Struct_declaration";
            break;
        case AST_index_type::Member_access:
            os << "Member_access";
            break;
        case AST_index_type::Import_declaration:
            os << "Import_declaration";
            break;
        default:
            os << "Unknown";
            break;
    }
    return os;
}

/// @brief constructs an invalid AST index (type=Invalid, index=0)
AST_index::AST_index() : type(AST_index_type::Invalid), index(0) {}

/// @brief constructs an AST index pointing to a specific node
/// @param type  which vector in the AST this index refers to
/// @param index  position within that vector
AST_index::AST_index(AST_index_type type, u32 index) : type(type), index(index) {}

/// @brief constructs a binary expression node (e.g. a + b)
Binary_expression::Binary_expression(AST_index lhs, Token_type opp, AST_index rhs)
    : lhs(lhs), opp(opp), rhs(rhs) {}

/// @brief constructs a unary expression node (e.g. -x, !x)
Unary_expression::Unary_expression(Token_type opp, AST_index operand)
    : opp(opp), operand(operand) {}

/// @brief constructs an assignment expression node (e.g. x = 5, x += 1)
Assignment_expression::Assignment_expression(AST_index target, Token_type opp, AST_index value)
    : target(target), opp(opp), value(value) {}

/// @brief constructs a block statement node containing a list of statements
Block_statement::Block_statement(std::vector<AST_index> statements)
    : statements(statements) {}

/// @brief constructs an if statement node with condition, true branch, and optional else branch
If_statement::If_statement(AST_index condition, AST_index true_condition, AST_index false_condition)
    : condition(condition), true_condition(true_condition), false_condition(false_condition) {}

/// @brief constructs a for loop node with init, condition, post-step, and body
For_statement::For_statement(AST_index init, AST_index condition, AST_index post, AST_index block)
    : init(init), condition(condition), post(post), block(block) {}

/// @brief constructs a datatype node (e.g. i32, f64, bool)
/// @param kind  the category of the type (signed int, float, bool, etc.)
/// @param bit_width  number of bits (e.g. 32 for i32)
Datatype::Datatype(Datatype_kind kind, u32 bit_width)
    : kind(kind), bit_width(bit_width) {}

/// @brief constructs a struct datatype node
Datatype::Datatype(Datatype_kind kind, u32 bit_width, const std::string& struct_name)
    : kind(kind), bit_width(bit_width), struct_name(struct_name) {}

/// @brief constructs a return statement node, value is Invalid for bare returns
Return_statement::Return_statement(AST_index value)
    : value(value) {}

/// @brief constructs a variable declaration node
/// @param name  identifier for the variable
/// @param datatype  explicit type (Invalid if inferred)
/// @param value  initialiser expression
/// @param is_const  true if declared with 'const'
Variable_declaration::Variable_declaration(AST_index name, AST_index datatype, AST_index value, bool is_const, bool is_public)
    : name(name), datatype(datatype), value(value), is_const(is_const), is_public(is_public) {}

/// @brief constructs a function call expression node
/// @param callee  identifier of the function being called
/// @param arguments  list of argument expressions
Call_expression::Call_expression(AST_index callee, std::vector<AST_index> arguments)
    : callee(callee), arguments(arguments) {}

/// @brief constructs a struct declaration node
Struct_declaration::Struct_declaration(AST_index name, std::vector<AST_index> field_types, std::vector<AST_index> field_names, bool is_public)
    : name(name), field_types(field_types), field_names(field_names), is_public(is_public) {}

/// @brief constructs a member access expression node (e.g. p.x)
Member_access_expression::Member_access_expression(AST_index object, AST_index member)
    : object(object), member(member) {}

/// @brief constructs an import declaration node
Import_declaration::Import_declaration(const std::string& file_path, Token token)
    : file_path(file_path), token(token) {}

/// @brief constructs an empty AST
AST::AST() {}

#ifndef NDEBUG

/// @brief debug prints a binary expression as (op lhs rhs)
void Binary_expression::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    const char* op_str = "?";
    switch(opp) {
        case Token_type::Plus: op_str = "+"; break;
        case Token_type::Minus: op_str = "-"; break;
        case Token_type::Multiply: op_str = "*"; break;
        case Token_type::Divide: op_str = "/"; break;
        case Token_type::Modulus: op_str = "%"; break;
        case Token_type::Equal: op_str = "=="; break;
        case Token_type::Not_equal: op_str = "!="; break;
        case Token_type::Less_than: op_str = "<"; break;
        case Token_type::Greater_than: op_str = ">"; break;
        case Token_type::Less_equal: op_str = "<="; break;
        case Token_type::Greater_equal: op_str = ">="; break;
        case Token_type::Logical_and: op_str = "&&"; break;
        case Token_type::Logical_or: op_str = "||"; break;
        case Token_type::Bitwise_and: op_str = "&"; break;
        case Token_type::Bitwise_or: op_str = "|"; break;
        case Token_type::Bitwise_xor: op_str = "^"; break;
        case Token_type::Left_shift: op_str = "<<"; break;
        case Token_type::Right_shift: op_str = ">>"; break;
        default: op_str = "?"; break;
    }
    std::cout << pad << "(" << op_str << "\n";
    lhs.print(ast, indent + 2);
    std::cout << "\n";
    rhs.print(ast, indent + 2);
    std::cout << ")";
}

/// @brief debug prints a unary expression as (op operand)
void Unary_expression::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    const char* op_str = "?";
    switch(opp) {
        case Token_type::Minus: op_str = "-"; break;
        case Token_type::Logical_not: op_str = "!"; break;
        case Token_type::Bitwise_not: op_str = "~"; break;
        default: op_str = "?"; break;
    }
    std::cout << pad << "(" << op_str << "\n";
    operand.print(ast, indent + 2);
    std::cout << ")";
}

/// @brief debug prints an assignment expression as (op= target value)
void Assignment_expression::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    const char* op_str = "=";
    switch(opp) {
        case Token_type::Assign: op_str = "="; break;
        case Token_type::Plus_assign: op_str = "+="; break;
        case Token_type::Minus_assign: op_str = "-="; break;
        case Token_type::Multiply_assign: op_str = "*="; break;
        case Token_type::Divide_assign: op_str = "/="; break;
        case Token_type::Modulus_assign: op_str = "%="; break;
        case Token_type::Bitwise_and_assign: op_str = "&="; break;
        case Token_type::Bitwise_or_assign: op_str = "|="; break;
        case Token_type::Bitwise_xor_assign: op_str = "^="; break;
        case Token_type::Left_shift_assign: op_str = "<<="; break;
        case Token_type::Right_shift_assign: op_str = ">>="; break;
        default: op_str = "?="; break;
    }
    std::cout << pad << "(" << op_str << "\n";
    target.print(ast, indent + 2);
    std::cout << "\n";
    value.print(ast, indent + 2);
    std::cout << ")";
}

/// @brief debug prints a block statement as { stmt; stmt; ... }
void Block_statement::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "{\n";
    for (const auto& statement : statements){
        statement.print(ast, indent + 2);
        std::cout << "\n";
    }
    std::cout << pad << "}";
}

/// @brief debug prints an if statement with condition, then, and optional else
void If_statement::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "if\n";
    std::cout << pad << "  condition:\n";
    condition.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  then:\n";
    true_condition.print(ast, indent + 4);
    if (false_condition.type != AST_index_type::Invalid){
        std::cout << "\n" << pad << "  else:\n";
        false_condition.print(ast, indent + 4);
    }
}

/// @brief debug prints a for loop with init, condition, post, and body
void For_statement::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "for\n";
    std::cout << pad << "  init:\n";
    init.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  condition:\n";
    condition.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  post:\n";
    post.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  block:\n";
    block.print(ast, indent + 4);
}

/// @brief debug prints a function declaration with name, params, body, and return type
void Function_declaration::print(const AST& ast, u32 indent) const{
    std::string pad(indent, ' ');
    std::cout << pad << "Function Declaration:\n";
    std::cout << pad << "  Identifier: ";
    identifier.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  Datatypes:";
    if (!datatypes.empty()) {
        std::cout << "\n";
        for (const auto& dt : datatypes) {
            dt.print(ast, indent + 6);
            std::cout << "\n";
        }
    } else {
        std::cout << " <none>\n";
    }
    std::cout << pad << "  Names:";
    if (!names.empty()) {
        std::cout << "\n";
        for (const auto& nm : names) {
            nm.print(ast, indent + 6);
            std::cout << "\n";
        }
    } else {
        std::cout << " <none>\n";
    }
    std::cout << pad << "  Block:\n";
    block.print(ast, indent + 4);
    std::cout << "\n";
    std::cout << pad << "  Return type: ";
    if (return_type.type == AST_index_type::Invalid){
        std::cout<<"void";
    }else{
        return_type.print(ast, indent + 4);
    }
    std::cout << "\n";
}

/// @brief debug prints a datatype (e.g. i32, f64, bool, void)
void Datatype::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    switch (kind) {
        case Datatype_kind::Unsigned_int:
            std::cout << pad << "u" << bit_width;
            break;
        case Datatype_kind::Signed_int:
            std::cout << pad << "i" << bit_width;
            break;
        case Datatype_kind::Float:
            std::cout << pad << "f" << bit_width;
            break;
        case Datatype_kind::Bool:
            std::cout << pad << "bool";
            break;
        case Datatype_kind::Void:
            std::cout << pad << "void";
            break;
        case Datatype_kind::String:
            std::cout << pad << "string";
            break;
        case Datatype_kind::Struct:
            std::cout << pad << struct_name;
            break;
    }
}

/// @brief debug prints a return statement with its value
void Return_statement::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "return";
    if (value.type != AST_index_type::Invalid) {
        std::cout << "\n";
        value.print(ast, indent + 2);
    }
}

/// @brief debug prints a variable declaration with name, type, and value
void Variable_declaration::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << (is_const ? "const " : "var ") << "\n";
    std::cout << pad << "  name: ";
    name.print(ast, 0);
    std::cout << "\n";
    std::cout << pad << "  type: ";
    if (datatype.type != AST_index_type::Invalid) {
        datatype.print(ast, 0);
    } else {
        std::cout << "<inferred>";
    }
    std::cout << "\n";
    std::cout << pad << "  value:\n";
    value.print(ast, indent + 4);
}


/// @brief debug prints a function call with callee and arguments
void Call_expression::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "call ";
    callee.print(ast, 0);
    std::cout << "(";
    for (u64 i = 0; i < arguments.size(); ++i) {
        if (i > 0) std::cout << ", ";
        arguments[i].print(ast, 0);
    }
    std::cout << ")";
}

/// @brief debug prints a struct declaration
void Struct_declaration::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "struct ";
    name.print(ast, 0);
    std::cout << " {\n";
    for (u64 i = 0; i < field_types.size(); ++i) {
        field_types[i].print(ast, indent + 2);
        std::cout << " ";
        field_names[i].print(ast, 0);
        std::cout << "\n";
    }
    std::cout << pad << "}";
}

/// @brief debug prints a member access expression (e.g. p.x)
void Member_access_expression::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    object.print(ast, indent);
    std::cout << ".";
    member.print(ast, 0);
}

/// @brief debug prints an import declaration
void Import_declaration::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    std::cout << pad << "@import(\"" << file_path << "\")";
}

/// @brief debug prints the entire AST starting from the root block
void AST::print() const {
    std::cout << "AST:\n";
    if (!block_statements.empty()){
        std::cout << "Program:\n";
        block_statements[0].print(*this, 0);
        std::cout << "\n";
    }
}




/// @brief debug prints any AST node by looking up its type and dispatching to the right print
void AST_index::print(const AST& ast, u32 indent) const {
    std::string pad(indent, ' ');
    switch (type) {
        case AST_index_type::Integer:
            std::cout << pad << ast.integers[index].value;
            break;
        case AST_index_type::Float_literal:
            std::cout << pad << ast.floats[index].value;
            break;
        case AST_index_type::String_literal:
            std::cout << pad << '"' << ast.string_literals[index].value << '"';
            break;
        case AST_index_type::Identifier:
            std::cout << pad << ast.identifiers[index].name;
            break;
        case AST_index_type::Binary_expression:
            ast.binary_expressions[index].print(ast, indent);
            break;
        case AST_index_type::Unary_expression:
            ast.unary_expressions[index].print(ast, indent);
            break;
        case AST_index_type::Assignment_expression:
            ast.assignment_expressions[index].print(ast, indent);
            break;
        case AST_index_type::Block_statement:
            ast.block_statements[index].print(ast, indent);
            break;
        case AST_index_type::If_statement:
            ast.if_statements[index].print(ast, indent);
            break;
        case AST_index_type::For_statement:
            ast.for_statements[index].print(ast, indent);
            break;
        case AST_index_type::Function_declaration:
            ast.function_declarations[index].print(ast,indent);
            break;
        case AST_index_type::Return_statement:
            ast.return_statements[index].print(ast, indent);
            break;
        case AST_index_type::Variable_declaration:
            ast.variable_declarations[index].print(ast, indent);
            break;
        case AST_index_type::Datatype:
            ast.datatypes[index].print(ast, indent);
            break;
        case AST_index_type::Call_expression:
            ast.call_expressions[index].print(ast, indent);
            break;
        case AST_index_type::Struct_declaration:
            ast.struct_declarations[index].print(ast, indent);
            break;
        case AST_index_type::Member_access:
            ast.member_access_expressions[index].print(ast, indent);
            break;
        case AST_index_type::Import_declaration:
            ast.import_declarations[index].print(ast, indent);
            break;
        case AST_index_type::Invalid:
            std::cout << pad << "<invalid>";
            break;
        default:
            std::cout << pad << "<unknown>";
            break;
    }
}

#endif
