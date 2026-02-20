/*
Author: Twan Roodenburg
Date: 19/02/2026
File: ast.hpp
Description: 
    The abstract syntax tree (AST) for the program, 
    used for representing the structure of the program in a way that is easy to traverse 
    and manipulate during semantic analysis and code generation.
    The AST contains vectors for each type of node in the AST, and the nodes reference each other using AST_index structs
    none of the structs have any functionality beyond storing their data and printing themselves for debugging purposes
*/


#pragma once
#include "modern_types.h"
#include "token.hpp"
#include <vector>
#include <string>
#include <iostream>

struct AST; // predeclare AST for print functions

/// @brief the type of a datatype, used for type annotations in the AST
enum class Datatype_kind
{
    Unsigned_int,  //< u8, u16, u32, u64, or arbitrary like u3, u24
    Signed_int,    //< i8, i16, i32, i64, or arbitrary like i7
    Float,         //< f16, f32, f64
    Bool,          //< bool
    Void,          //< void
    String,        //< string (pointer to null-terminated data)
    Struct,        //< a user-defined struct type
};

/// @brief the type of an AST index, used for indexing into the various vectors in the AST
enum class AST_index_type
{
    Invalid,                   //< an invalid index, used for error handling
    Integer,                   //< an integer literal
    Float_literal,             //< a float literal
    String_literal,            //< a string literal
    Identifier,                //< an identifier
    Datatype,                  //< a datatype
    Binary_expression,         //< a binary expression (e.g. 1 + 2)
    Unary_expression,          //< a unary expression (e.g. -x)
    Assignment_expression,     //< an assignment expression (e.g. x = 5)
    Block_statement,           //< a block statement (e.g. { ... })
    If_statement,              //< an if statement (e.g. if (x > 0) { ... } else { ... })
    For_statement,             //< a for statement (e.g. for (var i = 0; i < 10; i++) { ... })
    Function_declaration,      //< a function declaration (e.g. fn add(x: i32, y: i32) -> i32 { ... })
    Return_statement,          //< a return statement (e.g. return x + y)
    Variable_declaration,      //< a variable declaration (e.g. var x: i32 = 5)
    Call_expression,           //< a call expression (e.g. add(1, 2))
    Struct_declaration,        //< a struct declaration (e.g. struct Point { i32 x; i32 y })
    Member_access,             //< a member access expression (e.g. p.x)
};



/// @brief a tagged index for the AST, used for referencing nodes in the AST
struct AST_index
{
    AST_index_type type;        //< the type of the index, used for determining which vector in the AST to index into
    u32 index;                  //< the index into the corresponding vector in the AST

    AST_index();
    AST_index(AST_index_type type, u32 index);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a binary expression node in the AST, used for representing binary operations in the AST
struct Binary_expression
{
    AST_index lhs;
    Token_type opp;
    AST_index rhs;

    Binary_expression(AST_index lhs, Token_type opp, AST_index rhs);
    
    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a unary expression node in the AST, used for representing unary operations in the AST
struct Unary_expression
{
    Token_type opp;
    AST_index operand;

    Unary_expression(Token_type opp, AST_index operand);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief an assignment expression node in the AST, used for representing assignment operations in the AST
struct Assignment_expression
{
    AST_index target;
    Token_type opp;
    AST_index value;

    Assignment_expression(AST_index target, Token_type opp, AST_index value);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a block statement node in the AST, used for representing blocks of statements in the AST
struct Block_statement
{
    std::vector<AST_index> statements;

    Block_statement(std::vector<AST_index> statements);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief an if statement node in the AST, used for representing if statements in the AST
struct If_statement
{
    AST_index condition;
    AST_index true_condition;
    AST_index false_condition;

    If_statement(AST_index condition, AST_index true_condition, AST_index false_condition);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a for statement node in the AST, used for representing for statements in the AST
struct For_statement
{
    AST_index init;
    AST_index condition;
    AST_index post;
    AST_index block;

    For_statement(AST_index init, AST_index condition, AST_index post, AST_index block);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a function declaration node in the AST, used for representing function declarations in the AST
struct Function_declaration
{
    AST_index identifier;
    std::vector<AST_index> datatypes;
    std::vector<AST_index> names;
    AST_index block;
    AST_index return_type;

    Function_declaration(AST_index identifier,std::vector<AST_index> datatypes,std::vector<AST_index> names,AST_index block,AST_index return_type) 
        : identifier(identifier), datatypes(datatypes), names(names), block(block), return_type(return_type){}

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a datatype node in the AST, used for representing datatypes in the AST
struct Datatype
{
    Datatype_kind kind;
    u32 bit_width; // e.g. 8, 16, 32, 64 — allows arbitrary widths like Zig
    std::string struct_name; // name of the struct type (only used when kind == Struct)

    Datatype(Datatype_kind kind, u32 bit_width);
    Datatype(Datatype_kind kind, u32 bit_width, const std::string& struct_name);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a return statement node in the AST, used for representing return statements in the AST
struct Return_statement
{
    AST_index value; // the expression being returned (Invalid if bare return)

    Return_statement(AST_index value);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a variable declaration node in the AST, used for representing variable declarations in the AST
struct Variable_declaration
{
    AST_index name;       //< identifier
    AST_index datatype;   //< explicit type (Invalid if inferred via :=)
    AST_index value;      //< initialiser expression
    bool is_const;        //< const vs var

    Variable_declaration(AST_index name, AST_index datatype, AST_index value, bool is_const);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a function call expression node in the AST, used for representing function calls in the AST
struct Call_expression
{
    AST_index callee;                   // identifier being called
    std::vector<AST_index> arguments;   // argument expressions

    Call_expression(AST_index callee, std::vector<AST_index> arguments);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a struct declaration node in the AST
struct Struct_declaration
{
    AST_index name;                        //< struct name (identifier)
    std::vector<AST_index> field_types;    //< datatype of each field
    std::vector<AST_index> field_names;    //< identifier for each field

    Struct_declaration(AST_index name, std::vector<AST_index> field_types, std::vector<AST_index> field_names);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief a member access expression (e.g. p.x)
struct Member_access_expression
{
    AST_index object;   //< the expression being accessed (e.g. p)
    AST_index member;   //< the member name identifier (e.g. x)

    Member_access_expression(AST_index object, AST_index member);

    #ifndef NDEBUG
    void print(const AST& ast, u32 indent = 0) const;
    #endif
};

/// @brief the abstract syntax tree (AST) for the program
/// the AST contains vectors for each type of node in the AST, and the nodes reference each other using AST_index structs
/// this design allows for a simple and efficient representation of the AST, as well as easy traversal and manipulation of the AST with minimal memory overhead
struct AST
{
    std::vector<i64> integers;
    std::vector<Token> integer_tokens;          // parallel to integers – source location
    std::vector<double> floats;
    std::vector<Token> float_tokens;            // parallel to floats – source location
    std::vector<std::string> identifiers;
    std::vector<Token> identifier_tokens;       // parallel to identifiers – source location
    std::vector<std::string> string_literals;
    std::vector<Token> string_tokens;           // parallel to string_literals – source location
    std::vector<Binary_expression> binary_expressions;
    std::vector<Unary_expression> unary_expressions;
    std::vector<Assignment_expression> assignment_expressions;
    std::vector<Block_statement> block_statements;
    std::vector<If_statement> if_statements;
    std::vector<For_statement> for_statements;
    std::vector<Function_declaration> function_declarations;
    std::vector<Datatype> datatypes;
    std::vector<Return_statement> return_statements;
    std::vector<Variable_declaration> variable_declarations;
    std::vector<Call_expression> call_expressions;
    std::vector<Struct_declaration> struct_declarations;
    std::vector<Member_access_expression> member_access_expressions;

    AST();

    #ifndef NDEBUG
    void print() const;
    #endif
};

