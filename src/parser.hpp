/*
Author: Twan Roodenburg
Date: 19/02/2026
File: parser.hpp
Description: 
    The parser for the program, 
    used for converting a vector of tokens into an Abstract Syntax Tree (AST).
    The parser uses a recursive descent approach to parse the tokens.
*/


#pragma once
#include "print_colors.h"
#include "modern_types.h"
#include "lexer.hpp"
#include "ast.hpp"
#include <set>


/// @brief the parser class, used for converting a vector of tokens into an AST
class Parser
{
private:
    std::string& m_input;        //< the original source code, used for error messages
    Token m_current;             //< the current token being parsed, used for parsing and error messages
    AST m_ast;                   //< the AST being constructed by the parser, initially empty and filled in as the parser processes tokens
    AST_index m_last_index;      //< the AST index of the last parsed expression or statement, used for error messages and for building up larger expressions from smaller ones
    std::vector<Token> m_tokens; //< the vector of tokens produced by the lexer, used for parsing
    u64 m_token_index;           //< the current index into the m_tokens vector, used for keeping track of which token is being parsed
    bool m_has_error = false;    //< flags whether the parser has encountered an error, used for error handling in the parser
    u32 m_synth_counter = 0;     //< counter for generating unique synthetic variable names
    std::set<std::string> m_struct_names; //< set of declared struct names, used to recognise struct types in is_data_type

public:
    Parser(std::vector<Token> tokens, std::string& input);
    AST get_AST();
    void parse();
    bool has_error() const;

private:
    void advance(i8 n = 1);
    Token peek(i8 n = 1);
    void revert(i8 n = 1);

    bool is_opperator(Token_type type);
    bool is_binary_op(Token_type type);
    bool is_assignment_op(Token_type type);
    bool is_unary_prefix(Token_type type);
    bool is_data_type(Token token);
    i32 get_precedence(Token_type type);

    void syntax_error(std::string msg, Token token);

    AST_index parse_primary();
    AST_index parse_identifier();
    AST_index parse_datatype();
    AST_index parse_binary_expression(AST_index lhs,i32 min_prec);
    AST_index parse_integer();
    AST_index parse_float();
    AST_index parse_string();
    AST_index parse_call_expression();
    AST_index parse_block_statement();
    AST_index parse_if_statement();
    AST_index parse_for_statement();
    AST_index parse_expression();
    AST_index parse_statement();
    AST_index parse_function_declaration();
    AST_index parse_return_statement();
    AST_index parse_variable_declaration(bool is_const);
    AST_index parse_struct_declaration();

    AST_index synth_identifier(const std::string& name);
    AST_index synth_integer(i64 value);
    AST_index synth_var_decl(AST_index name, AST_index value);
    AST_index synth_binary(AST_index lhs, Token_type op, AST_index rhs);
    AST_index synth_assignment(AST_index target, Token_type op, AST_index value);
};