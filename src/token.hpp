/*
Author: Twan Roodenburg
Date: 17/02/2026
File: token.hpp
Description: 
    The token struct for the program, 
    used for storing the type of a lexed token and a view of the original source code for error reporting.
    The token struct also has helper functions for getting the position and line number of the token in the source code, 
    and for printing the line of source code containing the token with a marker, for error reporting.
*/



#pragma once
#include <iostream>
#include <string>
#include <string_view>
#include "modern_types.h"

/// @brief the type of a lexed token
enum class Token_type{
    Invalid,                    //< an invalid token, used for error handling
    Integer,                    //< an integer literal (e.g. 42)
    Float_literal,              //< a float literal (e.g. 3.14)
    Identifier,                 //< an identifier (e.g. my_variable)
    String_literal,             //< a string literal (e.g. "hello world")

    If,                         //< the 'if' keyword
    Else,                       //< the 'else' keyword
    For,                        //< the 'for' keyword
    In,                         //< the 'in' keyword
    Return,                     //< the 'return' keyword
    True,                       //< the 'true' keyword
    False,                      //< the 'false' keyword

    // Arithmetic   

    Plus,                       //< +
    Minus,                      //< -
    Multiply,                   //< *
    Divide,                     //< /
    Modulus,                    //< %

    // Assignment

    Assign,                     //< =
    Plus_assign,                //< +=
    Minus_assign,               //< -=
    Multiply_assign,            //< *=
    Divide_assign,              //< /=
    Modulus_assign,             //< %=
    Bitwise_and_assign,         //< &=
    Bitwise_or_assign,          //< |=
    Bitwise_xor_assign,         //< ^=
    Left_shift_assign,          //< <<=
    Right_shift_assign,         //< >>=

    // Comparison

    Equal,                      //< ==
    Not_equal,                  //< !=
    Less_than,                  //< <
    Greater_than,               //< >
    Less_equal,                 //< <=
    Greater_equal,              //< >=

    // Logical
    Logical_and,                //< &&
    Logical_or,                 //< ||
    Logical_not,                //< !

    // Bitwise

    Bitwise_and,                //< &
    Bitwise_or,                 //< |
    Bitwise_xor,                //< ^
    Bitwise_not,                //< ~
    Left_shift,                 //< <<
    Right_shift,                //< >>

    // Misc

    Arrow,                      //< ->
    Dot,                        //< .
    Dot_dot,                    //< ..
    Comma,                      //< ,
    Colon,                      //< :
    Question_mark,              //< ?

    Function,                   //< the 'fn' keyword
    Var,                        //< the 'var' keyword
    Const,                      //< the 'const' keyword

    Semicolon,                  //< ;
    Left_brace,                 //< {
    Right_brace,                //<  }
    Left_parenthesis,           //< (
    Right_parenthesis,          //< )
    Left_bracket,               //< [
    Right_bracket,              //< ]
    End_of_file,                //< the end of the input file                  
};

/// @brief a lexed token for storing the type of the token and a view of the original source code for error reporting
struct Token
{
    Token_type type;    //< the type of the token
    std::string_view view;    //< a view of the original source code for error reporting

    Token();
    Token(Token_type type, std::string_view view);
    u64 get_pos(const std::string& source);
    u32 get_line(const std::string& source);
    void print_line(const std::string& source);
};

std::ostream& operator<<(std::ostream& os, const Token_type& type);