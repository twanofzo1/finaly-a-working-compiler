/*
Author: Twan Roodenburg
Date: 22/02/2026
File: lexer.hpp
Description: 
    The lexer for the program, 
    used for converting a string of characters into a vector of tokens that can be parsed by the parser.
    The lexer uses a state machine to keep track of what it is currently lexing, 
    and produces tokens based on the characters it encounters in the input string.
    The lexer also handles errors by setting a flag and printing an error message, 
    which can be checked by the caller to determine if the lexing process was successful or if it encountered any errors.
*/


#pragma once
#include "token.hpp"
#include <vector> 
#include "log.hpp"
#include "modern_types.h"
#include <iostream>
#include <string>
#include <unordered_map>

/// @brief the state the lexer is in
enum class Lexer_state{
    Start,      //< the lexer is not currently lexing anything
    Number,     //< the lexer is currently lexing a number
    Identifier, //< the lexer is currently lexing an identifier
    Other,      //< the lexer is currently lexing a character that is not a number or an identifier
};


/// @brief a lexer that takes in a string and produces a vector of tokens
class Lexer
{
private:
    u64 m_pos = 0;                              //< the current position in the input string
    std::string& m_input;                       //< the input string that the lexer is lexing, it is a reference to avoid unnecessary copying
    char m_current;                             //< the current character that the lexer is looking at
    std::vector<Token> m_tokens;                //< the vector of tokens that the lexer has produced
    Lexer_state m_state = Lexer_state::Start;   //< the current state of the lexer
    bool m_has_error = false;                   //< flags whether the lexer has encountered an error, used for error handling in the lexer


public: 
    Lexer(std::string& input);
    void lex();
    const std::vector<Token>& get_tokens() const;
    bool has_error() const;
private:
    char peek(u8 n = 1);
    void advance(u8 n = 1);
    void revert(u8 n = 1);
    void push_token(Token_type type, u32 start, u32 end);
    void lexer_error(const std::string& message);
}; 

