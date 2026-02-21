/*
Author: Twan Roodenburg
Date: 17/02/2026
File: token.cpp
Description: 
    The token struct for the program, 
    used for storing the type of a lexed token and a view of the original source code for error reporting.
    The token struct also has helper functions for getting the position and line number of the token in the source code, 
    and for printing the line of source code containing the token with a marker, for error reporting.
*/

#include "token.hpp"

/// @brief constructs an invalid token, used as a default/error value
Token::Token() : type(Token_type::Invalid), view() {}

/// @brief constructs a token with the given type and view into the source
/// @param type  the type of the token
/// @param view  a view of the original source code
Token::Token(Token_type type, std::string_view view) : type(type), view(view) {}

/// @brief gets the byte position of this token in the source string
/// @param source  the original source code
/// @return  the byte offset from the start of source
u64 Token::get_pos(const std::string& source) {
    if (view.empty()) return -1;
    u64 pos = view.data() - source.data();
    return static_cast<u64>(pos);
}

/// @brief gets the line number of this token by counting newlines before it
/// @param source  the original source code
/// @return  the 1-based line number
u32 Token::get_line(const std::string& source) {
    if (view.empty()) return 0;
    u64 pos = view.data() - source.data();
    u32 line = 1;
    for (u64 i = 0; i < pos; ++i) {
        if (source[i] == '\n') ++line;
    }
    return line;
}

/// @brief prints the source line containing this token with a caret marker underneath
/// @param source  the original source code
void Token::print_line(const std::string& source) {
    if (view.empty()) {
        std::cout << "Token not found in source." << std::endl;
        return;
    }
    u64 pos = view.data() - source.data();
    u64 line_start = source.rfind('\n', pos);
    if (line_start == std::string::npos) line_start = 0; else ++line_start;
    u64 line_end = source.find('\n', pos);
    if (line_end == std::string::npos) line_end = source.size();
    std::string line = source.substr(line_start, line_end - line_start);
    std::cout << line << std::endl;
    u64 caret_pos = pos - line_start;
    for (u64 i = 0; i < caret_pos; ++i) std::cout << ' ';
    std::cout << '^';
    for (u64 i = 1; i < view.size(); ++i) std::cout << '~';
    std::cout << std::endl;
}

/// @brief prints the token type as a human-readable string (for debug output)
std::ostream& operator<<(std::ostream& os, const Token_type& type) {
    switch (type) {
        case Token_type::Invalid:           os << "Invalid";           break;
        case Token_type::Integer:           os << "Integer";           break;
        case Token_type::Float_literal:     os << "Float";             break;
        case Token_type::Plus:              os << "Plus";              break;
        case Token_type::Minus:             os << "Minus";             break;
        case Token_type::Multiply:          os << "Multiply";          break;
        case Token_type::Divide:            os << "Divide";            break;
        case Token_type::Modulus:           os << "Modulus";           break;
        case Token_type::Assign:            os << "Assign";            break;
        case Token_type::Plus_assign:       os << "Plus_assign";       break;
        case Token_type::Minus_assign:      os << "Minus_assign";      break;
        case Token_type::Multiply_assign:   os << "Multiply_assign";   break;
        case Token_type::Divide_assign:     os << "Divide_assign";     break;
        case Token_type::Modulus_assign:    os << "Modulus_assign";    break;
        case Token_type::Bitwise_and_assign:os << "Bitwise_and_assign";break;
        case Token_type::Bitwise_or_assign: os << "Bitwise_or_assign"; break;
        case Token_type::Bitwise_xor_assign:os << "Bitwise_xor_assign";break;
        case Token_type::Left_shift_assign: os << "Left_shift_assign"; break;
        case Token_type::Right_shift_assign:os << "Right_shift_assign";break;
        case Token_type::Equal:             os << "Equal";             break;
        case Token_type::Not_equal:         os << "Not_equal";         break;
        case Token_type::Less_than:         os << "Less_than";         break;
        case Token_type::Greater_than:      os << "Greater_than";      break;
        case Token_type::Less_equal:        os << "Less_equal";        break;
        case Token_type::Greater_equal:     os << "Greater_equal";     break;
        case Token_type::Logical_and:       os << "Logical_and";       break;
        case Token_type::Logical_or:        os << "Logical_or";        break;
        case Token_type::Logical_not:       os << "Logical_not";       break;
        case Token_type::Bitwise_and:       os << "Bitwise_and";       break;
        case Token_type::Bitwise_or:        os << "Bitwise_or";        break;
        case Token_type::Bitwise_xor:       os << "Bitwise_xor";       break;
        case Token_type::Bitwise_not:       os << "Bitwise_not";       break;
        case Token_type::Left_shift:        os << "Left_shift";        break;
        case Token_type::Right_shift:       os << "Right_shift";       break;
        case Token_type::Arrow:             os << "Arrow";             break;
        case Token_type::Dot:               os << "Dot";               break;
        case Token_type::Dot_dot:           os << "Dot_dot";           break;
        case Token_type::Comma:             os << "Comma";             break;
        case Token_type::Colon:             os << "Colon";             break;
        case Token_type::Question_mark:     os << "Question_mark";     break;
        case Token_type::Semicolon:         os << "Semicolon";         break;
        case Token_type::Left_brace:        os << "Left_brace";        break;
        case Token_type::Right_brace:       os << "Right_brace";       break;
        case Token_type::Left_parenthesis:  os << "Left_parenthesis";  break;
        case Token_type::Right_parenthesis: os << "Right_parenthesis"; break;
        case Token_type::Left_bracket:      os << "Left_bracket";      break;
        case Token_type::Right_bracket:     os << "Right_bracket";     break;
        case Token_type::End_of_file:       os << "EndOfFile";         break;
        case Token_type::Identifier:        os << "Identifier";        break;
        case Token_type::If:                os << "If";                break;
        case Token_type::Else:              os << "Else";              break;
        case Token_type::For:               os << "For";               break;
        case Token_type::In:                os << "In";                break;
        case Token_type::Return:            os << "Return";            break;
        case Token_type::True:              os << "True";              break;
        case Token_type::False:             os << "False";             break;
        case Token_type::Function:          os << "Function";          break;
        case Token_type::Var:               os << "Var";               break;
        case Token_type::Const:             os << "Const";             break;
        case Token_type::String_literal:    os << "String_literal";    break;
        case Token_type::Struct:            os << "Struct";            break;
        case Token_type::Pub:               os << "Pub";               break;
        case Token_type::Import:            os << "Import";            break;
        default: os << "Unknown"; break;
    }
    return os;
}
