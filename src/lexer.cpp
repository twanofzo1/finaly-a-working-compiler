/*
Author: Twan Roodenburg
Date: 22/02/2026
File: lexer.cpp
Description: 
    The lexer for the program, 
    used for converting a string of characters into a vector of tokens that can be parsed by the parser.
    The lexer uses a state machine to keep track of what it is currently lexing, 
    and produces tokens based on the characters it encounters in the input string.
    The lexer also handles errors by setting a flag and printing an error message, 
    which can be checked by the caller to determine if the lexing process was successful or if it encountered any errors.
*/

#include "lexer.hpp"
#include "print_colors.hpp"


/// @brief a map of keywords to their corresponding token types
std::unordered_map<std::string, Token_type> keywords{
    {"fn",     Token_type::Function},
    {"if",     Token_type::If},
    {"else",   Token_type::Else},
    {"for",    Token_type::For},
    {"in",     Token_type::In},
    {"return", Token_type::Return},
    {"true",   Token_type::True},
    {"false",  Token_type::False},
    {"var",    Token_type::Var},
    {"const",  Token_type::Const},
    {"struct", Token_type::Struct},
    {"pub",    Token_type::Pub},
};

/// @brief constructs a new lexer with the given input string
/// @param input a refference to the input string to lex
Lexer::Lexer(std::string& input) : m_input(input) {}


/// @brief lexes the input string and produces a vector of tokens
void Lexer::lex(){
    m_current = m_input[m_pos]; // get current character at position 0
    while (true)
    {
        if (m_current == ' ') {advance(); continue;}
        else if (m_current == '\n') {advance(); continue;}
        else if (m_current == '\0') {push_token(Token_type::End_of_file, m_pos, m_pos); return;}
        
        
        // Handle @import builtin
        else if (m_current == '@') {
            u32 startpos = m_pos;
            // Check if it's @import
            if (m_input.compare(m_pos, 7, "@import") == 0) {
                push_token(Token_type::Import, startpos, startpos + 7);
                advance(6); // skip 'import' (6 chars, push_token already sets state)
                advance(); // move past last char
                continue;
            } else {
                lexer_error("unexpected '@' — did you mean @import?");
                advance();
                continue;
            }
        }
        switch (m_state)
        {
            // Start state: determine the type of the next token based on the first character
            // If the first character is a digit, we are lexing a number
            // If the first character is a letter or an underscore, we are lexing an identifier
            // Otherwise, we are lexing an operator or a punctuation
            case Lexer_state::Start: {
                LOG("entering lexer state: Start");

                if (isdigit(m_current)){
                    m_state = Lexer_state::Number;
                }else if (isalpha(m_current) || m_current == '_'){
                    m_state = Lexer_state::Identifier;
                }else {
                    m_state = Lexer_state::Other; // should probably remove iif performance lacks, but it is more readable this way
                }
                continue; // reprocess next state
            }
            
            // Number state: lex a number literal, which can be an integer or a float
            // proceed until we reach a non-digit character, then check if the next character is a dot followed by a digit to determine if it is a float literal
            // If it is a float literal, proceed until we reach a non-digit character again, then push the float literal token
            // If it is not a float literal, push the integer literal token
            case Lexer_state::Number: {
                LOG("entering lexer state: Number");

                u32 startpos = m_pos;
                while (isdigit(m_current))
                {
                    advance();
                }
                // Check for float literal: digits.digits
                if (m_current == '.' && isdigit(peek())) {
                    advance(); // skip '.'
                    while (isdigit(m_current)) {
                        advance();
                    }
                    u32 endpos = m_pos;
                    push_token(Token_type::Float_literal, startpos, endpos);
                    continue;
                }
                u32 endpos = m_pos;
                push_token(Token_type::Integer, startpos, endpos);
                continue;
            }

            // Identifier state: lex an identifier, which is a sequence of letters, digits, and underscores that starts with a letter or an underscore
            // proceed until we reach a character that is not a letter, digit, or underscore, then check if the lexed identifier is a keyword, 
            // if it is, push the corresponding keyword token, otherwise push an identifier token
            // if its not a keyword, push an identifier token
            case Lexer_state::Identifier: {
                LOG("entering lexer state: Identifier");

                u32 startpos = m_pos;
                while (isalnum(m_current) || m_current == '_')
                {
                    advance();
                }
                u32 endpos = m_pos;
                std::string word(m_input.data() + startpos, endpos - startpos);
                auto it = keywords.find(word);
                if (it != keywords.end()){
                    push_token(it->second, startpos, endpos);
                } else {
                    push_token(Token_type::Identifier, startpos, endpos);
                }
                continue;
            }

            // Other state: lex an operator or a punctuation, which can be one or more characters long (e.g. +, +=, ==, etc.)
            case Lexer_state::Other: {
                LOG("entering lexer state: Other");

                if (m_current == '+'){
                    if (peek() == '='){ push_token(Token_type::Plus_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Plus,m_pos,m_pos+1); }
                }
                else if (m_current == '-'){
                    if (peek() == '='){ push_token(Token_type::Minus_assign,m_pos,m_pos+2); advance(); }
                    else if (peek() == '>'){ push_token(Token_type::Arrow,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Minus,m_pos,m_pos+1); }
                }
                else if (m_current == '*'){
                    if (peek() == '='){ push_token(Token_type::Multiply_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Multiply,m_pos,m_pos+1); }
                }
                else if (m_current == '/'){
                    if (peek() == '='){ push_token(Token_type::Divide_assign,m_pos,m_pos+2); advance(); }
                    if (peek() == '/'){ // single line comment
                        while (m_current != '\n' && m_current != '\0') advance();
                        continue;
                    }
                    if (peek() == '*'){ // multi line comment
                        advance(2); // skip '/*'
                        while (!(m_current == '*' && peek() == '/')) {
                            if (m_current == '\0') {
                                lexer_error("unterminated multi-line comment");
                                return;
                            }
                            advance();
                        }
                        advance(); // skip '*/' 
                        continue;
                    }
                    else { push_token(Token_type::Divide,m_pos,m_pos+1); }
                }
                else if (m_current == '%'){
                    if (peek() == '='){ push_token(Token_type::Modulus_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Modulus,m_pos,m_pos+1); }
                }
                else if (m_current == '='){
                    if (peek() == '='){ push_token(Token_type::Equal,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Assign,m_pos,m_pos+1); }
                }
                else if (m_current == '!'){
                    if (peek() == '='){ push_token(Token_type::Not_equal,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Logical_not,m_pos,m_pos+1); }
                }
                else if (m_current == '<'){
                    if (peek() == '='){ push_token(Token_type::Less_equal,m_pos,m_pos+2); advance(); }
                    else if (peek() == '<'){
                        if (peek(2) == '='){ push_token(Token_type::Left_shift_assign,m_pos,m_pos+3); advance(2); }
                        else { push_token(Token_type::Left_shift,m_pos,m_pos+2); advance(); }
                    }
                    else { push_token(Token_type::Less_than,m_pos,m_pos+1); }
                }
                else if (m_current == '>'){
                    if (peek() == '='){ push_token(Token_type::Greater_equal,m_pos,m_pos+2); advance(); }
                    else if (peek() == '>'){
                        if (peek(2) == '='){ push_token(Token_type::Right_shift_assign,m_pos,m_pos+3); advance(2); }
                        else { push_token(Token_type::Right_shift,m_pos,m_pos+2); advance(); }
                    }
                    else { push_token(Token_type::Greater_than,m_pos,m_pos+1); }
                }
                else if (m_current == '&'){
                    if (peek() == '&'){ push_token(Token_type::Logical_and,m_pos,m_pos+2); advance(); }
                    else if (peek() == '='){ push_token(Token_type::Bitwise_and_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Bitwise_and,m_pos,m_pos+1); }
                }
                else if (m_current == '|'){
                    if (peek() == '|'){ push_token(Token_type::Logical_or,m_pos,m_pos+2); advance(); }
                    else if (peek() == '='){ push_token(Token_type::Bitwise_or_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Bitwise_or,m_pos,m_pos+1); }
                }
                else if (m_current == '^'){
                    if (peek() == '='){ push_token(Token_type::Bitwise_xor_assign,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Bitwise_xor,m_pos,m_pos+1); }
                }
                else if (m_current == '~'){ push_token(Token_type::Bitwise_not,m_pos,m_pos+1); }
                else if (m_current == '.'){
                    if (peek() == '.'){ push_token(Token_type::Dot_dot,m_pos,m_pos+2); advance(); }
                    else { push_token(Token_type::Dot,m_pos,m_pos+1); }
                }
                else if (m_current == ','){ push_token(Token_type::Comma,m_pos,m_pos+1); }
                else if (m_current == ':'){ push_token(Token_type::Colon,m_pos,m_pos+1); }
                else if (m_current == '?'){ push_token(Token_type::Question_mark,m_pos,m_pos+1); }
                else if (m_current == ';'){ push_token(Token_type::Semicolon,m_pos,m_pos+1);}
                else if (m_current == '{'){ push_token(Token_type::Left_brace,m_pos,m_pos+1);}
                else if (m_current == '}'){ push_token(Token_type::Right_brace,m_pos,m_pos+1);}
                else if (m_current == '('){ push_token(Token_type::Left_parenthesis,m_pos,m_pos+1);}
                else if (m_current == ')'){ push_token(Token_type::Right_parenthesis,m_pos,m_pos+1);}
                else if (m_current == '['){ push_token(Token_type::Left_bracket,m_pos,m_pos+1);}
                else if (m_current == ']'){ push_token(Token_type::Right_bracket,m_pos,m_pos+1);}

                // String literals: "..."
                else if (m_current == '"'){
                    u32 startpos = m_pos;
                    advance(); // skip opening quote
                    while (m_current != '"' && m_current != '\0' && m_current != '\n') {
                        if (m_current == '\\') advance(); // skip escaped char
                        advance();
                    }
                    if (m_current != '"') {
                        lexer_error("unterminated string literal");
                    } else {
                        push_token(Token_type::String_literal, startpos, m_pos + 1);
                    }
                }

                // if we reach here, it means we have an unexpected character so print error message
                else {lexer_error("unexpected character: " + std::string(1, m_current));}
                m_state = Lexer_state::Start; //and manually set state to start since we are not using push_token
                break;
            }
        }
        advance();
    }
}

/// @brief returns the vector of tokens that the lexer has produced
/// @return a reference to the vector of tokens that the lexer has produced
const std::vector<Token>& Lexer::get_tokens() const{
    return m_tokens;
}

/// @brief returns whether the lexer has encountered an error, used for error handling in the lexer
/// @return true if the lexer has encountered an error, false otherwise
bool Lexer::has_error() const {
    return m_has_error;
}

/// @brief prints an error message and exits the program, used for error handling in the lexer
/// @param message the error message to print
void Lexer::lexer_error(const std::string& message){
    u64 line = 1;
    for (u64 i = 0; i < m_pos; ++i) {
        if (m_input[i] == '\n') ++line;
    }
    set_terminal_color(Terminal_color::Red);
    std::cerr << "Lexer error at line " << line << ", position " << m_pos << ": " << message << std::endl;
    std::string line_content = m_input.substr(m_input.rfind('\n', m_pos) + 1, m_input.find('\n', m_pos) - m_input.rfind('\n', m_pos) - 1);
    std::cerr << line_content << std::endl;
    for (u64 i = 0; i < m_pos - m_input.rfind('\n', m_pos) - 1; ++i) std::cerr << ' ';
    std::cerr << '^' << std::endl;
    set_terminal_color(Terminal_color::Default);

    m_has_error = true;
}




// TODO: remove the if statements if performance is an issue


/// @brief peeks the next character in the input string without advancing the current position
/// @param n the number of characters to look ahead, default is 1
/// @return the character at the given position, or '\0' if the position is out of bounds
char Lexer::peek(u8 n){
    if (m_pos + n > m_input.size()){ 
        ERROR("peeked past file size. pos: " << m_pos+ n << " file size: " << m_input.size());
        return '\0';
    }
    return m_input[m_pos + n];
}

/// @brief advances the current position in the input string by n characters, and updates the current character
/// @param n the number of characters to advance, default is 1
void Lexer::advance(u8 n){
    if (m_pos + n > m_input.size()){ 
        ERROR("advanced past file size. pos: " << m_pos+ n << " file size: " << m_input.size());
        m_current = '\0'; 
        return;
    }
    m_pos += n;
    m_current = m_input[m_pos];
}


/// @brief reverts the current position in the input string by n characters, and updates the current character
/// @param n the number of characters to revert, default is 1
void Lexer::revert(u8 n){
    if (m_pos < n){ 
        ERROR("reverted past file start. pos: " << m_pos - n << " file size: " << m_input.size());
        return;
    }
    m_pos -= n;
    m_current = m_input[m_pos]; 
}


/// @brief pushes a token to the vector of tokens and updates the lexer state to Start
/// @param type the type of the token to push
/// @param start the start position of the token in the input string
/// @param end the end position of the token in the input string
void Lexer::push_token(Token_type type, u32 start, u32 end){
    if (end > m_input.size()) end = m_input.size(); 
    std::string_view view(m_input.data() + start, end - start);
    m_tokens.push_back(Token(type, view));
    m_state = Lexer_state::Start; 
}



