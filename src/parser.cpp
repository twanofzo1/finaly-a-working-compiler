/*
Author: Twan Roodenburg
Date: 19/02/2026
File: parser.cpp
Description: 
    The parser for the program, 
    used for converting a vector of tokens into an Abstract Syntax Tree (AST).
    The parser uses a recursive descent approach to parse the tokens.
*/

#include "parser.hpp"


//_________________________________________ Public methods _________________________________________


/// @brief constructs a parser from a token stream and the source input
/// @param tokens  the tokens produced by the lexer
/// @param input   the raw source text (used for error messages)
Parser::Parser(std::vector<Token> tokens, std::string& input)
    : m_tokens(tokens), m_token_index(0), m_input(input) {
    if (!m_tokens.empty()) {
        m_current = m_tokens[0];
    } else {
        m_current = Token();
    }
    m_ast.block_statements.push_back(Block_statement({})); // global scope block
}


/// @brief returns the built AST
AST Parser::get_AST(){
    return m_ast;
}

/// @brief runs the parser, filling the root block with all top-level statements
void Parser::parse(){
    // the first block is the global scope
    while (m_current.type != Token_type::Invalid && m_current.type != Token_type::End_of_file)
    {
        AST_index result = parse_statement();
        m_ast.block_statements[0].statements.push_back(result);
    }
}

/// @brief returns whether the parser has encountered an error during parsing
/// @return  true if an error was encountered, false otherwise
bool Parser::has_error() const {
    return m_has_error;
}



//_________________________________________ Private methods _________________________________________


/// @brief moves the token cursor forward by n positions and updates the current token
/// @param n  number of tokens to skip
void Parser::advance(i8 n){
    ASSERT(
        m_token_index + n <= m_tokens.size(), 
        "advanced past end of token stream. index: " << m_token_index + n << " m_tokens size: " << m_tokens.size()
    );

    m_token_index += n;
    m_current = m_tokens[m_token_index];
}

/// @brief looks ahead n tokens without consuming them and returns the token at that position
/// @param n  offset from the current token
/// @return the token at position current + n
Token Parser::peek(i8 n){
    ASSERT(
        m_token_index + n < m_tokens.size(),
        "peeked past end of token stream. index: " << m_token_index + n << " m_tokens size: " << m_tokens.size()
    );

    return m_tokens[m_token_index + n];
}

/// @brief moves the token cursor backwards by n positions and updates the current token
/// @param n  number of tokens to go back
void Parser::revert(i8 n){
    ASSERT(
        m_token_index >= n, 
        "reverted past beginning of token stream. index: " << m_token_index - n
    );
    
    m_token_index -= n;
    m_current = m_tokens[m_token_index];
}

/// @brief checks if a token type is any operator (binary or assignment)
bool Parser::is_opperator(Token_type type){
    return is_binary_op(type) || is_assignment_op(type);
}

/// @brief checks if a token type is a binary operator (+, -, *, ==, etc.)
bool Parser::is_binary_op(Token_type type){
    switch(type){
        case Token_type::Plus: case Token_type::Minus:
        case Token_type::Multiply: case Token_type::Divide: case Token_type::Modulus:
        case Token_type::Equal: case Token_type::Not_equal:
        case Token_type::Less_than: case Token_type::Greater_than:
        case Token_type::Less_equal: case Token_type::Greater_equal:
        case Token_type::Logical_and: case Token_type::Logical_or:
        case Token_type::Bitwise_and: case Token_type::Bitwise_or: case Token_type::Bitwise_xor:
        case Token_type::Left_shift: case Token_type::Right_shift:
            return true;
        default: return false;
    }
}

/// @brief checks if a token type is an assignment operator (=, +=, -=, etc.)
bool Parser::is_assignment_op(Token_type type){
    switch(type){
        case Token_type::Assign:
        case Token_type::Plus_assign: case Token_type::Minus_assign:
        case Token_type::Multiply_assign: case Token_type::Divide_assign:
        case Token_type::Modulus_assign:
        case Token_type::Bitwise_and_assign: case Token_type::Bitwise_or_assign:
        case Token_type::Bitwise_xor_assign:
        case Token_type::Left_shift_assign: case Token_type::Right_shift_assign:
            return true;
        default: return false;
    }
}

/// @brief checks if a token type is a unary prefix operator (-, !, ~)
bool Parser::is_unary_prefix(Token_type type){
    switch(type){
        case Token_type::Minus:
        case Token_type::Logical_not:
        case Token_type::Bitwise_not:
            return true;
        default: return false;
    }
}

/// @brief checks if a token is a data type keyword (i32, f64, bool, void, string, etc.)
bool Parser::is_data_type(Token token){
    if (token.type != Token_type::Identifier) return false;
    std::string_view v = token.view;
    if (v == "bool" || v == "void" || v == "string") return true;
    if (v.size() >= 2) {
        char prefix = v[0];
        if (prefix == 'u' || prefix == 'i' || prefix == 'f') {
            for (u64 i = 1; i < v.size(); i++) {
                if (!isdigit(v[i])) return false;
            }
            return true;
        }
    }
    return false;
}

/// @brief returns operator precedence (higher = binds tighter), follows C rules
int Parser::get_precedence(Token_type type){
    switch(type){
        case Token_type::Logical_or:                          return 1;
        case Token_type::Logical_and:                         return 2;
        case Token_type::Bitwise_or:                          return 3;
        case Token_type::Bitwise_xor:                         return 4;
        case Token_type::Bitwise_and:                         return 5;
        case Token_type::Equal: case Token_type::Not_equal:   return 6;
        case Token_type::Less_than: case Token_type::Greater_than:
        case Token_type::Less_equal: case Token_type::Greater_equal: return 7;
        case Token_type::Left_shift: case Token_type::Right_shift:   return 8;
        case Token_type::Plus: case Token_type::Minus:        return 9;
        case Token_type::Multiply: case Token_type::Divide:
        case Token_type::Modulus:                             return 10;
        default:{
            ERROR("unexpected tokentype in get_precedence got: " <<type);
            return 0;
        }                                              
    }
}

/// @brief reports a syntax error at the given token and marks the parser as errored
void Parser::syntax_error(std::string msg, Token token){
    set_terminal_color(Terminal_color::Red);
    std::cerr << "Syntax error on line: " << token.get_line(m_input) << " Pos: " << token.get_pos(m_input) << " " << msg << std::endl;
    set_terminal_color(Terminal_color::Default);
    token.print_line(m_input);
    m_has_error = true;
}

/// @brief parses a primary expression (literal, identifier, unary, parenthesised, or call)
/// @return AST index of the parsed expression
AST_index Parser::parse_primary(){
    LOG("parse_primary");

    if (m_current.type == Token_type::Integer){
        return parse_integer();
    }

    if (m_current.type == Token_type::Float_literal){
        return parse_float();
    }

    if (m_current.type == Token_type::String_literal){
        return parse_string();
    }

    // Unary prefix operators: -, !, ~, ++, --
    if (is_unary_prefix(m_current.type)){
        Token_type op = m_current.type;
        advance();
        AST_index operand = parse_primary();
        m_ast.unary_expressions.push_back(Unary_expression(op, operand));
        return AST_index(AST_index_type::Unary_expression, m_ast.unary_expressions.size() - 1);
    }

    // Parenthesized expression
    if (m_current.type == Token_type::Left_parenthesis){
        advance();
        AST_index expr = parse_expression();
        ASSERT(m_current.type == Token_type::Right_parenthesis, "expected ) but got: " << m_current.type);
        advance();
        return expr;
    }

    // Identifier (variable name or function call)
    if (m_current.type == Token_type::Identifier){
        // Look ahead to see if this is a function call
        if (peek(1).type == Token_type::Left_parenthesis && !is_data_type(m_current)) {
            return parse_call_expression();
        }
        return parse_identifier();
    }

    ERROR("unknown primary expression type got: " << m_current.type);
    return AST_index();
}

/// @brief parses an identifier token and adds it to the AST
/// @return AST index pointing to the identifier
AST_index Parser::parse_identifier(){
    LOG("parse_identifier");
    ASSERT(m_current.type == Token_type::Identifier, "expected identifier but got: " << m_current.type);
    std::string name(m_current.view);
    m_ast.identifiers.push_back(name);
    m_ast.identifier_tokens.push_back(m_current);
    AST_index index(AST_index_type::Identifier, m_ast.identifiers.size() - 1);
    advance();
    return index;
}

/// @brief parses a binary expression using precedence climbing
/// @param lhs      the left-hand side already parsed
/// @param min_prec  minimum precedence to keep parsing
/// @return AST index of the resulting binary expression tree
AST_index Parser::parse_binary_expression(AST_index lhs, i32 min_prec) {
    LOG("parse_binary_expression");

    while (is_binary_op(m_current.type) && get_precedence(m_current.type) >= min_prec) {
        Token_type op = m_current.type;
        i32 prec = get_precedence(op);
        advance();

        AST_index rhs = parse_primary();

        // If next op has higher precedence, parse it first (left-to-right associativity)
        while (is_binary_op(m_current.type) && get_precedence(m_current.type) > prec) {
            rhs = parse_binary_expression(rhs, get_precedence(m_current.type));
        }

        m_ast.binary_expressions.push_back(Binary_expression(lhs, op, rhs));
        lhs = AST_index(AST_index_type::Binary_expression, m_ast.binary_expressions.size() - 1);
    }

    return lhs;
}

/// @brief parses an integer literal and stores its value in the AST
/// @return AST index pointing to the integer
AST_index Parser::parse_integer(){
    LOG("push_node_integer");
    ASSERT(
        m_current.type == Token_type::Integer,
        "expected Token_type number as first in parse_integer but got:" << m_current.type
    );
    i32 val;
    val = std::stoi(std::string(m_current.view));
    m_ast.integers.push_back(val);
    m_ast.integer_tokens.push_back(m_current);

    AST_index index(AST_index_type::Integer, m_ast.integers.size() - 1);
    advance();
    return index;
}

/// @brief parses a float literal and stores its value in the AST
/// @return AST index pointing to the float
AST_index Parser::parse_float(){
    LOG("push_node_float");
    ASSERT(
        m_current.type == Token_type::Float_literal,
        "expected Token_type float as first in parse_float but got:" << m_current.type
    );
    double val = std::stod(std::string(m_current.view));
    m_ast.floats.push_back(val);
    m_ast.float_tokens.push_back(m_current);

    AST_index index(AST_index_type::Float_literal, m_ast.floats.size() - 1);
    advance();
    return index;
}

/// @brief parses a string literal and stores its value (without quotes) in the AST
/// @return AST index pointing to the string
AST_index Parser::parse_string(){
    LOG("parse_string");
    ASSERT(
        m_current.type == Token_type::String_literal,
        "expected Token_type string literal but got:" << m_current.type
    );
    // Strip the surrounding quotes from the view
    std::string val(m_current.view.substr(1, m_current.view.size() - 2));
    m_ast.string_literals.push_back(val);
    m_ast.string_tokens.push_back(m_current);

    AST_index index(AST_index_type::String_literal, m_ast.string_literals.size() - 1);
    advance();
    return index;
}

/// @brief parses a function call expression: name(arg1, arg2, ...)
/// @return AST index of the call expression
AST_index Parser::parse_call_expression(){
    LOG("parse_call_expression");

    // Parse the callee identifier
    AST_index callee = parse_identifier();

    // Consume '('
    ASSERT(m_current.type == Token_type::Left_parenthesis,
           "expected ( in function call but got: " << m_current.type);
    advance();

    // Parse argument list
    std::vector<AST_index> arguments;
    if (m_current.type != Token_type::Right_parenthesis) {
        while (true) {
            arguments.push_back(parse_expression());
            if (m_current.type == Token_type::Comma) {
                advance();
            } else {
                break;
            }
        }
    }

    ASSERT(m_current.type == Token_type::Right_parenthesis,
           "expected ) after function call arguments but got: " << m_current.type);
    advance();

    m_ast.call_expressions.push_back(Call_expression(callee, arguments));
    return AST_index(AST_index_type::Call_expression, m_ast.call_expressions.size() - 1);
}

/// @brief parses a block statement: { stmt; stmt; ... }
/// @return AST index of the block
AST_index Parser::parse_block_statement(){
    LOG("parse_block_statement");
    ASSERT(
        m_current.type == Token_type::Left_brace,
        "expected { when starting block statement but got " << m_current.type
    );
    advance();

    std::vector<AST_index> statements;

    while (m_current.type != Token_type::Right_brace)
    {
        statements.push_back(parse_statement());
    }
    advance();

    Block_statement block_statement(statements);
    m_ast.block_statements.push_back(block_statement);
    AST_index index(AST_index_type::Block_statement, m_ast.block_statements.size() - 1);
    return index;
}

/// @brief parses an if statement with optional else branch
/// @return AST index of the if statement
AST_index Parser::parse_if_statement(){
    LOG("parse_if_statement");
    ASSERT(
        m_current.type == Token_type::If,
        "expected If when starting if statement"
    );
    advance();

    AST_index condition = parse_expression();
    AST_index true_condition = parse_block_statement();
    AST_index false_condition;

    if (m_current.type == Token_type::Else){
        advance();
        false_condition = parse_block_statement();
    }

    If_statement if_statement(condition, true_condition, false_condition);
    m_ast.if_statements.push_back(if_statement);
    
    AST_index index(AST_index_type::If_statement, m_ast.if_statements.size() - 1);
    return index;
}

/// @brief creates a synthetic identifier in the AST (for desugared for-loops)
/// @param name  the identifier name string
/// @return AST index pointing to the new identifier
AST_index Parser::synth_identifier(const std::string& name){
    m_ast.identifiers.push_back(name);
    m_ast.identifier_tokens.push_back(m_current); // use current token for location
    return AST_index(AST_index_type::Identifier, m_ast.identifiers.size() - 1);
}

/// @brief creates a synthetic integer literal in the AST
/// @param value  the integer value
/// @return AST index pointing to the new integer
AST_index Parser::synth_integer(i64 value){
    m_ast.integers.push_back(value);
    m_ast.integer_tokens.push_back(m_current);
    return AST_index(AST_index_type::Integer, m_ast.integers.size() - 1);
}

/// @brief creates a synthetic variable declaration in the AST
/// @param name  identifier AST index for the variable name
/// @param value  AST index for the initialiser expression
/// @return AST index pointing to the new variable declaration
AST_index Parser::synth_var_decl(AST_index name, AST_index value){
    m_ast.variable_declarations.push_back(Variable_declaration(name, AST_index(), value, false));
    return AST_index(AST_index_type::Variable_declaration, m_ast.variable_declarations.size() - 1);
}

/// @brief creates a synthetic binary expression in the AST
/// @param lhs  left-hand side AST index
/// @param op   the operator token type
/// @param rhs  right-hand side AST index
/// @return AST index pointing to the new binary expression
AST_index Parser::synth_binary(AST_index lhs, Token_type op, AST_index rhs){
    m_ast.binary_expressions.push_back(Binary_expression(lhs, op, rhs));
    return AST_index(AST_index_type::Binary_expression, m_ast.binary_expressions.size() - 1);
}

/// @brief creates a synthetic assignment expression in the AST
/// @param target  target identifier AST index
/// @param op      assignment operator
/// @param value   value expression AST index
/// @return AST index pointing to the new assignment expression
AST_index Parser::synth_assignment(AST_index target, Token_type op, AST_index value){
    m_ast.assignment_expressions.push_back(Assignment_expression(target, op, value));
    return AST_index(AST_index_type::Assignment_expression, m_ast.assignment_expressions.size() - 1);
}

/// @brief parses a for loop in one of three forms:
///   1. for { body }                       — infinite loop
///   2. for N { body }                     — loop N times
///   3. for x = step in start..end { body } — range-based loop
/// All forms are desugared into the existing For_statement(init, cond, post, block).
/// @return AST index of the for statement
AST_index Parser::parse_for_statement(){
    LOG("parse_for_statement");
    ASSERT(
        m_current.type == Token_type::For,
        "expected For when starting for statement"
    );
    advance();

    AST_index init;
    AST_index condition;
    AST_index post;
    AST_index block;

    // Form 1: for { body } — infinite loop
    if (m_current.type == Token_type::Left_brace) {
        block = parse_block_statement();
        For_statement for_statement(init, condition, post, block);
        m_ast.for_statements.push_back(for_statement);
        return AST_index(AST_index_type::For_statement, m_ast.for_statements.size() - 1);
    }

    ///   3. for x = step in start..end { body } — range-based loop
    //   for x = step in start..end { body }
    //   for x in start..end { body }         (step defaults to 1)
    // Identified by: identifier followed by '=' or 'in'
    if (m_current.type == Token_type::Identifier &&
        (peek(1).type == Token_type::Assign || peek(1).type == Token_type::In)) {

        std::string var_name(m_current.view);
        advance(); // skip variable name

        // Parse optional step: = step
        AST_index step;
        if (m_current.type == Token_type::Assign) {
            advance(); // skip '='
            step = parse_primary();
        } else {
            // Default step of 1
            step = synth_integer(1);
        }

        // Expect 'in' keyword
        if (m_current.type != Token_type::In) {
            syntax_error("expected 'in' in range-based for loop", m_current);
            return AST_index();
        }
        advance(); // skip 'in'

        // Parse start expression
        AST_index start = parse_primary();

        // Expect '..'
        if (m_current.type != Token_type::Dot_dot) {
            syntax_error("expected '..' in range-based for loop", m_current);
            return AST_index();
        }
        advance(); // skip '..'

        // Parse end expression
        AST_index end = parse_primary();

        // Desugar init: var x := start  (declares the loop variable in the for scope)
        AST_index var_id = synth_identifier(var_name);
        init = synth_var_decl(var_id, start);

        // Desugar condition: x < end
        AST_index cond_id = synth_identifier(var_name);
        condition = synth_binary(cond_id, Token_type::Less_than, end);

        // Desugar post: x = x + step
        AST_index post_target = synth_identifier(var_name);
        AST_index post_read = synth_identifier(var_name);
        AST_index increment = synth_binary(post_read, Token_type::Plus, step);
        post = synth_assignment(post_target, Token_type::Assign, increment);

        block = parse_block_statement();

        For_statement for_statement(init, condition, post, block);
        m_ast.for_statements.push_back(for_statement);
        return AST_index(AST_index_type::For_statement, m_ast.for_statements.size() - 1);
    }

    // Form 2: for N { body } — loop N times
    // Anything else before '{' is treated as a count expression
    {
        AST_index count = parse_expression();

        // Generate a unique synthetic counter variable name
        std::string counter_name = "__for_" + std::to_string(m_synth_counter++);

        // Desugar: var __for_N := 0
        AST_index var_id = synth_identifier(counter_name);
        AST_index zero = synth_integer(0);
        init = synth_var_decl(var_id, zero);

        // Desugar: __for_N < count
        AST_index cond_id = synth_identifier(counter_name);
        condition = synth_binary(cond_id, Token_type::Less_than, count);

        // Desugar: __for_N = __for_N + 1
        AST_index post_target = synth_identifier(counter_name);
        AST_index post_read = synth_identifier(counter_name);
        AST_index one = synth_integer(1);
        AST_index increment = synth_binary(post_read, Token_type::Plus, one);
        post = synth_assignment(post_target, Token_type::Assign, increment);

        block = parse_block_statement();

        For_statement for_statement(init, condition, post, block);
        m_ast.for_statements.push_back(for_statement);
        return AST_index(AST_index_type::For_statement, m_ast.for_statements.size() - 1);
    }
}

/// @brief parses a full expression (primary, then assignment or binary ops)
/// @return AST index of the parsed expression
AST_index Parser::parse_expression(){
    LOG("parse_expression");

    AST_index lhs = parse_primary();

    // Assignment (right-to-left associative)
    if (is_assignment_op(m_current.type)){
        Token_type op = m_current.type;
        advance();
        AST_index rhs = parse_expression(); // right-to-left: recurse fully
        m_ast.assignment_expressions.push_back(Assignment_expression(lhs, op, rhs));
        return AST_index(AST_index_type::Assignment_expression, m_ast.assignment_expressions.size() - 1);
    }

    // Binary operators with precedence climbing
    if (is_binary_op(m_current.type)){
        return parse_binary_expression(lhs, 0);
    }

    return lhs;
}

/// @brief parses a data type token (e.g. i32, f64, bool, void)
/// @return AST index of the datatype node
AST_index Parser::parse_datatype(){
    ASSERT(is_data_type(m_current), "expected datatype but got: " << m_current.type);
    std::string_view v = m_current.view;
    Datatype_kind kind;
    u32 bit_width = 0;

    if (v == "bool") {
        kind = Datatype_kind::Bool;
        bit_width = 1;
    } else if (v == "void") {
        kind = Datatype_kind::Void;
        bit_width = 0;
    } else if (v == "string") {
        kind = Datatype_kind::String;
        bit_width = 64;
    } else {
        char prefix = v[0];
        std::string digits(v.substr(1));
        bit_width = std::stoi(digits);
        if (prefix == 'u') {
            kind = Datatype_kind::Unsigned_int;
        } else if (prefix == 'i') {
            kind = Datatype_kind::Signed_int;
        } else if (prefix == 'f') {
            kind = Datatype_kind::Float;
        } else {
            ERROR("unknown datatype prefix: " << prefix);
            return AST_index();
        }
    }

    m_ast.datatypes.push_back(Datatype(kind, bit_width));
    AST_index index(AST_index_type::Datatype, m_ast.datatypes.size() - 1);
    advance();
    return index;
}

/// @brief parses a function declaration: fn name(params) : return_type { body }
/// @return AST index of the function declaration
AST_index Parser::parse_function_declaration(){
    LOG("parse_function_declaration");
    ASSERT(
        m_current.type == Token_type::Function,
        "expected fn when starting function declaration but got" << m_current.type
    );
    advance();

    AST_index name = parse_identifier();
    if (m_current.type != Token_type::Left_parenthesis){
        syntax_error("Parsing function declaration failed, expected: left parenthesis",m_current);
        return AST_index();
    }
    advance();

    std::vector<AST_index> parameter_names;
    std::vector<AST_index> datatypes;
    if (is_data_type(m_current)){
        while (true)
        {
            AST_index datatype = parse_datatype();
            datatypes.push_back(datatype);
            if (m_current.type != Token_type::Identifier){
                syntax_error("Parsing function declaration failed, expected: parameter name",m_current);
                return AST_index();
            }
            AST_index param_name = parse_identifier();
            parameter_names.push_back(param_name);

            if (m_current.type == Token_type::Comma){
                advance();
            } else if (m_current.type == Token_type::Right_parenthesis){
                advance();
                break;
            } else {
                syntax_error("Parsing function declaration failed, expected: comma or right parenthesis",m_current);
                return AST_index();
            }
        }
    } else{
        if (m_current.type == Token_type::Right_parenthesis){
            advance();
        } else{
            syntax_error("Parsing function declaration failed, expected: right parenthesis",m_current);
            return AST_index();
        }
    }

    AST_index return_type;

    if (m_current.type == Token_type::Colon){
        advance();
        if (!is_data_type(m_current)){
            syntax_error("Parsing function declaration failed, expected: function return type",m_current);
            return AST_index();
        }
        return_type = parse_datatype();
    }

    AST_index block = parse_block_statement();

    Function_declaration func(name,datatypes,parameter_names,block,return_type);
    m_ast.function_declarations.push_back(func);
    AST_index index(AST_index_type::Function_declaration,m_ast.function_declarations.size() -1);
    return index;
}

/// @brief parses a return statement with an optional return expression
/// @return AST index of the return statement
AST_index Parser::parse_return_statement(){
    LOG("parse_return_statement");
    ASSERT(
        m_current.type == Token_type::Return,
        "expected return keyword but got " << m_current.type
    );
    advance();

    AST_index value;
    // If next token is not ; or } or EOF, parse the return expression
    if (m_current.type != Token_type::Semicolon &&
        m_current.type != Token_type::Right_brace &&
        m_current.type != Token_type::End_of_file) {
        value = parse_expression();
    }

    if (m_current.type == Token_type::Semicolon) {
        advance();
    }

    m_ast.return_statements.push_back(Return_statement(value));
    return AST_index(AST_index_type::Return_statement, m_ast.return_statements.size() - 1);
}

/// @brief parses a variable or const declaration: var/const name : type = expr;
/// @param is_const  true if this is a const declaration
/// @return AST index of the variable declaration
AST_index Parser::parse_variable_declaration(bool is_const){
    LOG("parse_variable_declaration");
    if (m_current.type != Token_type::Var && m_current.type != Token_type::Const){
        syntax_error("Parsing variable declaration failed, expected: const or var",m_current);
        return AST_index();
    }
    // current token is var or const, skip it
    advance();

    // expect identifier (variable name)
    if (m_current.type != Token_type::Identifier) {
        syntax_error("Variable declaration expected identifier", m_current);
        return AST_index();
    }
    AST_index name = parse_identifier();

    AST_index datatype;
    AST_index value;

    if (m_current.type == Token_type::Colon) {
        advance();

        // var x := expr;   (inferred type)
        if (m_current.type == Token_type::Assign) {
            advance();
            value = parse_expression();
        }
        // var x : type = expr;   (explicit type)
        else if (is_data_type(m_current)) {
            datatype = parse_datatype();

            if (m_current.type == Token_type::Assign) {
                advance();
                value = parse_expression();
            }
        } else {
            syntax_error("Variable declaration expected type or :=", m_current);
            return AST_index();
        }
    } else {
        syntax_error("Variable declaration expected ':'", m_current);
        return AST_index();
    }

    if (m_current.type == Token_type::Semicolon) {
        advance();
    }

    m_ast.variable_declarations.push_back(Variable_declaration(name, datatype, value, is_const));
    return AST_index(AST_index_type::Variable_declaration, m_ast.variable_declarations.size() - 1);
}


/// @brief parses a single statement (expression, block, if, for, fn, return, var, const)
/// @return AST index of the parsed statement
AST_index Parser::parse_statement(){
    LOG("parse_statement");
    switch (m_current.type)
    {
        case Token_type::Integer:
        case Token_type::Float_literal:
        case Token_type::String_literal:
        case Token_type::Identifier:
        case Token_type::Minus:
        case Token_type::Logical_not:
        case Token_type::Bitwise_not:
        case Token_type::Left_parenthesis:{
            AST_index expr = parse_expression();
            if (m_current.type == Token_type::Semicolon) advance();
            return expr;
        }
        
        case Token_type::Left_brace:{
            return parse_block_statement();
        }

        case Token_type::If:{
            return parse_if_statement();
        }

        case Token_type::For:{
            return parse_for_statement();
        }

        case Token_type::Function:{
            return parse_function_declaration();
        }

        case Token_type::Return:{
            return parse_return_statement();
        }

        case Token_type::Var:{
            return parse_variable_declaration(false);
        }

        case Token_type::Const:{
            return parse_variable_declaration(true);
        }
        
        default:{
            WARNING("unknown statement type got: " << m_current.type);
            advance();
            break;
        }
    }
    return AST_index();
}

