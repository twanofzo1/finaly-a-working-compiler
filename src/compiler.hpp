#pragma once
#include "token.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "semantics.hpp"
#include "ir.hpp"
#include "optimiser.hpp"
#include "codegen.hpp"
#include <fstream>
#include <string>
#include <iostream>
#include <algorithm>





class Compiler
{
private:
    std::string m_input;
    std::string m_file_dir;    // directory of the source file (for resolving imports)
    std::vector<Token> m_tokens;
    AST m_ast;
    IR_Program m_ir;
    IR_Program m_optimised_ir;
    std::string m_assembly;

    bool lex();
    bool parse();
    bool analyse();
    bool generate_ir();
    bool optimise_ir();
    bool generate_assembly();
public:
    Compiler();
    void init(int argc, char** argv);
    void run();
    void cleanup();

    void print_tokens() const;
    void print_ast() const;
    void print_ir() const;
    void print_optimised_ir() const;
    void print_assembly() const;

};





