/*
Author: Twan Roodenburg
Date: 22/02/2026
File: compiler.cpp
Description: 
    the main compiler class, which orchestrates the entire compilation process by invoking 
    the lexer, parser, semantic analyser, IR generator, IR optimiser, and code generator in sequence.
*/


#include "compiler.hpp"

//_________________________________________ Public methods _________________________________________

Compiler::Compiler() {}

void Compiler::init(int argc, char** argv)
{
    #ifdef _WIN32
        std::cerr << "Error: This compiler is not supported on Windows." << std::endl;
        exit(1);
    #endif


    if (argc < 2 || argc > 2) {
        std::cerr << "Usage: " << argv[0] << " <source_file>" << std::endl;
        exit(1);
    }

    std::ifstream stream(argv[1]);
    m_input = std::string((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
    // Strip \r for consistent line handling (CRLF -> LF)
    m_input.erase(std::remove(m_input.begin(), m_input.end(), '\r'), m_input.end());

    // Extract the directory of the source file for resolving @import paths
    std::string filepath(argv[1]);
    auto last_slash = filepath.find_last_of("/\\");
    if (last_slash != std::string::npos) {
        m_file_dir = filepath.substr(0, last_slash);
    } else {
        m_file_dir = ".";
    }
}


void Compiler::run()
{
    if (!lex()) {
        std::cerr << "Compilation failed during lexing." << std::endl;
        exit(1);
    }

    if (!parse()) {
        std::cerr << "Compilation failed during parsing." << std::endl;
        exit(1);
    }

    if (!analyse()) {
        std::cerr << "Compilation failed during semantic analysis." << std::endl;
        exit(1);
    }

    if (!generate_ir()) {
        std::cerr << "Compilation failed during IR generation." << std::endl;
        exit(1);
    }

    if (!optimise_ir()) {
        std::cerr << "Compilation failed during IR optimisation." << std::endl;
        exit(1);
    }

    if (!generate_assembly()) {
        std::cerr << "Compilation failed during code generation." << std::endl;
        exit(1);
    }

}

void Compiler::cleanup(){}


#ifndef NDEBUG
void Compiler::print_tokens() const {
    for (const auto& token : m_tokens) {
        std::cout << token.type << " " << token.view << std::endl;
    }
}
void Compiler::print_ast() const {
    m_ast.print();
}
void Compiler::print_ir() const {
    m_ir.print();
}
void Compiler::print_optimised_ir() const {
    m_optimised_ir.print();
}
void Compiler::print_assembly() const {
    std::cout << m_assembly << std::endl;
}
#else
void Compiler::print_tokens() const {}
void Compiler::print_ast() const {}
void Compiler::print_ir() const {}
void Compiler::print_optimised_ir() const {}
void Compiler::print_assembly() const {}
#endif

//_________________________________________ Private methods _________________________________________



bool Compiler::lex() {
    LOG("Lexer");
    Lexer lexer(m_input);
    lexer.lex();
    if (lexer.has_error()) {
        std::cerr << "Lexing failed." << std::endl;
        return false;
    }
    m_tokens = lexer.get_tokens();
    return true;
}

bool Compiler::parse() {
    LOG("Parser");
    Parser parser(m_tokens, m_input, m_file_dir);
    parser.parse();
    if (parser.has_error()) {
        std::cerr << "Parsing failed." << std::endl;
        return false;
    }
    m_ast = parser.get_AST();
    return true;
}


bool Compiler::analyse() {
    LOG("Semantic analyser");
    Semantic_analyser analyser(m_ast, m_input, m_file_dir);
    analyser.analyse();
    if (analyser.has_errors()) {
        std::cerr << "Semantic analysis failed." << std::endl;
        return false;
    }
    return true;
}


bool Compiler::generate_ir() {
    LOG("IR generation");
    IR_Generator ir_gen(m_ast);
    m_ir = ir_gen.generate();
    return true;
}

bool Compiler::optimise_ir() {
    LOG("IR optimisation");
    m_optimised_ir = m_ir; 
    IR_Optimiser optimiser(m_optimised_ir, m_input);
    optimiser.optimise();
    return true;
}

bool Compiler::generate_assembly() {
    LOG("Code generation");
    Codegen codegen(m_optimised_ir);
    m_assembly = codegen.generate();
    // Write the generated assembly to output.s
    std::ofstream asm_file("output.s");
    asm_file << m_assembly;
    asm_file.close();
    return true;
}