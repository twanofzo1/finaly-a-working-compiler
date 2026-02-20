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

int main(int argc, char** argv) {

    #ifdef _WIN32
        #error "Windows is not supported (yet) due to differences in calling convention and name mangling. Linux or WSL is recommended for development."
        exit(1);
    #endif



    if (argc < 2 || argc > 2) {
        std::cerr << "Usage: " << argv[0] << " <source_file>" << std::endl;
        return 1;
    }

    std::ifstream stream(argv[1]);
    std::string input((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
    // Strip \r for consistent line handling (CRLF -> LF)
    input.erase(std::remove(input.begin(), input.end(), '\r'), input.end());

    Lexer lexer(input);
    lexer.lex();

    if (lexer.has_error()) {
        std::cerr << "Lexing failed." << std::endl;
        return 1;
    }

    const auto tokens = lexer.get_tokens();

    #ifndef NDEBUG
    LOG("TOKENCOUNT:" <<tokens.size());
    for (const auto token : tokens){
        std::cout<<token.type<<" "<<token.view<<std::endl;
    }
    #endif
    
    Parser parser(tokens, input);
    parser.parse();
    if (parser.has_error()) {
        std::cerr << "Parsing failed." << std::endl;
        return 1;
    }
    AST ast = parser.get_AST();

    #ifndef NDEBUG
    std::cout<<"\n\n";
    ast.print();
    #endif

    Semantic_analyser analyser(ast, input);
    analyser.analyse();
    if (analyser.has_errors()) {
        std::cerr << "Semantic analysis failed." << std::endl;
        return 1;
    }

    #ifndef NDEBUG
    LOG("Semantic analysis passed.");
    #endif

    IR_Generator ir_gen(ast);
    IR_Program ir = ir_gen.generate();

    #ifndef NDEBUG
    std::cout << "\n── IR (before optimisation) ──\n" << std::endl;
    ir.print();
    #endif

    IR_Optimiser optimiser(ir, input);
    optimiser.optimise();

    #ifndef NDEBUG
    std::cout << "\n── IR (after optimisation) ──\n" << std::endl;
    ir.print();
    #endif

    // ── Code generation ──
    Codegen codegen(ir);
    std::string assembly = codegen.generate();

    #ifndef NDEBUG
    std::cout << "\n── Assembly ──\n" << std::endl;
    std::cout << assembly << std::endl;
    #endif

    // Write assembly to file
    std::ofstream asm_out("output.s");
    asm_out << assembly;
    asm_out.close();

    #ifndef NDEBUG
    LOG("Assembly written to output.s");
    #endif

    return 0;
}