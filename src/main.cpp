#include "compiler.hpp"

int main(int argc, char** argv) {
    Compiler compiler;
    compiler.init(argc, argv);
    compiler.run();
    compiler.cleanup();
    return 0;
}