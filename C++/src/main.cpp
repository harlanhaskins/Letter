//
//  main.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <vector>
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "Driver.hpp"

cl::opt<std::string> filename(cl::Positional, cl::desc("<input file>"), cl::Required);
cl::opt<bool> emitAST("emit-ast", cl::desc("Emit the AST to stdout"));
cl::opt<OptimizationLevel> optimizationLevel(cl::desc("Choose optimization level:"),
                                    cl::values(
                                               clEnumVal(None , "No optimizations, enable debugging"),
                                               clEnumVal(O1, "Enable trivial optimizations"),
                                               clEnumVal(O2, "Enable default optimizations"),
                                               clEnumVal(O3, "Enable expensive optimizations"),
                                               clEnumValEnd));
cl::opt<bool> emitIR("emit-llvm", cl::desc("Emit the generated LLVM IR to stdout"));

int main(int argc, const char * argv[]) {
    cl::ParseCommandLineOptions(argc, argv);
    
    std::ifstream file(filename, std::iostream::binary | std::iostream::ate);
    if (!file) {
        std::cerr << "Could not open file \"" + filename + "\"" << std::endl;
        return EXIT_FAILURE;
    }
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    
    char *code = (char *)malloc(size * sizeof(char));
    file.read(code, size);
    
    Options options = (Options) { filename.c_str(), code, optimizationLevel, emitAST, emitIR };
    
    Driver::run(options);
    
    free(code);
    
    return 0;
}
