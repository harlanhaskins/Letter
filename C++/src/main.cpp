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
#include "Exp.hpp"
#include "Func.hpp"
#include "Parser.hpp"
#include "IRGenerator.hpp"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "LetterJIT.h"

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
    
    std::ifstream file(filename.c_str(), std::iostream::binary | std::iostream::ate);
    if (!file) {
        std::cerr << "Could not open file \"" + filename + "\"" << std::endl;
        return EXIT_FAILURE;
    }
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    
    char *testCode = (char *)malloc(size * sizeof(char));
    file.read(testCode, size);
    Parser p(testCode);
    std::vector<std::shared_ptr<SourceItem>> exps;
    std::vector<std::shared_ptr<SourceItem>> funcs;
    p.parseFile(exps, funcs);
    if (!p.errors.empty()) {
        for (auto &error: p.errors) {
            std::cerr << error << std::endl;
        }
        return EXIT_FAILURE;
    }
    if (emitAST) {
        for (auto &func: funcs) {
            std::cout << func->dump() << std::endl;
        }
        for (auto &exp: exps) {
            std::cout << exp->dump() << std::endl;
        }
    } else {
        IRGenerator generator(filename, optimizationLevel);
        
        // generate function prototypes so functions can reference undeclared functions
        for (auto &func : funcs) {
            dynamic_cast<UserFunc *>(&*func)->codegenProto(generator);
        }
        for (auto &func : funcs) {
            func->codegen(generator);
        }
        generator.genMainFunc(exps);
        generator.finish();
        if (!generator.errors.empty()) {
            for (auto &error: generator.errors) {
                std::cerr << error << std::endl;
            }
            return EXIT_FAILURE;
        }
        if (emitIR) {
            generator.module->print(llvm::outs(), nullptr);
        } else {
            generator.execute();
        }
    }
    return 0;
}
