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
#include "Analyzer.hpp"
#include "IRGenerator.hpp"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "LetterJIT.h"

Env defaultEnv() {
    Env env;
    env.addFunc(std::make_shared<BuiltinFunc>("+", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("-", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("*", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("/", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("min", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("max", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("=", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("<", 2));
    env.addFunc(std::make_shared<BuiltinFunc>(">", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("<=", 2));
    env.addFunc(std::make_shared<BuiltinFunc>(">=", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("if", 3));
    env.addFunc(std::make_shared<BuiltinFunc>("print", 1));
    env.addFunc(std::make_shared<BuiltinFunc>("and", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("or", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("not", 1));
    env.addFunc(std::make_shared<BuiltinFunc>("mod", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("do", INFINITE_ARITY));
    return env;
}

cl::opt<std::string> filename(cl::Positional, cl::desc("<input file>"), cl::Required);
cl::opt<bool> emitAST("emit-ast", cl::desc("Emit the AST to stdout"));
cl::opt<bool> optim("O", cl::desc("Optimize the code prior to execution"));
cl::opt<bool> emitIR("emit-llvm", cl::desc("Emit the generated LLVM IR to stdout"));

int main(int argc, const char * argv[]) {
    cl::ParseCommandLineOptions(argc, argv);
    
    std::ifstream file(filename.c_str(), std::iostream::binary | std::iostream::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    
    char *testCode = (char *)malloc(size * sizeof(char));
    file.read(testCode, size);
    Parser p(testCode);
    std::vector<std::shared_ptr<Exp>> exps;
    std::vector<std::shared_ptr<UserFunc>> funcs;
    p.parseFile(exps, funcs);
    if (emitAST) {
        for (auto &func: funcs) {
            std::cout << func->dump() << std::endl;
        }
        for (auto &exp: exps) {
            std::cout << exp->dump() << std::endl;
        }
    } else {
        IRGenerator generator(argv[1], optim);
        for (auto &func : funcs) {
            generator.genFunc(func);
        }
        generator.genMainFunc(exps);
        if (emitIR) {
            generator.module->print(llvm::outs(), nullptr);
        } else {
            generator.execute();
        }
    }
    return 0;
}
