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

void usage(const char *progName) {
    std::cerr << "Usage: " << progName << " [FILE]" << std::endl;
}

int main(int argc, const char * argv[]) {
    if (argc < 2) {
        usage(argv[0]);
        return EXIT_FAILURE;
    }
    std::ifstream file(argv[1], std::iostream::binary | std::iostream::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    char *testCode = (char *)malloc(size * sizeof(char));
    file.read(testCode, size);
    Parser p(testCode);
    std::vector<std::shared_ptr<Exp>> exps;
    std::vector<std::shared_ptr<UserFunc>> funcs;
    p.parseFile(exps, funcs);
    IRGenerator generator(argv[1], true);
    for (auto &func : funcs) {
        generator.genFunc(func);
    }
    generator.genMainFunc(exps);
    generator.module->print(llvm::outs(), nullptr);
    return 0;
}
