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
#include "Parser.hpp"
#include "Analyzer.hpp"

Env defaultEnv() {
    Env env;
    env.addFunc(std::make_shared<BuiltinFunc>("+", 2));
    env.addFunc(std::make_shared<BuiltinFunc>("-", 2));
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

int main(int argc, const char * argv[]) {
    std::ifstream file("/Users/harlan/Documents/Code/Haskell/Letter/prog01.ltr", std::iostream::binary | std::iostream::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    char *testCode = (char *)malloc(size * sizeof(char));
    file.read(testCode, size);
    Parser p(testCode);
    std::vector<std::shared_ptr<Exp>> exps;
    std::vector<std::shared_ptr<Func>> funcs;
    p.parseFile(exps, funcs);
    Env env = defaultEnv();
    for (auto &func: funcs) {
        env.addFunc(func);
    }
    ExpAnalyzer expAnalyzer(env);
    for (auto &exp : exps) {
        std::vector<std::string> reasons = expAnalyzer.analyze(exp);
        if (!reasons.empty()) {
            for (auto &reason: reasons) {
                std::cout << "Warning: " << reason << std::endl;
            }
        }
    }
    FuncAnalyzer funcAnalyzer(env);
    for (auto &func : funcs) {
        std::vector<std::string> reasons = funcAnalyzer.analyze(func);
        if (!reasons.empty()) {
            for (auto &reason: reasons) {
                std::cout << "Warning: " << reason << std::endl;
            }
        }
    }
    return 0;
}
