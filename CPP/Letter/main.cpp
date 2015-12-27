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
#include "Parser.hpp"

int main(int argc, const char * argv[]) {
    std::ifstream file("/Users/harlan/Documents/Code/Haskell/Letter/prog01.ltr", std::iostream::binary | std::iostream::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    char *testCode = (char *)malloc(size * sizeof(char));
    file.read(testCode, size);
    Parser p(testCode);
    std::vector<std::unique_ptr<Exp>> exps;
    std::vector<std::unique_ptr<Func>> funcs;
    p.parseFile(exps, funcs);
    for (auto &exp : exps) {
        std::cout << exp->dump() << "\n";
    }
    for (auto &func : funcs) {
        std::cout << func->dump() << "\n";
    }
    return 0;
}
