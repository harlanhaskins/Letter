//
//  Driver.cpp
//  Letter
//
//  Created by Harlan Haskins on 1/2/16.
//  Copyright © 2016 Harlan Haskins. All rights reserved.
//

#include "Driver.hpp"
#include "Parser.hpp"

bool Driver::run(Options o) {
    Parser p(o.input);
    std::vector<std::shared_ptr<SourceItem>> exps;
    std::vector<std::shared_ptr<SourceItem>> funcs;
    p.parseFile(exps, funcs);
    if (!p.errors.empty()) {
        for (auto &error: p.errors) {
            std::cerr << error << std::endl;
        }
        return false;
    }
    if (o.emitAST) {
        for (auto &func: funcs) {
            std::cout << func->dump() << std::endl;
        }
        for (auto &exp: exps) {
            std::cout << exp->dump() << std::endl;
        }
    } else {
        IRGenerator generator(o.filename, o.optimizationLevel);
        
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
            return false;
        }
        if (o.emitIR) {
            generator.module->print(llvm::outs(), nullptr);
        } else {
            generator.execute();
        }
    }
    return true;
}