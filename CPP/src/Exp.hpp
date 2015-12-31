//
//  AST.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef AST_hpp
#define AST_hpp

#include <stdio.h>

#include <stdlib.h>
#include <vector>
#include <iostream>
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

class IRGenerator;

using namespace llvm;

class Exp {
public:
    virtual ~Exp() {}
    virtual std::string dump(std::string indent = "") = 0;
    virtual Value *codegen(IRGenerator &gen) = 0;
};

class NumExp: public Exp {
public:
    long value;
    NumExp(long value): value(value) {}
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

class VarExp: public Exp {
public:
    std::string name;
    VarExp(std::string name): name(name) {}
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

class FunCallExp: public Exp {
public:
    std::string func;
    std::vector<std::shared_ptr<Exp>> args;
    static std::shared_ptr<FunCallExp> create(std::string name, std::vector<std::shared_ptr<Exp>> args);
    FunCallExp(std::string func, std::vector<std::shared_ptr<Exp>> args): func(func), args(args) {}
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

#endif /* AST_hpp */
