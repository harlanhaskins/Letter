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

typedef struct SourceLoc {
    int column;
    int line;
} SourceLoc;

class SourceItem {
public:
    SourceLoc sourceLoc;
    int getLine() { return this->sourceLoc.line; }
    int getColumn() { return this->sourceLoc.column; }
    std::string dumpLoc() { return "[line " + std::to_string(getLine()) + ", column " + std::to_string(getColumn()) + "]"; }
    virtual std::string dump(std::string indent = "") = 0;
    virtual Value *codegen(IRGenerator &gen) = 0;
};

class NumExp: public SourceItem {
public:
    long value;
    NumExp(long value, SourceLoc loc): value(value) { this->sourceLoc = loc; }
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

class VarExp: public SourceItem {
public:
    std::string name;
    VarExp(std::string name, SourceLoc loc): name(name) { this->sourceLoc = loc; }
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

class FunCallExp: public SourceItem {
public:
    std::string func;
    std::vector<std::shared_ptr<SourceItem>> args;
    static std::shared_ptr<FunCallExp> create(std::string name, std::vector<std::shared_ptr<SourceItem>> args);
    FunCallExp(std::string func, std::vector<std::shared_ptr<SourceItem>> args, SourceLoc loc): func(func), args(args) { this->sourceLoc = loc; }
    virtual std::string dump(std::string indent = "");
    virtual Value *codegen(IRGenerator &gen);
};

#endif /* AST_hpp */
