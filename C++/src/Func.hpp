//
//  Func.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef Func_hpp
#define Func_hpp

#include "Exp.hpp"
#include <stdio.h>
#include <iostream>
#include <vector>

const auto INFINITE_ARITY = -1;

class IRGenerator;

class Func {
public:
    std::string name;
    virtual std::string dump(std::string indent = "") = 0;
    virtual int arity() = 0;
};

class UserFunc: public Func, public SourceItem {
public:
    std::shared_ptr<SourceItem> body;
    std::vector<std::string> args;
    UserFunc(std::string name, std::vector<std::string> args, std::shared_ptr<SourceItem> body, SourceLoc loc): body(move(body)), args(args) {
        this->name = name;
        this->sourceLoc = loc;
    }
    virtual std::string dump(std::string indent = "");
    Function *codegen(IRGenerator &gen);
    Function *codegenProto(IRGenerator &gen);
    virtual int arity();
};

class BuiltinFunc: public Func {
private:
    int _arity;
public:
    typedef std::vector<std::shared_ptr<SourceItem>> source_item_v;
    std::function<Value *(source_item_v args)> codegenBlock;
    BuiltinFunc(std::string name, int arity, std::function<Value *(source_item_v args)> codegenBlock) {
        this->_arity = arity;
        this->name = name;
        this->codegenBlock = codegenBlock;
    }
    virtual std::string dump(std::string indent = "");
    Value *codegenCall(source_item_v &args);
    virtual int arity();
};

#endif /* Func_hpp */
