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

class Exp {
public:
    virtual ~Exp() {}
    virtual std::string dump(std::string indent = "") = 0;
};

class NumExp: public Exp {
public:
    long value;
    NumExp(long value): value(value) {}
    virtual std::string dump(std::string indent = "");
};

class VarExp: public Exp {
public:
    std::string name;
    VarExp(std::string name): name(name) {}
    virtual std::string dump(std::string indent = "");
};

class FunCallExp: public Exp {
public:
    std::string func;
    std::vector<std::unique_ptr<Exp>> args;
    FunCallExp(std::string func, std::vector<std::unique_ptr<Exp>> args): func(func), args(move(args)) {}
    virtual std::string dump(std::string indent = "");
};

class LetExp: public Exp {
public:
    std::string name;
    std::unique_ptr<Exp> binding;
    LetExp(std::string name, std::unique_ptr<Exp> binding): name(name), binding(move(binding)) {}
    virtual std::string dump(std::string indent = "");
};

#endif /* AST_hpp */
