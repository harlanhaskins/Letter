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

class Func {
public:
    std::string name;
    int arity;
    std::vector<std::string> args;
    virtual std::string dump(std::string indent = "") = 0;
};

class UserFunc: public Func {
    std::unique_ptr<Exp> body;
public:
    UserFunc(std::string name, int arity, std::vector<std::string> args, std::unique_ptr<Exp> body): body(move(body)) {
        this->args = args;
        this->arity = arity;
        this->name = name;
    }
    virtual std::string dump(std::string indent = "");
};

class BuiltinFunc: public Func {
    std::function<Exp (std::vector<std::unique_ptr<Exp>> args)> reduce;
public:
    BuiltinFunc(std::string name, int arity, std::vector<std::string> args, std::function<Exp (std::vector<std::unique_ptr<Exp>> args)> reduce): reduce(reduce) {
        this->args = args;
        this->arity = arity;
        this->name = name;
    }
    virtual std::string dump(std::string indent = "");
};

#endif /* Func_hpp */
