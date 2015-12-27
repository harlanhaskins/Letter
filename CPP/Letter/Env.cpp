//
//  Env.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Env.hpp"

void Env::addGlobal(std::string name, std::shared_ptr<Exp> exp) {
    this->globals.insert(std::pair<std::string, std::shared_ptr<Exp>>(name, exp));
}

void Env::addFunc(std::shared_ptr<Func> func) {
    this->funcs.insert(std::pair<std::string, std::shared_ptr<Func>>(func->name, func));
}

std::shared_ptr<Exp> Env::lookupGlobal(std::string name) {
    return this->globals[name];
}

std::shared_ptr<Func> Env::lookupFunc(std::string name) {
    return this->funcs[name];
}