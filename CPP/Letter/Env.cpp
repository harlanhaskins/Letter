//
//  Env.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Env.hpp"
using namespace std;

void Env::addGlobal(string name, shared_ptr<Exp> exp) {
    this->globals.insert(make_pair(name, exp));
}

void Env::addFunc(shared_ptr<Func> func) {
    this->funcs.insert(make_pair(func->name, func));
}

shared_ptr<Exp> Env::lookupGlobal(string name) {
    return this->globals[name];
}

shared_ptr<Func> Env::lookupFunc(string name) {
    return this->funcs[name];
}