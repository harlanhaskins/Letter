//
//  Func.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Func.hpp"
#include "Exp.hpp"
#include <iostream>
#include <vector>

using namespace std;

std::string UserFunc::dump(std::string indent) {
    auto s = indent + "FunDef \"" + this->name + "\" args=[";
    for (size_t i = 0; i < this->args.size(); i++) {
        if (i != 0) {
            s += ", ";
        }
        s += this->args[i];
    }
    s += indent + "] \n" + this->body->dump(indent + "    ");
    return s;
}