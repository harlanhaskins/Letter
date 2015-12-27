//
//  AST.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Exp.hpp"

using namespace std;

string NumExp::dump(string indent) {
    return indent + "NumExp " + to_string(this->value);
}

string VarExp::dump(string indent) {
    return indent + "VarExp \"" + this->name + "\"";
}

string LetExp::dump(string indent) {
    return indent + "LetExp \"" + this->name + "\" = \n" + this->binding->dump(indent + "    ");
}

string FunCallExp::dump(string indent) {
    auto s = indent + "FunCallExp \"" + this->func + "\" args=[\n";
    for (size_t i = 0; i < this->args.size(); i++) {
        if (i != 0) {
            s += ",\n";
        }
        s += this->args[i]->dump(indent + "    ");
    }
    s += "\n" + indent + "]";
    return s;
}