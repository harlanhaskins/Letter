//
//  AST.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Exp.hpp"
#include "IRGenerator.hpp"

using namespace std;

string NumExp::dump(string indent) {
    return indent + dumpLoc() + ": NumExp " + to_string(this->value);
}

Value *NumExp::codegen(IRGenerator &gen) {
    return ConstantInt::get(gen.module->getContext(), APInt(64, value));
}

string VarExp::dump(string indent) {
    return indent + dumpLoc() + ": VarExp \"" + this->name + "\"";
}

Value *VarExp::codegen(IRGenerator &gen) {
    // Look this variable up in the function.
    AllocaInst *v = gen.lookupBinding(name);
    if (!v) {
        gen.recordError("Unknown variable name \"" + name + "\"", *this);
        return nullptr;
    }
    return gen.builder.CreateLoad(v, name);
}

string FunCallExp::dump(string indent) {
    auto s = indent + dumpLoc() + ": FunCallExp \"" + this->func + "\" args=[\n";
    for (size_t i = 0; i < this->args.size(); i++) {
        if (i != 0) {
            s += ",\n";
        }
        s += this->args[i]->dump(indent + "    ");
    }
    s += "\n" + indent + "]";
    return s;
}

Value *FunCallExp::codegen(IRGenerator &gen) {
    auto handleArityMismath = [this, &gen](int expected, int got) {
        gen.recordError("Invalid number of arguments to function " + func + ". Expected " + std::to_string(expected) + " got " + std::to_string(got), *this);
    };
    if (auto builtin = gen.builtins[func]) {
        if (builtin->arity() != INFINITE_ARITY&& builtin->arity() != args.size()) {
            handleArityMismath(builtin->arity(), args.size());
            return nullptr;
        }
        return builtin->codegenCall(args);
    }
    Function *parent = gen.module->getFunction(func);
    if (!parent) {
        gen.recordError("Unknown function \"" + func + "\"", *this);
        return nullptr;
    }
    if (parent->arg_size() != args.size()) {
        handleArityMismath(parent->arg_size(), args.size());
        return nullptr;
    }
    std::vector<Value *> genArgs;
    for (auto &arg: args) {
        auto code = arg->codegen(gen);
        if (!code) return nullptr;
        genArgs.push_back(code);
    }
    return gen.builder.CreateCall(parent, genArgs, "calltmp");
}
