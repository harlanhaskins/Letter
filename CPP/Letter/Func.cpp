//
//  Func.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Func.hpp"
#include "Exp.hpp"
#include "IRGenerator.hpp"
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

Value *UserFunc::codegen(IRGenerator &gen) {
    std::vector<Type *> types(arity(), Type::getInt64Ty(gen.module->getContext()));
    auto ftype = FunctionType::get(Type::getInt64Ty(gen.module->getContext()), types, false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, name, gen.module.get());
    unsigned idx = 0;
    for (auto &arg: f->args()) {
        arg.setName(args[idx++]);
    }
    auto bb = BasicBlock::Create(gen.module->getContext(), "entry", f);
    gen.builder.SetInsertPoint(bb);
    gen.namedValues.clear();
    gen.createArgumentAllocas(args, f);
    
    if (Value *ret = body->codegen(gen)) {
        gen.builder.CreateRet(ret);
        gen.addFunction(f);
        return f;
    }
    f->eraseFromParent();
    return nullptr;
}

std::string BuiltinFunc::dump(std::string indent) {
    return "BuiltinFunDef arity = " + std::to_string(this->arity());
}

int UserFunc::arity() {
    return (int)this->args.size();
}

int BuiltinFunc::arity() {
    return this->_arity;
}

Value *BuiltinFunc::codegenCall(std::vector<llvm::Value *> &args) {
    return codegenBlock(args);
}