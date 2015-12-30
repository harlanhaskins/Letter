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

shared_ptr<FunCallExp> FunCallExp::create(std::string name, std::vector<std::shared_ptr<Exp> > args) {
    if (name == "if") return make_shared<IfExp>(args);
    if (name == "let") return make_shared<LetExp>(args);
    if (name == "do") return make_shared<DoExp>(args);
    return make_shared<FunCallExp>(name, args);
}

string NumExp::dump(string indent) {
    return indent + "NumExp " + to_string(this->value);
}

Value *NumExp::codegen(IRGenerator &gen) {
    return ConstantInt::get(gen.module->getContext(), APInt(64, value));
}

string VarExp::dump(string indent) {
    return indent + "VarExp \"" + this->name + "\"";
}

Value *VarExp::codegen(IRGenerator &gen) {
    // Look this variable up in the function.
    Value *v = gen.lookupBinding(name);
    if (!v) {
        gen.recordError("Unknown variable name \"" + name + "\"");
        return nullptr;
    }
    return gen.builder.CreateLoad(v, name);
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

Value *FunCallExp::codegen(IRGenerator &gen) {
    auto handleArityMismath = [this, &gen](int expected, int got) {
        gen.recordError("Invalid number of arguments to function " + func + ". Expected " + std::to_string(expected) + " got " + std::to_string(got));
    };
    if (auto builtin = gen.builtins[func]) {
        if (builtin->arity() != args.size()) {
            handleArityMismath(builtin->arity(), args.size());
            return nullptr;
        }
        return builtin->codegenCall(args);
    }
    Function *parent = gen.module->getFunction(func);
    if (!parent) {
        gen.recordError("Unknown function \"" + func + "\"");
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

Value *LetExp::codegen(IRGenerator &gen) {
    auto v = binding->codegen(gen);
    if (!v) return nullptr;
    
    AllocaInst *curr = gen.namedValues[name];
    if (!curr) {
        curr = gen.builder.CreateAlloca(Type::getInt64Ty(gen.module->getContext()), 0, name);
        gen.namedValues[name] = curr;
    }
    gen.builder.CreateStore(v, curr);
    
    return v;
}

Value *DoExp::codegen(IRGenerator &gen) {
    auto parent = gen.builder.GetInsertBlock()->getParent();
    if (!parent) {
        gen.recordError("No parent for `do` block.");
        return nullptr;
    }
    auto res = gen.builder.CreateAlloca(Type::getInt64Ty(gen.module->getContext()), nullptr, "dores");
    auto bb = BasicBlock::Create(gen.module->getContext(), "do", parent);
    auto doretbb = BasicBlock::Create(gen.module->getContext(), "doret");
    gen.builder.CreateBr(bb);
    gen.builder.SetInsertPoint(bb);
    auto oldBindings = gen.namedValues;
    for (int i = 0; i < args.size() - 1; i++) {
        if (!args[i]->codegen(gen)) return nullptr;
    }
    gen.builder.CreateBr(doretbb);
    parent->getBasicBlockList().push_back(doretbb);
    bb = gen.builder.GetInsertBlock();
    gen.builder.SetInsertPoint(doretbb);
    Value *ret = args.back()->codegen(gen);
    if (!ret) return nullptr;
    gen.builder.CreateStore(ret, res);
    gen.namedValues = oldBindings;
    return ret;
}

Value *IfExp::codegen(IRGenerator &gen) {
    auto cond = condExp->codegen(gen);
    if (!cond) return nullptr;
    auto cmp = gen.builder.CreateICmpNE(cond, ConstantInt::get(gen.module->getContext(), APInt(cond->getType()->getScalarSizeInBits(), 0)), "ifcond");
    auto f = gen.builder.GetInsertBlock()->getParent();
    auto thenbb = BasicBlock::Create(gen.module->getContext(), "then", f);
    auto elsebb = BasicBlock::Create(gen.module->getContext(), "else");
    auto mergebb = BasicBlock::Create(gen.module->getContext(), "ifcont");
    gen.builder.CreateCondBr(cmp, thenbb, elsebb);
    gen.builder.SetInsertPoint(thenbb);
    
    auto then = thenExp->codegen(gen);
    if (!then) return nullptr;
    gen.builder.CreateBr(mergebb);
    thenbb = gen.builder.GetInsertBlock();
    
    f->getBasicBlockList().push_back(elsebb);
    gen.builder.SetInsertPoint(elsebb);
    
    auto elsev = elseExp->codegen(gen);
    
    if (!elsev) return nullptr;
    
    gen.builder.CreateBr(mergebb);
    elsebb = gen.builder.GetInsertBlock();
    
    // Emit merge block.
    f->getBasicBlockList().push_back(mergebb);
    gen.builder.SetInsertPoint(mergebb);
    PHINode *phi =
    gen.builder.CreatePHI(then->getType(), 2, "iftmp");
    
    phi->addIncoming(then, thenbb);
    phi->addIncoming(elsev, elsebb);
    return phi;
}
