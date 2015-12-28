//
//  IRGenerator.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include <iostream>
#include "IRGenerator.hpp"

using namespace llvm;

Value *IRGenerator::genExp(std::shared_ptr<Exp> exp) {
    auto funCallExp = dynamic_cast<FunCallExp *>(&*exp);
    if (funCallExp) {
        return genFunCallExp(*funCallExp);
    }
    auto numExp = dynamic_cast<NumExp *>(&*exp);
    if (numExp) {
        return genNumExp(*numExp);
    }
    auto varExp = dynamic_cast<VarExp *>(&*exp);
    if (varExp) {
        return genVarExp(*varExp);
    }
    auto letExp = dynamic_cast<LetExp *>(&*exp);
    if (letExp) {
        return genLetExp(*letExp);
    }
    return error("Unknown expression type");
}

Value *IRGenerator::error(std::string message) {
    std::cerr << message << std::endl;
    return nullptr;
}

Value *IRGenerator::genNumExp(NumExp exp) {
    return ConstantInt::get(globalContext, APInt(64, exp.value));
}

Value *IRGenerator::genVarExp(VarExp exp) {
    // Look this variable up in the function.
    Value *v = namedValues[exp.name];
    if (!v) error("Unknown variable name \"" + exp.name + "\"");
    return v;
}

Value *IRGenerator::genLetExp(LetExp exp) {
    return nullptr;
}

Value *IRGenerator::genFunc(std::shared_ptr<UserFunc> func) {
    std::vector<Type *> types(func->arity(), Type::getInt64Ty(globalContext));
    auto ftype = FunctionType::get(Type::getInt64Ty(globalContext), types, false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, func->name, module);
    unsigned idx = 0;
    for (auto &arg: f->args()) {
        arg.setName(func->args[idx++]);
    }
    auto bb = BasicBlock::Create(globalContext, "entry", f);
    builder.SetInsertPoint(bb);
    namedValues.clear();
    
    for (auto &arg : f->args()) {
        namedValues[arg.getName()] = &arg;
    }
    
    if (Value *ret = genExp(func->body)) {
        builder.CreateRet(ret);
        verifyFunction(*f);
        passManager->run(*f);
        return f;
    }
    f->eraseFromParent();
    return nullptr;
}

Value *IRGenerator::i64Cast(Value *v) {
    return builder.CreateIntCast(v, Type::getInt64Ty(globalContext), true);
}

Value *IRGenerator::genFunCallExp(FunCallExp exp) {
    if (exp.func == "+") {
        return builder.CreateAdd(genExp(exp.args[0]), genExp(exp.args[1]), "addtmp");
    }
    if (exp.func == "*") {
        return builder.CreateMul(genExp(exp.args[0]), genExp(exp.args[1]), "multmp");
    }
    if (exp.func == "-") {
        return builder.CreateSub(genExp(exp.args[0]), genExp(exp.args[1]), "subtmp");
    }
    if (exp.func == "/") {
        return i64Cast(builder.CreateUDiv(genExp(exp.args[0]), genExp(exp.args[1]), "divtmp"));
    }
    if (exp.func == "<") {
        return i64Cast(builder.CreateICmpSLT(genExp(exp.args[0]), genExp(exp.args[1]), "lttmp"));
    }
    if (exp.func == ">") {
        return i64Cast(builder.CreateICmpSGT(genExp(exp.args[0]), genExp(exp.args[1]), "gttmp"));
    }
    if (exp.func == ">=") {
        return i64Cast(builder.CreateICmpSGE(genExp(exp.args[0]), genExp(exp.args[1]), "getmp"));
    }
    if (exp.func == "<=") {
        return i64Cast(builder.CreateICmpSLE(genExp(exp.args[0]), genExp(exp.args[1]), "letmp"));
    }
    if (exp.func == "=") {
        return i64Cast(builder.CreateICmpEQ(genExp(exp.args[0]), genExp(exp.args[1]), "eqtmp"));
    }
    if (exp.func == "mod") {
        return i64Cast(builder.CreateURem(genExp(exp.args[0]), genExp(exp.args[1]), "modtmp"));
    }
    if (exp.func == "if") {
        if (exp.args.size() != 3) {
            return error("Invalid number of arguments to function " + exp.func + ". Expected 3 got " + std::to_string(exp.args.size()));
        }
        auto cond = genExp(exp.args[0]);
        if (!cond) return nullptr;
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(globalContext, APInt(cond->getType()->getScalarSizeInBits(), 0)), "ifcond");
        auto f = builder.GetInsertBlock()->getParent();
        auto thenbb = BasicBlock::Create(globalContext, "then", f);
        auto elsebb = BasicBlock::Create(globalContext, "else");
        auto mergebb = BasicBlock::Create(globalContext, "ifcont");
        builder.CreateCondBr(cmp, thenbb, elsebb);
        builder.SetInsertPoint(thenbb);
        
        auto then = genExp(exp.args[1]);
        if (!then) return nullptr;
        builder.CreateBr(mergebb);
        thenbb = builder.GetInsertBlock();
        
        f->getBasicBlockList().push_back(elsebb);
        builder.SetInsertPoint(elsebb);
        
        auto elsev = genExp(exp.args[2]);
        
        if (!elsev) return nullptr;
        
        builder.CreateBr(mergebb);
        elsebb = builder.GetInsertBlock();
        
        // Emit merge block.
        f->getBasicBlockList().push_back(mergebb);
        builder.SetInsertPoint(mergebb);
        PHINode *phi =
        builder.CreatePHI(then->getType(), 2, "iftmp");
        
        phi->addIncoming(then, thenbb);
        phi->addIncoming(elsev, elsebb);
        return phi;
    }
    if (exp.func == "do") {
        
    }
    Function *func = module->getFunction(exp.func);
    if (!func) return error("Unknown function \"" + exp.func + "\"");
    auto args = exp.args;
    if (func->arg_size() != args.size()) {
        return error("Invalid number of arguments to function " + exp.func + ". Expected " + std::to_string(func->arg_size()) + " got " + std::to_string(args.size()));
    }
    std::vector<Value *> genArgs;
    for (auto &arg: exp.args) {
        auto code = genExp(arg);
        if (!code) return nullptr;
        genArgs.push_back(code);
    }
    return builder.CreateCall(func, genArgs, "calltmp");
}