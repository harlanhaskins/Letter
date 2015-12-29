//
//  IRGenerator.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include <iostream>
#include "IRGenerator.hpp"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

Constant *globalStringPtr(Module *m, std::string value) {
    GlobalVariable* globalArray = new GlobalVariable(*m, ArrayType::get(Type::getInt8Ty(m->getContext()), value.size() + 1), true, GlobalValue::PrivateLinkage, 0, ".printfFormat");
    globalArray->setAlignment(1);
    
    // Constant Definitions
    Constant *constArray = ConstantDataArray::getString(m->getContext(), value, true);
    
    // Global Variable Definitions
    globalArray->setInitializer(constArray);
    
    return globalArray;
}

void IRGenerator::genBuiltins() {
    auto printf = genPrintf();
    
    std::vector<Type *> types { Type::getInt64Ty(module->getContext()) };
    auto printType = FunctionType::get(Type::getInt64Ty(module->getContext()), types, false);
    auto func = Function::Create(printType, Function::ExternalLinkage, "print", module);
    namedValues.clear();
    
    Value *firstArg = nullptr;
    for (auto &arg: func->args()) {
        arg.setName("arg");
        firstArg = &arg;
    }
    
    auto globalArray = globalStringPtr(module, "%d\n");
    
    Constant* zero = Constant::getNullValue(IntegerType::getInt32Ty(module->getContext()));
    
    std::vector<Constant*> indices = {zero, zero};
    
    auto printfFormat = ConstantExpr::getGetElementPtr(globalArray->getType()->getScalarType()->getContainedType(0), globalArray, indices);
    
    auto bb = BasicBlock::Create(module->getContext(), "entry", func);
    builder.SetInsertPoint(bb);
    
    if (printf) {
        std::vector<Value *> args { printfFormat, firstArg };
        builder.CreateCall(printf, args, "calltmp");
        builder.CreateRet(firstArg);
        verifyFunction(*func);
        if (optimized) passManager->run(*func);
    } else {
        func->eraseFromParent();
    }
}

/// CreateArgumentAllocas - Create an alloca for each argument and register the
/// argument in the symbol table so that references to it will succeed.
void IRGenerator::createArgumentAllocas(std::vector<std::string> args, Function *f) {
    Function::arg_iterator iter = f->arg_begin();
    for (int i = 0, e = args.size(); i != e; ++i, ++iter) {
        // Create an alloca for this variable.
        AllocaInst *alloca = createEntryBlockAlloca(f, args[i]);
        
        // Store the initial value into the alloca.
        builder.CreateStore(iter, alloca);
        
        // Add arguments to variable symbol table.
        namedValues[args[i]] = alloca;
    }
}

llvm::Value *IRGenerator::genPrintf() {
    std::vector<Type *> args { Type::getInt8PtrTy(module->getContext()) };
    auto printfType = FunctionType::get(Type::getInt64Ty(module->getContext()), args, true);
    auto f = Function::Create(printfType, Function::ExternalLinkage, "printf", module);
    f->getAttributes().addAttribute(module->getContext(), 1, Attribute::NoAlias);
    return f;
}

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
    return ConstantInt::get(module->getContext(), APInt(64, exp.value));
}

Value *IRGenerator::genVarExp(VarExp exp) {
    // Look this variable up in the function.
    Value *v = namedValues[exp.name];
    if (!v) error("Unknown variable name \"" + exp.name + "\"");
    return builder.CreateLoad(v, exp.name);
}

Value *IRGenerator::genLetExp(LetExp exp) {
    auto v = genExp(exp.binding);
    if (!v) return nullptr;
    
    AllocaInst *curr = namedValues[exp.name];
    if (!curr) {
        curr = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), 0, exp.name);
        namedValues[exp.name] = curr;
    }
    builder.CreateStore(v, curr);
    
    return v;
}

AllocaInst *IRGenerator::createEntryBlockAlloca(Function *f,
                                          const std::string &name) {
    IRBuilder<> tmpBuilder(&f->getEntryBlock(),
                     f->getEntryBlock().begin());
    return tmpBuilder.CreateAlloca(Type::getInt64Ty(module->getContext()), 0,
                             name.c_str());
}

Value *IRGenerator::genFunc(std::shared_ptr<UserFunc> func) {
    std::vector<Type *> types(func->arity(), Type::getInt64Ty(module->getContext()));
    auto ftype = FunctionType::get(Type::getInt64Ty(module->getContext()), types, false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, func->name, module);
    unsigned idx = 0;
    for (auto &arg: f->args()) {
        arg.setName(func->args[idx++]);
    }
    auto bb = BasicBlock::Create(module->getContext(), "entry", f);
    builder.SetInsertPoint(bb);
    namedValues.clear();
    createArgumentAllocas(func->args, f);
    
    if (Value *ret = genExp(func->body)) {
        builder.CreateRet(ret);
        verifyFunction(*f);
        if (optimized) passManager->run(*f);
        return f;
    }
    f->eraseFromParent();
    return nullptr;
}

llvm::Value *IRGenerator::genMainFunc(std::vector<std::shared_ptr<Exp>> exps) {
    auto ftype = FunctionType::get(Type::getInt64Ty(module->getContext()), false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, "main", module);
    auto bb = BasicBlock::Create(module->getContext(), "entry", f);
    builder.SetInsertPoint(bb);
    namedValues.clear();
    for (auto &exp: exps) {
        genExp(exp);
    }
    builder.CreateRet(Constant::getNullValue(Type::getInt64Ty(module->getContext())));
    verifyFunction(*f);
    if (optimized) {
        passManager->run(*f);
    }
    return f;
}

Value *IRGenerator::i64Cast(Value *v) {
    return builder.CreateIntCast(v, Type::getInt64Ty(module->getContext()), true);
}

void IRGenerator::printBindings() {
    std::cerr << "Printing bindings:" << std::endl;
    for (auto iterator = namedValues.begin(); iterator != namedValues.end(); iterator++) {
        std::cerr << "    " << iterator->first << ": ";
        iterator->second->dump();
    }
}

Value *IRGenerator::genDoFunc(FunCallExp exp, Function *parent) {
    auto res = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), nullptr,"dores");
    auto bb = BasicBlock::Create(module->getContext(), "do", parent);
    auto doretbb = BasicBlock::Create(module->getContext(), "doret");
    builder.CreateBr(bb);
    builder.SetInsertPoint(bb);
    auto oldBindings = namedValues;
    for (int i = 0; i < exp.args.size() - 1; i++) {
        if (!genExp(exp.args[i])) return nullptr;
    }
    builder.CreateBr(doretbb);
    parent->getBasicBlockList().push_back(doretbb);
    bb = builder.GetInsertBlock();
    builder.SetInsertPoint(doretbb);
    Value *ret = genExp(exp.args.back());
    if (!ret) return nullptr;
    builder.CreateStore(ret, res);
    namedValues = oldBindings;
    return ret;
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
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(module->getContext(), APInt(cond->getType()->getScalarSizeInBits(), 0)), "ifcond");
        auto f = builder.GetInsertBlock()->getParent();
        auto thenbb = BasicBlock::Create(module->getContext(), "then", f);
        auto elsebb = BasicBlock::Create(module->getContext(), "else");
        auto mergebb = BasicBlock::Create(module->getContext(), "ifcont");
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
        std::map<std::string, AllocaInst *> oldBindings = namedValues;
        auto func = builder.GetInsertBlock()->getParent();
        return genDoFunc(exp, func);
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