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
    builtins["+"] = std::make_shared<BuiltinFunc>("+", 2, [this](std::vector<std::shared_ptr<Exp>> args){
        return this->builder.CreateAdd(args[0]->codegen(*this), args[1]->codegen(*this), "addtmp");
    });
    builtins["*"] = std::make_shared<BuiltinFunc>("*", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return builder.CreateMul(args[0]->codegen(*this), args[1]->codegen(*this), "multmp");
    });
    builtins["-"] = std::make_shared<BuiltinFunc>("-", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return builder.CreateSub(args[0]->codegen(*this), args[1]->codegen(*this), "subtmp");
    });
    builtins["/"] = std::make_shared<BuiltinFunc>("/", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateUDiv(args[0]->codegen(*this), args[1]->codegen(*this), "divtmp"));
    });
    builtins["<"] = std::make_shared<BuiltinFunc>("<", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpSLT(args[0]->codegen(*this), args[1]->codegen(*this), "lttmp"));
    });
    builtins[">"] = std::make_shared<BuiltinFunc>(">", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpSGT(args[0]->codegen(*this), args[1]->codegen(*this), "gttmp"));
    });
    builtins[">="] = std::make_shared<BuiltinFunc>(">=", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpSGE(args[0]->codegen(*this), args[1]->codegen(*this), "getmp"));
    });
    builtins["<="] = std::make_shared<BuiltinFunc>("<=", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpSLE(args[0]->codegen(*this), args[1]->codegen(*this), "letmp"));
    });
    builtins["="] = std::make_shared<BuiltinFunc>("=", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpEQ(args[0]->codegen(*this), args[1]->codegen(*this), "eqtmp"));
    });
    builtins["mod"] = std::make_shared<BuiltinFunc>("mod", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateURem(args[0]->codegen(*this), args[1]->codegen(*this), "modtmp"));
    });

    auto printf = genPrintf();
    
    std::vector<Type *> types { Type::getInt64Ty(module->getContext()) };
    auto printType = FunctionType::get(Type::getInt64Ty(module->getContext()), types, false);
    auto func = Function::Create(printType, Function::ExternalLinkage, "print", module.get());
    namedValues.clear();
    
    Value *firstArg = nullptr;
    for (auto &arg: func->args()) {
        arg.setName("arg");
        firstArg = &arg;
    }
    
    auto globalArray = globalStringPtr(module.get(), "%d\n");
    
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

int64_t IRGenerator::execute() {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    LetterJIT jit = LetterJIT();
    this->module->setDataLayout(jit.getTargetMachine().createDataLayout());
    jit.addModule(std::move(this->module));
    auto main = (int64_t (*)())jit.findSymbol("main").getAddress();
    if (main) {
        return main();
    } else {
        std::cerr << "Could not find main function." << std::endl;
        return EXIT_FAILURE;
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
    auto f = Function::Create(printfType, Function::ExternalLinkage, "printf", module.get());
    f->getAttributes().addAttribute(module->getContext(), 1, Attribute::NoAlias);
    return f;
}

void IRGenerator::recordError(std::string error) {
    errors.push_back(error);
}

void IRGenerator::addBinding(std::string name, AllocaInst *inst) {
    namedValues[name] = inst;
}

AllocaInst *IRGenerator::lookupBinding(std::string name) {
    return namedValues[name];
}

AllocaInst *IRGenerator::createEntryBlockAlloca(Function *f,
                                          const std::string &name) {
    IRBuilder<> tmpBuilder(&f->getEntryBlock(),
                     f->getEntryBlock().begin());
    return tmpBuilder.CreateAlloca(Type::getInt64Ty(module->getContext()), 0,
                             name.c_str());
}

void IRGenerator::addFunction(Function *function) {
    verifyFunction(*function);
    if (optimized) {
        passManager->run(*function);
    }
}

llvm::Value *IRGenerator::genMainFunc(std::vector<std::shared_ptr<Exp>> exps) {
    auto ftype = FunctionType::get(Type::getInt64Ty(module->getContext()), false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, "main", module.get());
    auto bb = BasicBlock::Create(module->getContext(), "entry", f);
    builder.SetInsertPoint(bb);
    namedValues.clear();
    for (auto &exp: exps) {
        exp->codegen(*this);
    }
    builder.CreateRet(Constant::getNullValue(Type::getInt64Ty(module->getContext())));
    addFunction(f);
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