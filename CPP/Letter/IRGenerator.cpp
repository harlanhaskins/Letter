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
    builtins["!="] = std::make_shared<BuiltinFunc>("=", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateICmpNE(args[0]->codegen(*this), args[1]->codegen(*this), "netmp"));
    });
    builtins["mod"] = std::make_shared<BuiltinFunc>("mod", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        return i64Cast(builder.CreateURem(args[0]->codegen(*this), args[1]->codegen(*this), "modtmp"));
    });
    builtins["if"] = std::make_shared<BuiltinFunc>("if", 3, [this](std::vector<std::shared_ptr<Exp>> args) {
        auto cond = args[0]->codegen(*this);
        if (!cond) return (Value *)nullptr;
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(module->getContext(), APInt(cond->getType()->getScalarSizeInBits(), 0)), "ifcond");
        auto f = builder.GetInsertBlock()->getParent();
        auto thenbb = BasicBlock::Create(module->getContext(), "then", f);
        auto elsebb = BasicBlock::Create(module->getContext(), "else");
        auto mergebb = BasicBlock::Create(module->getContext(), "ifcont");
        builder.CreateCondBr(cmp, thenbb, elsebb);
        builder.SetInsertPoint(thenbb);
        
        auto then = args[1]->codegen(*this);
        if (!then) return (Value *)nullptr;
        builder.CreateBr(mergebb);
        thenbb = builder.GetInsertBlock();
        
        f->getBasicBlockList().push_back(elsebb);
        builder.SetInsertPoint(elsebb);
        
        auto elsev = args[2]->codegen(*this);
        
        if (!elsev) return (Value *)nullptr;
        
        builder.CreateBr(mergebb);
        elsebb = builder.GetInsertBlock();
        
        // Emit merge block.
        f->getBasicBlockList().push_back(mergebb);
        builder.SetInsertPoint(mergebb);
        PHINode *phi =
        builder.CreatePHI(then->getType(), 2, "iftmp");
        
        phi->addIncoming(then, thenbb);
        phi->addIncoming(elsev, elsebb);
        return (Value *)phi;
    });
    
    builtins["do"] = std::make_shared<BuiltinFunc>("do", -1, [this](std::vector<std::shared_ptr<Exp>> args) {
        auto parent = builder.GetInsertBlock()->getParent();
        if (!parent) {
            recordError("No parent for `do` block.");
            return (Value *)nullptr;
        }
        auto res = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), nullptr, "dores");
        auto bb = BasicBlock::Create(module->getContext(), "do", parent);
        auto doretbb = BasicBlock::Create(module->getContext(), "doret");
        builder.CreateBr(bb);
        builder.SetInsertPoint(bb);
        auto oldBindings = namedValues;
        for (int i = 0; i < args.size() - 1; i++) {
            if (!args[i]->codegen(*this)) return (Value *)nullptr;
        }
        builder.CreateBr(doretbb);
        parent->getBasicBlockList().push_back(doretbb);
        bb = builder.GetInsertBlock();
        builder.SetInsertPoint(doretbb);
        Value *ret = args.back()->codegen(*this);
        if (!ret) return (Value *)nullptr;
        builder.CreateStore(ret, res);
        namedValues = oldBindings;
        return ret;
    });
    
    builtins["let"] = std::make_shared<BuiltinFunc>("let", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        auto v = args[1]->codegen(*this);
        if (!v) return (Value *)nullptr;
        
        VarExp *var = dynamic_cast<VarExp *>(&*args[0]);
        if (!var) {
            recordError("First argument to 'let' must be a variable.");
            return (Value *)nullptr;
        }
        
        AllocaInst *curr = namedValues[var->name];
        if (!curr) {
            curr = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), 0, var->name);
            namedValues[var->name] = curr;
        }
        builder.CreateStore(v, curr);
        
        return v;
    });
    
    builtins["while"] = std::make_shared<BuiltinFunc>("while", 2, [this](std::vector<std::shared_ptr<Exp>> args) {
        auto f = builder.GetInsertBlock()->getParent();
        if (!f) {
            recordError("No parent for `while` loop.");
            return (Value *)nullptr;
        }
        auto whilebb = BasicBlock::Create(module->getContext(), "while", f);
        auto bodybb = BasicBlock::Create(module->getContext(), "whilebody");
        auto endbb = BasicBlock::Create(module->getContext(), "whileend");
        builder.CreateBr(whilebb);
        builder.SetInsertPoint(whilebb);
        auto cond = args[0]->codegen(*this);
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(module->getContext(), APInt(cond->getType()->getScalarSizeInBits(), 0)), "whilecond");
        builder.CreateCondBr(cmp, bodybb, endbb);
        f->getBasicBlockList().push_back(bodybb);
        whilebb = builder.GetInsertBlock();
        builder.SetInsertPoint(bodybb);
        args[1]->codegen(*this);
        builder.CreateBr(whilebb);
        f->getBasicBlockList().push_back(endbb);
        builder.SetInsertPoint(endbb);
        Value *v = ConstantInt::get(Type::getInt64Ty(module->getContext()), 0);
        return v;
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