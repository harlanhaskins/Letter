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

Value *genBinary(IRGenerator &gen, BuiltinFunc::exp_v args, std::function<Value * (Value *, Value *)> block) {
    return [&gen, block](BuiltinFunc::exp_v args) {
        auto v = args[0]->codegen(gen);
        if (!v) return (Value *)nullptr;
        auto v1 = args[1]->codegen(gen);
        if (!v1) return (Value *)nullptr;
        return block(v, v1);
    }(args);
}

void IRGenerator::genBuiltins() {
    
    // create bindings for standard binary instructions, using their LLVM counterparts.
    builtins["+"] = std::make_shared<BuiltinFunc>("+", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return builder.CreateAdd(v, v1, "addtmp");
        });
    });
    builtins["*"] = std::make_shared<BuiltinFunc>("*", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return builder.CreateMul(v, v1, "multmp");
        });
    });
    builtins["-"] = std::make_shared<BuiltinFunc>("-", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return builder.CreateSub(v, v1, "subtmp");
        });
    });
    builtins["/"] = std::make_shared<BuiltinFunc>("/", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateUDiv(v, v1, "divtmp"));
        });
    });
    builtins["<"] = std::make_shared<BuiltinFunc>("<", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpSLT(v, v1, "lttmp"));
        });
    });
    builtins[">"] = std::make_shared<BuiltinFunc>(">", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpSGT(v, v1, "gttmp"));
        });
    });
    builtins[">="] = std::make_shared<BuiltinFunc>(">=", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpSGE(v, v1, "getmp"));
        });
    });
    builtins["<="] = std::make_shared<BuiltinFunc>("<=", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpSLE(v, v1, "letmp"));
        });
    });
    builtins["="] = std::make_shared<BuiltinFunc>("=", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpEQ(v, v1, "eqtmp"));
        });
    });
    builtins["!="] = std::make_shared<BuiltinFunc>("=", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateICmpNE(v, v1, "netmp"));
        });
    });
    builtins["mod"] = std::make_shared<BuiltinFunc>("mod", 2, [this](BuiltinFunc::exp_v args) {
        return genBinary(*this, args, [this](Value *v, Value *v1) {
            return i64Cast(builder.CreateURem(v, v1, "modtmp"));
        });
    });
    
    // create builtins for language constructs (if, do, let, while)
    
    builtins["if"] = std::make_shared<BuiltinFunc>("if", 3, [this](BuiltinFunc::exp_v args) {
        
        // emit the condition
        auto cond = args[0]->codegen(*this);
        if (!cond) return (Value *)nullptr;
        
        // compare the condition to 0
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(module->getContext(), APInt(64, 0)), "ifcond");
        
        // get the current function
        auto f = builder.GetInsertBlock()->getParent();
        
        // basic blocks for each part of the 'if'.
        auto thenbb = BasicBlock::Create(module->getContext(), "then", f);
        auto elsebb = BasicBlock::Create(module->getContext(), "else");
        auto mergebb = BasicBlock::Create(module->getContext(), "ifcont");
        
        // conditionally go to 'then' or 'else'
        builder.CreateCondBr(cmp, thenbb, elsebb);
        builder.SetInsertPoint(thenbb);
        
        // emit the 'then' case
        auto then = args[1]->codegen(*this);
        if (!then) return (Value *)nullptr;
        
        // break to 'ifcont'
        builder.CreateBr(mergebb);
        thenbb = builder.GetInsertBlock();
        
        f->getBasicBlockList().push_back(elsebb);
        builder.SetInsertPoint(elsebb);
        
        // emit the 'else' case
        auto elsev = args[2]->codegen(*this);
        if (!elsev) return (Value *)nullptr;
        
        // break to 'ifcont'
        builder.CreateBr(mergebb);
        elsebb = builder.GetInsertBlock();
        
        // Emit merge block.
        f->getBasicBlockList().push_back(mergebb);
        builder.SetInsertPoint(mergebb);
        
        // make phi node from either then or else.
        PHINode *phi = builder.CreatePHI(then->getType(), 2, "iftmp");
        
        phi->addIncoming(then, thenbb);
        phi->addIncoming(elsev, elsebb);
        
        // return the phi node's value
        return (Value *)phi;
    });
    
    builtins["do"] = std::make_shared<BuiltinFunc>("do", -1, [this](BuiltinFunc::exp_v args) {
        // create a stack variable to store the `do` result
        auto res = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), nullptr, "dores");
        
        // save the old bindings
        auto oldBindings = namedValues;
        
        // emit the first n-1 args (they only really exist for side-effects)
        for (int i = 0; i < args.size() - 1; i++) {
            if (!args[i]->codegen(*this)) return (Value *)nullptr;
        }
        
        // emit the last arg and store its value in the dores alloca.
        Value *ret = args.back()->codegen(*this);
        if (!ret) return (Value *)nullptr;
        builder.CreateStore(ret, res);
        
        // restore the old bindings.
        namedValues = oldBindings;
        return ret;
    });
    
    builtins["let"] = std::make_shared<BuiltinFunc>("let", 2, [this](BuiltinFunc::exp_v args) {
        // force the first argument as a varexp
        VarExp *exp = dynamic_cast<VarExp *>(&*args[0]);
        if (!exp) {
            recordError("First argument to `let` must be a variable.");
            return (Value *)nullptr;
        }
        
        auto name = exp->name;
        
        // gen the binding
        auto v = args[1]->codegen(*this);
        if (!v) return (Value *)nullptr;
        
        // either update or create a new binding
        AllocaInst *curr = namedValues[name];
        if (!curr) {
            curr = builder.CreateAlloca(Type::getInt64Ty(module->getContext()), 0, name);
            namedValues[name] = curr;
        }
        
        // store the value in the binding address
        builder.CreateStore(v, curr);
        
        return (Value *)v;
    });
    
    builtins["while"] = std::make_shared<BuiltinFunc>("while", 2, [this](BuiltinFunc::exp_v args) {
        auto f = builder.GetInsertBlock()->getParent();
        
        // basic blocks for the condition, body, and end of the loop
        auto whilebb = BasicBlock::Create(module->getContext(), "while", f);
        auto bodybb = BasicBlock::Create(module->getContext(), "whilebody");
        auto endbb = BasicBlock::Create(module->getContext(), "whileend");
        
        // break to the while loop
        builder.CreateBr(whilebb);
        builder.SetInsertPoint(whilebb);
        
        // emit condition
        auto cond = args[0]->codegen(*this);
        auto cmp = builder.CreateICmpNE(cond, ConstantInt::get(module->getContext(), APInt(64, 0)), "whilecond");
        
        // conditionally break into the body or the end
        builder.CreateCondBr(cmp, bodybb, endbb);
        
        // add the body to the func
        f->getBasicBlockList().push_back(bodybb);
        whilebb = builder.GetInsertBlock();
        
        // emit the while body
        builder.SetInsertPoint(bodybb);
        args[1]->codegen(*this);
        
        // break back to the condition
        builder.CreateBr(whilebb);
        
        // emit the end
        f->getBasicBlockList().push_back(endbb);
        builder.SetInsertPoint(endbb);
        
        // return 0
        Value *v = ConstantInt::get(Type::getInt64Ty(module->getContext()), 0);
        return v;
    });

    auto printf = genPrintf();
    
    std::vector<Type *> types { Type::getInt64Ty(module->getContext()) };
    auto printType = FunctionType::get(Type::getInt64Ty(module->getContext()), types, false);
    auto func = Function::Create(printType, Function::PrivateLinkage, "print", module.get());
    namedValues.clear();
    
    Value *firstArg = nullptr;
    for (auto &arg: func->args()) {
        arg.setName("arg");
        firstArg = &arg;
    }
    
    auto globalArray = globalStringPtr(module.get(), "%d\n");
    
    Constant* zero = Constant::getNullValue(IntegerType::getInt64Ty(module->getContext()));
    
    std::vector<Constant*> indices = {zero, zero};
    
    auto printfFormat = ConstantExpr::getGetElementPtr(globalArray->getType()->getScalarType()->getContainedType(0), globalArray, indices);
    
    auto bb = BasicBlock::Create(module->getContext(), "entry", func);
    builder.SetInsertPoint(bb);
    
    if (printf) {
        std::vector<Value *> args { printfFormat, firstArg };
        builder.CreateCall(printf, args, "calltmp");
        builder.CreateRet(firstArg);
        addFunction(func);
    } else {
        func->eraseFromParent();
    }
}

int64_t IRGenerator::execute() {
    jit->addModule(std::move(this->module));
    auto main_f = (int64_t (*)())jit->findSymbol("letter_main").getAddress();
    if (main_f) {
        return main_f();
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
    passManager->run(*function);
}

llvm::Value *IRGenerator::genMainFunc(std::vector<std::shared_ptr<Exp>> exps) {
    auto ftype = FunctionType::get(Type::getInt64Ty(module->getContext()), false);
    auto f = Function::Create(ftype, Function::ExternalLinkage, "letter_main", module.get());
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