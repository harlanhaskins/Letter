//
//  IRGenerator.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef IRGenerator_hpp
#define IRGenerator_hpp

#include <stdio.h>
#include "Exp.hpp"
#include "Func.hpp"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

class IRGenerator {
public:
    llvm::Module *module;
    bool optimized;
    IRGenerator(std::string moduleName, bool optimized): optimized(optimized), builder(getGlobalContext()) {
        this->module = new Module(moduleName, getGlobalContext());
        this->passManager = new legacy::FunctionPassManager(module);
        
        // Set up the optimizer pipeline.
        this->passManager->add(createBasicAliasAnalysisPass());
        this->passManager->add(createInstructionCombiningPass());
        // Promote allocas to registers.
        this->passManager->add(createPromoteMemoryToRegisterPass());
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        // Reassociate expressions.
        this->passManager->add(createReassociatePass());
        // Eliminate Common SubExpressions.
        this->passManager->add(createGVNPass());
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        this->passManager->add(createCFGSimplificationPass());
        this->passManager->doInitialization();
        
        this->genBuiltins();
    };
    ~IRGenerator() {
        delete this->module;
        delete this->passManager;
    }
    llvm::Value *genExp(std::shared_ptr<Exp> exp);
    llvm::Value *genFunc(std::shared_ptr<UserFunc> func);
    llvm::Value *genMainFunc(std::vector<std::shared_ptr<Exp>> exps);
private:
    legacy::FunctionPassManager *passManager;
    llvm::IRBuilder<> builder;
    std::map<std::string, llvm::AllocaInst*> namedValues;
    void genBuiltins();
    llvm::Value * genPrintf();
    llvm::Value *error(std::string message);
    llvm::Value *genNumExp(NumExp exp);
    llvm::Value *genVarExp(VarExp exp);
    llvm::Value *genLetExp(LetExp exp);
    llvm::Value *genFunCallExp(FunCallExp exp);
    llvm::Value *i64Cast(llvm::Value *v);
    llvm::AllocaInst *createEntryBlockAlloca(Function *f,
                                             const std::string &name);
    void createArgumentAllocas(std::vector<std::string> args, Function *f);
};

#endif /* IRGenerator_hpp */
