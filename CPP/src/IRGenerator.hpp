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
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "LetterJIT.h"

class IRGenerator {
public:
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;
    std::map<std::string, std::shared_ptr<BuiltinFunc>> builtins;
    std::map<std::string, llvm::AllocaInst*> namedValues;
    bool optimized;
    std::vector<std::string> errors;
    IRGenerator(std::string moduleName, bool optimized): optimized(optimized), builder(getGlobalContext()) {
        
        this->module = llvm::make_unique<Module>(moduleName, getGlobalContext());
        this->passManager = llvm::make_unique<legacy::FunctionPassManager>(module.get());
        
        // Set up the optimizer pipeline.
        this->passManager->add(createBasicAliasAnalysisPass());
        this->passManager->add(createInstructionCombiningPass());
        this->passManager->add(createReassociatePass());
        if (optimized) {
            this->passManager->add(createGVNPass());
            this->passManager->add(createCFGSimplificationPass());
            this->passManager->add(createPromoteMemoryToRegisterPass());
            this->passManager->add(createTailCallEliminationPass());
        }
        this->passManager->doInitialization();
        
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        
        this->jit = llvm::make_unique<LetterJIT>();
        this->module->setDataLayout(jit->getTargetMachine().createDataLayout());
        this->module->setTargetTriple(jit->getTargetMachine().getTargetTriple().getTriple());
        
        this->genBuiltins();
    };
    llvm::AllocaInst *lookupBinding(std::string name);
    void addBinding(std::string name, llvm::AllocaInst *inst);
    void addFunction(Function *function);
    void recordError(std::string error);
    llvm::Value *genExp(std::shared_ptr<Exp> exp);
    llvm::Value *genFunc(std::shared_ptr<UserFunc> func);
    llvm::Value *genMainFunc(std::vector<std::shared_ptr<Exp>> exps);
    llvm::AllocaInst *createEntryBlockAlloca(Function *f,
                                             const std::string &name);
    void createArgumentAllocas(std::vector<std::string> args, Function *f);
    int64_t execute();
private:
    std::unique_ptr<LetterJIT> jit;
    std::unique_ptr<legacy::FunctionPassManager> passManager;
    void genBuiltins();
    void printBindings();
    llvm::Value * genPrintf();
    llvm::Value *error(std::string message);
    llvm::Value *genDoFunc(FunCallExp exp, Function *parent);
    llvm::Value *genFunCallExp(FunCallExp exp);
    llvm::Value *i64Cast(llvm::Value *v);
};

#endif /* IRGenerator_hpp */
