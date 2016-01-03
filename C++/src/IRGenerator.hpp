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
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/InlinerPass.h"
#include "LetterJIT.h"

typedef enum OptimizationLevel {
    none = 0, O1, O2, O3
} OptimizationLevel;

class IRGenerator {
public:
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;
    std::map<std::string, std::shared_ptr<BuiltinFunc>> builtins;
    std::map<std::string, llvm::AllocaInst*> namedValues;
    OptimizationLevel optimizationLevel;
    std::vector<std::string> errors;
    IRGenerator(std::string moduleName, OptimizationLevel optimizationLevel): optimizationLevel(optimizationLevel), builder(getGlobalContext()) {
        
        this->module = llvm::make_unique<Module>(moduleName, getGlobalContext());
        this->passManager = llvm::make_unique<legacy::FunctionPassManager>(module.get());
        this->modulePassManager = llvm::make_unique<legacy::PassManager>();
        
        createOptimizationPipeline();
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
    void recordError(std::string error, SourceItem &exp);
    llvm::Value *genExp(std::shared_ptr<SourceItem> exp);
    llvm::Value *genFunc(std::shared_ptr<UserFunc> func);
    llvm::Value *genMainFunc(std::vector<std::shared_ptr<SourceItem>> exps);
    llvm::AllocaInst *createEntryBlockAlloca(Function *f,
                                             const std::string &name);
    void createArgumentAllocas(std::vector<std::string> args, Function *f);
    int64_t execute();
    void finish();
private:
    std::unique_ptr<LetterJIT> jit;
    std::unique_ptr<legacy::FunctionPassManager> passManager;
    std::unique_ptr<legacy::PassManager> modulePassManager;
    void genBuiltins();
    void printBindings();
    void genPrint(bool newline, llvm::Value *printf);
    void createOptimizationPipeline();
    llvm::Value *genPrintf();
    llvm::Value *error(std::string message);
    llvm::Value *genDoFunc(FunCallExp exp, Function *parent);
    llvm::Value *genFunCallExp(FunCallExp exp);
    llvm::Value *i64Cast(llvm::Value *v);
};

#endif /* IRGenerator_hpp */
