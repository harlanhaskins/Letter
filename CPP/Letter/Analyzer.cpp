//
//  Analyzer.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Analyzer.hpp"

std::vector<std::string> ExpAnalyzer::analyze(std::shared_ptr<Exp> exp) {
    std::vector<std::string> reasons;
    auto funCallExp = dynamic_cast<FunCallExp *>(&*exp);
    if (funCallExp) {
        std::string reason;
        if (!ensureCallExists(*funCallExp, reason)) {
            reasons.push_back(reason);
            return reasons;
        }
        if (!ensureArityMatch(*funCallExp, reason)) {
            reasons.push_back(reason);
            return reasons;
        }
    }
    return reasons;
}

bool ExpAnalyzer::ensureCallExists(FunCallExp exp, std::string &reason) {
    auto func = env.lookupFunc(exp.func);
    if (!func) {
        reason = "Could not find function named \"" + exp.func + "\".";
        return false;
    }
    return true;
}

bool ExpAnalyzer::ensureArityMatch(FunCallExp exp, std::string &reason) {
    auto func = env.lookupFunc(exp.func);
    if (func->arity() != INFINITE_ARITY && func->arity() != exp.args.size()) {
        reason = "Function \"" + exp.func + "\" expects " + std::to_string(func->arity()) + " arguments, but " + std::to_string(exp.args.size()) + " provided.";
        return false;
    }
    return true;
}

std::vector<std::string> FuncAnalyzer::analyze(std::shared_ptr<Func> f) {
    ExpAnalyzer analyzer(this->env);
    UserFunc *userFunc = dynamic_cast<UserFunc *>(&*f);
    if (userFunc) {
        return analyzer.analyze(userFunc->body);
    }
    return std::vector<std::string>();
}