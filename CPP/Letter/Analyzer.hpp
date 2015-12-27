//
//  Analyzer.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef Analyzer_hpp
#define Analyzer_hpp

#include <stdio.h>
#include <vector>
#include "Func.hpp"
#include "Env.hpp"

typedef enum Error {
    NoError = 0,
    ArityMismatch = 1
} Error;

class Analyzer {
public:
    Env env;
    Analyzer(Env env): env(env) {}
};

class FuncAnalyzer: public Analyzer {
public:
    std::vector<std::string> analyze(std::shared_ptr<Func> f);
    FuncAnalyzer(Env env): Analyzer(env) {}
private:
};

class ExpAnalyzer: public Analyzer {
public:
    std::vector<std::string> analyze(std::shared_ptr<Exp> exp);
    ExpAnalyzer(Env env): Analyzer(env) {}
private:
    bool ensureArityMatch(FunCallExp exp, std::string &reason);
    bool ensureCallExists(FunCallExp exp, std::string &reason);
};

#endif /* Analyzer_hpp */
