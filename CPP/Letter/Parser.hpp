//
//  Parser.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef Parser_hpp
#define Parser_hpp

#include <stdio.h>
#include "Exp.hpp"
#include "Func.hpp"

class Parser {
    int currentTokenIndex = 0;
    int currentToken;
    std::string input;
    long numericValue = 0;
    std::string identifierValue = "";
public:
    Parser(std::string input): input(input) {}
    std::unique_ptr<Exp> parseExpression();
    std::unique_ptr<Func> parseFunction();
    void parseLine(std::unique_ptr<Exp> &exp, std::unique_ptr<Func> &func);
    void parseFile(std::vector<std::unique_ptr<Exp>> &exps, std::vector<std::unique_ptr<Func>> &funcs);
private:
    int gettok();
    int seekToNextToken();
    char currentChar();
    std::string unconsumedInput();
    std::unique_ptr<Exp> parseNumExp();
    std::unique_ptr<Exp> parseVarLookup();
    std::unique_ptr<Exp> parseFunCall();
    std::unique_ptr<Exp> parseLetExp();
    std::unique_ptr<Exp> error(std::string msg);
    std::unique_ptr<Func> errorFunc(std::string msg);
};

#endif /* Parser_hpp */
