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
    int line = 1;
    int column = 0;
public:
    Parser(std::string input): input(input) {
        seekToNextToken();
    }
    std::shared_ptr<Exp> parseExpression();
    std::shared_ptr<UserFunc> parseFunction();
    std::vector<std::string> errors;
    void parseLine(std::shared_ptr<Exp> &exp, std::shared_ptr<UserFunc> &func);
    void parseFile(std::vector<std::shared_ptr<Exp>> &exps, std::vector<std::shared_ptr<UserFunc>> &funcs);
private:
    int gettok();
    int seekToNextToken();
    char currentChar();
    SourceLoc currentLoc();
    void advance();
    std::string unconsumedInput();
    std::shared_ptr<Exp> parseNumExp();
    std::shared_ptr<Exp> parseVarLookup();
    std::shared_ptr<Exp> parseFunCall();
    std::shared_ptr<Exp> parseLetExp();
    std::shared_ptr<Exp> error(std::string msg);
    std::shared_ptr<UserFunc> errorFunc(std::string msg);
};

#endif /* Parser_hpp */
