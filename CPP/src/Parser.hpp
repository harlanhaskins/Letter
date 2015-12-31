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
    std::shared_ptr<SourceItem> parseExpression();
    std::shared_ptr<SourceItem> parseFunction();
    std::vector<std::string> errors;
    void parseLine(std::shared_ptr<SourceItem> &exp, std::shared_ptr<SourceItem> &func);
    void parseFile(std::vector<std::shared_ptr<SourceItem>> &exps, std::vector<std::shared_ptr<SourceItem>> &funcs);
private:
    int gettok();
    int seekToNextToken();
    char currentChar();
    SourceLoc currentLoc();
    SourceLoc currentExpLoc;
    void advance();
    std::string unconsumedInput();
    std::shared_ptr<SourceItem> parseNumExp();
    std::shared_ptr<SourceItem> parseVarLookup();
    std::shared_ptr<SourceItem> parseFunCall();
    std::shared_ptr<SourceItem> parseLetExp();
    std::shared_ptr<SourceItem> error(std::string msg);
};

#endif /* Parser_hpp */
