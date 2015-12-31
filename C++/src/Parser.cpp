//
//  Parser.cpp
//  Letter
//
//  Created by Harlan Haskins on 12/24/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#include "Func.hpp"
#include "Parser.hpp"
#include <vector>
#include <iostream>

using namespace std;

enum Token {
    EofTok = -1,
    FunDefTok = -2,
    FunCallTok = -3,
    NumTok = -5,
    CommaTok = -6,
    IdentifierTok = -7,
};

char Parser::currentChar() {
    return currentTokenIndex >= input.size() ? EOF : input[currentTokenIndex];
}

bool isident(char c) {
    return isalnum(c) || c == '?' || c == '^' || c == '*' || c == '+' || c == '-' || c == '_' || c == '/' || c == '=' || c == '<' || c == '>';
}

SourceLoc Parser::currentLoc() {
    return (SourceLoc) { column, line };
}

void Parser::advance() {
    currentTokenIndex++;
    if (currentChar() == '\n' || currentChar() == '\r') {
        line++;
        column = 0;
    } else {
        column++;
    }
}

int Parser::gettok() {
    while (isspace(currentChar())) {
        advance();
    }
    currentExpLoc = currentLoc();
    if (isident(currentChar())) {
        string id;
        do {
            id += currentChar();
            advance();
        } while (isident(currentChar()));
        char *failure;
        auto num = strtol(id.c_str(), &failure, 10);
        if (*failure) {
            identifierValue = id;
            return IdentifierTok;
        } else {
            numericValue = num;
            return NumTok;
        }
    }
    
    if (currentChar() == '(') {
        identifierValue = "";
        advance();
        while (isident(currentChar())) {
            identifierValue += currentChar();
            advance();
        }
        
        if (identifierValue == "def") {
            return FunDefTok;
        } else {
            while (isident(currentChar())) {
                identifierValue += currentChar();
                advance();
            }
            return FunCallTok;
        }
    }
    
    if (currentChar() == ';') {
        do {
            advance();
        } while (currentChar() != EOF && currentChar() != '\n' && currentChar() != '\r');
        
        if (currentChar() != EOF) return gettok();
    }
    
    if (currentChar() == EOF) {
        return EofTok;
    }

    auto c = currentChar();
    advance();
    return c;
}

int Parser::seekToNextToken() {
    return this->currentToken = gettok();
}

string Parser::unconsumedInput() {
    return input.substr(currentTokenIndex, input.size() - currentTokenIndex);
}

void Parser::parseFile(vector<shared_ptr<SourceItem>> &exps, vector<shared_ptr<SourceItem>> &funcs) {
    while (currentToken != EofTok) {
        shared_ptr<SourceItem> exp;
        shared_ptr<SourceItem> func;
        parseLine(exp, func);
        if (exp) {
            exps.push_back(exp);
        } else if (func) {
            funcs.push_back(func);
        }
        seekToNextToken();
    }
}

shared_ptr<SourceItem> Parser::parseFunction() {
    auto loc = currentExpLoc;
    seekToNextToken();
    auto name = identifierValue;
    vector<string> args;
    seekToNextToken();
    while (currentToken != ')') {
        if (currentToken == EofTok)
            return error("Unexpected EOF in function definiton for " + name);
        args.push_back(identifierValue);
        seekToNextToken();
    }
    seekToNextToken(); // find subexpression.
    auto exp = parseExpression();
    if (!exp) return error("Invalid expression in function body for " + name);
    seekToNextToken();
    return make_shared<UserFunc>(name, args, move(exp), loc);
}

void Parser::parseLine(shared_ptr<SourceItem> &exp, shared_ptr<SourceItem> &func) {
    if (this->currentToken == FunDefTok) {
        exp = nullptr;
        func = parseFunction();
    } else {
        exp = parseExpression();
        func = nullptr;
    }
}

shared_ptr<SourceItem> Parser::parseExpression() {
    switch (this->currentToken) {
        case IdentifierTok:
            return parseVarLookup();
        case NumTok:
            return parseNumExp();
        case FunCallTok:
            return parseFunCall();
        case EofTok:
            return error("Unexpected EOF");
        default: break;
    }
    return nullptr;
}

shared_ptr<SourceItem> Parser::parseNumExp() {
    return make_shared<NumExp>(numericValue, currentExpLoc);
}

shared_ptr<SourceItem> Parser::parseVarLookup() {
    return make_shared<VarExp>(identifierValue, currentExpLoc);
}

shared_ptr<SourceItem> Parser::parseFunCall() {
    auto funcName = identifierValue;
    auto loc = currentExpLoc;
    vector<shared_ptr<SourceItem>> exps;
    seekToNextToken();
    while (currentToken != ')') {
        if (currentToken == EofTok)
            return error("Unexpected EOF found in function call \"" + funcName + "\"");
        auto exp = parseExpression();
        if (!exp) return nullptr;
        exps.push_back(move(exp));
        seekToNextToken();
    }
    return make_shared<FunCallExp>(funcName, exps, loc);
}

shared_ptr<SourceItem> Parser::error(string msg) {
    errors.push_back(msg);
    return nullptr;
}