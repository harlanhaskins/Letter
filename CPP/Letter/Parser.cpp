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
    LetTok = -4,
    NumTok = -5,
    CommaTok = -6,
    IdentifierTok = -7,
};

char Parser::currentChar() {
    return currentTokenIndex >= input.size() ? EOF : input[currentTokenIndex];
}

bool isident(char c) {
    return !isspace(c) && c != ')' && c != '(' && c != ';' && c != EOF;
}

int Parser::gettok() {
    while (isspace(currentChar())) {
        currentTokenIndex++;
    }
    if (isident(currentChar())) {
        string id;
        do {
            id += currentChar();
            currentTokenIndex++;
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
        currentTokenIndex++;
        while (isident(currentChar())) {
            identifierValue += currentChar();
            currentTokenIndex++;
        }
        
        if (identifierValue == "def") {
            return FunDefTok;
        } else if (identifierValue == "let") {
            return LetTok;
        } else {
            while (isident(currentChar())) {
                identifierValue += currentChar();
                currentTokenIndex++;
            }
            return FunCallTok;
        }
    }
    
    if (currentChar() == ';') {
        do {
            currentTokenIndex++;
        } while (currentChar() != EOF && currentChar() != '\n' && currentChar() != '\r');
        
        if (currentChar() != EOF) return gettok();
    }
    
    if (currentChar() == EOF) {
        return EofTok;
    }

    auto c = currentChar();
    currentTokenIndex++;
    return c;
}

int Parser::seekToNextToken() {
    return this->currentToken = gettok();
}

string Parser::unconsumedInput() {
    return input.substr(currentTokenIndex, input.size() - currentTokenIndex);
}

void Parser::parseFile(vector<shared_ptr<Exp>> &exps, vector<shared_ptr<UserFunc>> &funcs) {
    while (currentToken != EofTok) {
        shared_ptr<Exp> exp;
        shared_ptr<UserFunc> func;
        parseLine(exp, func);
        if (exp) {
            exps.push_back(exp);
        } else if (func) {
            funcs.push_back(func);
        }
        seekToNextToken();
    }
}

shared_ptr<UserFunc> Parser::parseFunction() {
    auto name = identifierValue;
    vector<string> args;
    seekToNextToken();
    while (currentToken != ')') {
        if (currentToken == EofTok)
            return errorFunc("Unexpected EOF in function definiton for " + name);
        args.push_back(identifierValue);
        seekToNextToken();
    }
    seekToNextToken(); // find subexpression.
    auto exp = parseExpression();
    if (!exp) return errorFunc("Invalid expression in function body for " + name);
    seekToNextToken();
    return make_shared<UserFunc>(name, args, move(exp));
}

void Parser::parseLine(shared_ptr<Exp> &exp, shared_ptr<UserFunc> &func) {
    if (this->currentToken == FunDefTok) {
        exp = nullptr;
        seekToNextToken();
        func = parseFunction();
    } else {
        exp = parseExpression();
        func = nullptr;
    }
}

shared_ptr<Exp> Parser::parseExpression() {
    switch (this->currentToken) {
        case IdentifierTok:
            return parseVarLookup();
        case NumTok:
            return parseNumExp();
        case FunCallTok:
            return parseFunCall();
        case LetTok:
            return parseLetExp();
        case EofTok:
            return error("Unexpected EOF");
        default: break;
    }
    return nullptr;
}

shared_ptr<Exp> Parser::parseNumExp() {
    return make_shared<NumExp>(numericValue);
}

shared_ptr<Exp> Parser::parseVarLookup() {
    return make_shared<VarExp>(identifierValue);
}

shared_ptr<Exp> Parser::parseLetExp() {
    seekToNextToken();
    auto name = identifierValue;
    seekToNextToken();
    auto exp = parseExpression();
    if (!exp) {
        return nullptr;
    }
    seekToNextToken();
    if (currentToken != ')') {
        return error("Expected ')' in 'let' expression.");
    }
    return make_shared<LetExp>(name, move(exp));
}

shared_ptr<Exp> Parser::parseFunCall() {
    auto funcName = identifierValue;
    vector<shared_ptr<Exp>> exps;
    seekToNextToken();
    while (currentToken != ')') {
        if (currentToken == EofTok)
            return error("Unexpected EOF found in function call \"" + funcName + "\"");
        auto exp = parseExpression();
        if (!exp) return nullptr;
        exps.push_back(move(exp));
        seekToNextToken();
    }
    return make_shared<FunCallExp>(funcName, move(exps));
}

shared_ptr<Exp> Parser::error(string msg) {
    cerr << msg << endl;
    return nullptr;
}

shared_ptr<UserFunc> Parser::errorFunc(string msg) {
    error(msg);
    return nullptr;
}