//
//  Env.hpp
//  Letter
//
//  Created by Harlan Haskins on 12/27/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

#ifndef Env_hpp
#define Env_hpp

#include <stdio.h>
#include <map>
#include "Func.hpp"

class Env {
    std::map<std::string, std::shared_ptr<Func>> funcs;
    std::map<std::string, std::shared_ptr<Exp>> globals;
public:
    void addFunc(std::shared_ptr<Func> func);
    void addGlobal(std::string name, std::shared_ptr<Exp> exp);
    std::shared_ptr<Exp> lookupGlobal(std::string name);
    std::shared_ptr<Func> lookupFunc(std::string name);
};

#endif /* Env_hpp */
