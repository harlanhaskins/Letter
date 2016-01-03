//
//  Driver.hpp
//  Letter
//
//  Created by Harlan Haskins on 1/2/16.
//  Copyright © 2016 Harlan Haskins. All rights reserved.
//

#ifndef Driver_hpp
#define Driver_hpp

#include <stdio.h>
#include "IRGenerator.hpp"

#endif /* Driver_hpp */

typedef struct Options {
    const char *filename;
    const char *input;
    OptimizationLevel optimizationLevel;
    bool emitAST;
    bool emitIR;
} Options;

class Driver {
public:
    static bool run(Options o);
};