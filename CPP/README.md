# Letter C++

A simple parser, static analyzer, and LLVM Code Generator for Letter in C++.

# Building

To build this, you'll need LLVM 3.7, installed via Homebrew. I'm going to put this in a more permanent place,
but for now, all you need to do is

```
brew install llvm37
```

And the provided Xcode project will link properly.

# Running

The program will, by default, JIT and execute the entire file you provide it.

## Arguments

* -emit-ast:
  * Emit the AST of the file to stdout
* -emit-llvm:
  * Emit the LLVM IR to stdout (useful for debugging)
* -O
  * Optimize the code prior to execution or printing.
