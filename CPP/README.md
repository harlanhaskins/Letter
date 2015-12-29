# Letter C++

A simple parser, static analyzer, and LLVM Code Generator for Letter in C++.

# Building

To build this, you'll need LLVM 3.7, installed via Homebrew. I'm going to put this in a more permanent place,
but for now, all you need to do is

```
brew install llvm37
```

And the provided Xcode project will link properly.

The program expects one argument, the filename to compile.
It'll print the AST for the whole file, and all the LLVM IR.
