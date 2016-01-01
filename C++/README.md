# Letter C++

A simple parser, static analyzer, and LLVM Code Generator for Letter in C++.

# Building

## OS X (with Xcode)

To build this, you'll need LLVM 3.7, installed via Homebrew. I'm going to put this in a more permanent place,
but for now, all you need to do is

```
brew install llvm37
```

And the provided Xcode project will link properly.

## Non-Xcode install (make)

If you're not on OS X, you'll need llvm-3.7 symlinked to `llvm-config` in your `$PATH`.

Just run

```
make
```

# Running

The program will, by default, JIT and execute the entire file you provide it.

## Arguments

* `-emit-ast`:
  * Emit the AST of the file to stdout
* `-emit-llvm`:
  * Emit the LLVM IR to stdout (useful for debugging)
* `-O[none, 1, 2, 3]`
  * Optimize the code prior to execution or printing.
    * `none`: Perform no optimizations
    * `1`: Perform trivial optimizations
    * `2`: Perform slightly more advanced optimizations
    * `3`: Perform ridiculous optimizations like function inilining all over
