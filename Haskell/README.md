# Letter Haskell

An interpreter, REPL, and static analyzer for Letter.

## Building

This project is managed through `stack`, so you can build `letter` with

```
stack build
```

And you can install it to `$HOME/.local/bin` with

```
stack install
```

The executable accepts the following arguments:

```
letter [FILE]:
* -t | --target
  * Allows you to specify a compile target for Letter code. The only supported argument is `c`.
* -d | --dump
  * Dumps the AST to the screen, including function bindings.
```

If no file is provided, the REPL will open.
