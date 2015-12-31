# Letter

Letter is a simple LISP-like programming language that supports functions,
dynamic scope, and local variables.

There are no types; Letter only has integers.

There are two directories correspoding to 2 implementations of Letter.

The C++ directory has an LLVM IR-generating JIT execution environment.

The Haskell directory has an interpreter and REPL.

```scheme
(def fact (n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(fact 8)
```

## Statements

* `def`
    * Adds a function into the global function environment.
    * Syntax: `(def fun (arg1 arg2) (* arg1 arg2))`

* `do`
    * Opens a new scope. Allows sequencing multiple expressions.
    * Always evaluates to the result of the last expression inside it.
    * Syntax: `(do (print 3) (print 5) 5)` (evaluates to `5`).

* `let`
    * Adds a variable binding to the current scope.
    * If it already exists, the value is mutated.
    * Syntax: `(let var 4)` (sets `var` to `4`)

* `if`
    * Evaluates the first argument. If it's 1, evaluates and yields the
      2nd argument. Otherwise, evaluates and yields the 3rd argument.
    * Syntax: `(if (= x 0) 3 4)` (evaluates to 3 if x is 0, otherwise 4

* `print`
    * Prints the argument to the console and yields its evaluated result.
    * Syntax: `(print x)` (prints the value in x to stdout, plus a newline)

* `not`
    * Returns 1 if the argument is zero, and otherwise, returns 0.
    * Syntax: `(not x)`

* `while`
    * Checks the first argument, and if it's true, runs the second argument. Always returns zero.
    * Syntax: `(while (> 0 x) (do (print x) (let x (- x 1))))` (will print all values from `x` to `0`)

* `+` / `-` / `*` / `/` / `mod` / `=` / `>` / `<` / `>` / `>=` / `<=`
    * Are provided in the initial basis. They do what you expect.

All expressions reduce to a 64-bit integer in LLVM, or an arbitrary sized integer in the interpreter.
