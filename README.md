# Letter

Letter is a simple LISP-like programming language that supports functions,
dynamic scope, and local variables.

There are no types; Letter only has machine integers.

```scheme
(def fact (n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))
    )
)

(fact 8)
```

## Statements

* `def`
    * Adds a function into the global function environment.

* `do`
    * Opens a new scope. Allows sequencing multiple expressions.
    * Always evaluates to the result of the last expression inside it.

* `if`
    * Evaluates the first argument. If it's 1, evaluates and yields the
      2nd argument. Otherwise, evaluates and yields the 3rd argument.

* `print`
    * Prints the argument to the console and yields its evaluated result.

* `+` / `-` / `\*` / `/` / `mod` / `=` / `>` / `\<` / `\>` / `>=` / `\<=`
    * Are provided in the initial basis.

All expressions reduce to an Integer.
