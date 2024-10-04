# HW04
## Soft deadline: 23:59 08.10.2024
## Hard Deadline: 23:59 10.10.2024

1. (1 point) Replace the project in [expr](expr/) with a stack project containing your solution of HW03. 
   * Make sure to split your code in modules. For example, it makes sense to separate the code related to errors from the definitions of expressions, while the interpreter should be placed in the third module. 
2. (1 points) Add variables and let-expressions to your expression data types. 
   * A variable is represented by a string. 
   * A let-expression should mimic a let-expression in Haskell: it should be possible to express something like `let x = 13 in let y = x + 1 in y ** 2`. 
3. (3 points) Update your interpreter to support variables.
   * Use maps from the [containers](https://hackage.haskell.org/package/containers) package
   * Make the substitution contain the result of the evaluation of the expression bound to the variable: `eval :: Map String Double -> Expr -> Either Error Double`
4. (3 points) Replace your handwritten test suite with a `tasty` test suite. Ensure good coverage.

If you start working on this assignment before we've finished checking your HW03 assignment, you can switch to a project in HW03 branch. This way you won't need to synchronize between branches. No need to create HW04 and open PR in this case either. 
