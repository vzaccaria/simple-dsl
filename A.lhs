

Defining optimizable streaming computations
================

In this module, we will define a set of basic building blocks to define streaming computation. 
We will exploit Template Haskell to manipulate the program itself at compile time to introduce optimizations
that are expressed in Haskell itself:

> module A where
> 
> import GHC.Exts
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax

Initial examples
----------------

Let's define a function that computes the symbolic power of an expression 
whose value is known only when the program is run. The power exponent is, instead, known at compile-time [^1].

[^1]: Taken from 
**DSL Implementation in MetaOCaml, Template Haskell, and C++**, (Czarnecki1 et al.).

Our function receives thus an `int`, a **staged expression** `Q Exp` and returns another staged expression `Q Exp`.

> expand_power :: Int -> Q Exp -> Q Exp

A staged expression is just a representation of a portion of code. It is like an *abstract syntax tree*
that represents a computation to be performed at run-time. The above type means that the function
receives a representation of a symbol and returns an expression of that symbol, without evaluating it.

The function itself receives  two parameters (the exponent and the expression to be exponentiated):

> expand_power n x =

When `n` is 0, the function returns the AST for the literal `1`, by using Template Haskell quoting:

>     if n==0
>         then [| 1 |]

The above should be interpreted as: *if `n` is 0, then return an AST that when evaluated at run-time 
is 1.* If `n` is not 0, then we must build, with a (compile-time) recursion, the expression tree:

>         else [| $x * $(expand_power (n-1) x ) |]

First of all, we are building an AST for a multiplication so the value returned is a quoted value. The dollar sign 
is the *splice* operator; `$x` allows to take the AST passed as parameter `x` and splice it into the bigger AST as the first operand 
of operator `*`. Splicing allows to evaluate at compile time expressions that will return an AST, so we can invoke
recursively `expand_power` to provide the representation of the sub-expression.


We can also define a function that returns a representation of an Haskell lambda:

> mk_power :: Int -> Q Exp
> mk_power n = [| \x -> $(expand_power n [| x |]) |]

The second quoted value `[| x |]` means that the AST to be passed to the `expand_power` function
should be the formal parameter `x` of the closure we are building. Scoping works just as we expect.



> instance Lift Rational where
>   lift x = return (LitE (RationalL x))
> 
> dotv :: [Rational] -> Q Exp -> Int -> Int -> Q Exp 
> dotv c x n m = 
>     let coeff = head c
>         p = [| coeff * ($x !! m) |]
>         in  if n == 1
>             then p
>             else [| $p + $(A.dotv (tail c) x (n-1) (m+1)) |]
> 
> 
> 
> flt::[Rational] -> Q Exp
> flt c = [| \x -> $(dotv (reverse c) [| x |] (length c) 0) |]


