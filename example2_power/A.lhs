

Initial example
----------------

Let's define a module with a function that computes the symbolic power of an expression whose **value is known only when the program is run**. The power exponent is, instead, known at compile-time [^1].

[^1]: Taken from 
**DSL Implementation in MetaOCaml, Template Haskell, and C++**, (Czarnecki1 et al., 2003).

Let's call our module `A`. This module will import Template Haskell and not much else: 

> module A where
> 
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax

Our function `expand_power` receives an `int`, a **staged expression** `Q Exp` and returns another staged expression `Q Exp`.

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






> expand_power_anf :: Int -> Q Exp -> Q Exp
> expand_power_anf n x =
>     if n==0
>         then [| 1 |]
>         else [| let y = $x
>					 in let z = $(expand_power_anf (n-1) [| y |]) 
>					    in y * z |]


> expand_power_anf2 :: Int -> Q Exp -> Q Exp
> expand_power_anf2 n x =
>     if n==0
>         then [| 1 |]
>         else 
>				if (n `mod` 2) == 0
>					then [| let y = $x
>					 			in let z = $(expand_power_anf (quot n 2) [| y |]) 
> 									in z*z |]
> 					else [| let y = $x
>					 			in let z = $(expand_power_anf (n-1) [| y |]) 
>					    		in y * z |]






