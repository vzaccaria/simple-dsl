

First steps with Template Haskell
================

Template Haskell promises to be the next "big thing" for parallel code
generation. It allows to algorithmically generate and manipulate Haskell programs, much like LISP macros. With *quasi-quotation*, it even allows to define other languages whose syntax can be manipulated.

In this short blog post, I'll guide you through some very simple examples showing the power of Template Haskell.
Originally I struggled a lot in putting up a working example, so I hope this could be of help to anyone of you starting out on this. 

Pre-requirements
----------------

Before we start, you should have GHC (Haskell's compiler) and Cabal (Haskell's package manager) installed. This usually depends on your OS.

Then you should install `template-haskell` with Cabal:

	cabal install template-haskell




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

First of all, we are building an AST for a multiplication so the value returned is a quoted value. The dollar sign 
is the *splice* operator; `$x` allows to take the AST passed as parameter `x` and splice it into the bigger AST as the first operand 
of operator `*`. Splicing allows to evaluate at compile time expressions that will return an AST, so we can invoke
recursively `expand_power` to provide the representation of the sub-expression.


We can also define a function that returns a representation of an Haskell lambda:

> mk_power :: Int -> Q Exp
> mk_power n = [| \x -> $(expand_power n [| x |]) |]

The second quoted value `[| x |]` means that the AST to be passed to the `expand_power` function
should be the formal parameter `x` of the closure we are building. Scoping works just as we expect.

Using the function generator
----------------------------

To use `mk_power` and generate a program at *compile-time*, we have to 
write another module (due to the 'limitations' of the current implementation of template Haskell). This will be our `main.hs` program. It will use `mk_power` to instantiate a specialized power function:

	module Main where

	import Language.Haskell.TH.Syntax
	import Language.Haskell.TH.Ppr
	import Language.Haskell.TH
	import A

Then, we need a function to print the generated code (instead of executing it)[^2]:

	printCode :: Q Exp -> IO ()
	printCode ast = runQ ast >>= putStrLn . pprint

[^2]: Taken from https://www.cs.kent.ac.uk/people/rpg/cmb21/PTH.pdf

So that we can print the generated code:

	main = printCode(mk_power 3)

Most importantly, you can also execute that code in the same program. In fact, we can concretize `mk_power` with the `$` operator:

	power3 = $(mk_power 3)	

and use it as a normal power function. 


Building and running the program
----------------------------

If you build the program: 

	ghc --make -XTemplateHaskell main.hs -o main

And run it:

	./main

You get: 

	\x_0 -> x_0 GHC.Num.* (x_0 GHC.Num.* (x_0 GHC.Num.* 1))

which is the Haskell code for the generated function.

Conclusions
-----------

Template Haskell opens a whole new world for the creation of embedded DSLs. We can also write optimization passes that work on generated code. In this example, we could have optimized `(x_0 GHC.Num.* 1)` to `x_0` but much more complex DSLs could apply domain specific optimizations.















