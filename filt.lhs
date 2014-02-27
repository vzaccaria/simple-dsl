
Let's define the module Filt, which will provide basic 
DSP filtering functions.

> module Filt where
>
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax


We need to make it possible to Lift rationals into the Q monad:

> instance Lift Rational where
>   lift x = return (LitE (RationalL x))

Dot product:

> dotv :: [Rational] -> Q Exp -> Int -> Int -> Q Exp 
> dotv c x n m = 
>     let coeff = head c
>         p = [| coeff * ($x !! m) |]
>         in  if n == 1
>             then p
>             else [| $p + $(Filt.dotv (tail c) x (n-1) (m+1)) |]
> 
> 
> flt0 :: [Rational] -> ( Q Exp -> Q Exp )
> flt0 c = \x -> dotv (reverse c) x (length c) 0
> 
>
> flt1 :: [Rational] -> [Rational] -> (Q Exp -> Q Exp)
> flt1 c1 c2 = \x ->
>					let e1 = (flt0 c1)(x)
>					    e2 = (flt0 c2)(e1)
>					    in e2



flt::[Rational] -> Q Exp
flt c = [| \x -> $(dotv (reverse c) [| x |] (length c) 0) |]


