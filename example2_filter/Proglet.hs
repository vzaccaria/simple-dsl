{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import Utils (printCode, printAST)
import TopLevel (thePipe, simplify)
import TopLevel (filtA, filtB, filtC, optPipe, normPipe)

ffA:: [Rational] -> Rational 
ffA = \x -> $(filtA [| x |])

ffB:: [Rational] -> Rational 
ffB = \x -> $(filtB [| x |])

ffC:: [Rational] -> Rational 
ffC = \x -> $(filtC [| x |])

applyDSP:: ([Rational] -> Rational) -> Integer -> [Rational] -> [Rational]
applyDSP f _ [] = []
applyDSP f 0 _  = []
applyDSP f n (x:xs) = f(x:xs):(applyDSP f (n-1) xs)

unboxedOptPipe:: [Rational] -> Rational
unboxedOptPipe = \x -> $(simplify $ thePipe [| x |])

sumDiff [] _ = 0
sumDiff _ [] = 0
sumDiff (a:b) (x:y) = (a-x) + sumDiff b y

chainedFilters inputArray = 
    let intermediate = applyDSP ffA 90 inputArray 
        in let final = applyDSP ffB 80 intermediate 
            in applyDSP ffC 20 final 

main = do
    let ref = chainedFilters [1 .. 100] 
        in let opt = applyDSP unboxedOptPipe 20  [1 .. 100]     
            in print $ sumDiff ref opt


