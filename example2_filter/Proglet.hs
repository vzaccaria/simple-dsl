{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import Utils (printCode, printAST)
import TopLevel (myPipe, simplify)
--import TopLevel (filtA, filtB, filtC, thePipe)
--executeThis :: Fractional a => [a] -> a
--executeThis v = $(normPipe [| v |])

unboxedNormPipe:: [Rational] -> Rational
unboxedNormPipe = \x -> $(simplify $ myPipe [| x |])

--unboxedOptPipe:: [Rational] -> Rational
--unboxedOptPipe = \x -> $(simplify $ thePipe [| x |])

main = do
    let inputArray = [1 .. 1000] 
        in let ref = unboxedNormPipe inputArray 
            in print ref

