{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module FiltExamples where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L
import FiltFIR
import ExpSimplify (iterateConstProp, iterateConstantFolding)

import ExpSimplifyUtils 
        (justZero, isPlus, isTimes, one, zero, isNum, 
        getNum, isDelayedSignal, isDelay, getDelayValue, getDelayedSignal,
        getNestedL, getNestedR, getNestedO, printAST, printCode) 

import ExpSimplifyDSL 
        (iterateDelayOpt, iterateDistOpt)

import System.Random
import Data.List


-- MAIN OPTIMIZATION FUNCTIONS

simplifyPass:: Exp -> Exp 
simplifyPass eq = do
    iterateConstantFolding $ iterateConstProp $ iterateDelayOpt $ iterateDistOpt eq 
    

simplifyAll = until (\x -> simplifyPass x == x) simplifyPass 

simplify :: ExpQ -> ExpQ
simplify eq = do
    e <- eq 
    return (simplifyAll e)

-- EXAMPLE PIPE

--myPipe:: ExpQ -> ExpQ 
--myPipe = \x -> FiltFIR.flt0 [1, 2] $ FiltFIR.flt0 [3, 4] x 

filtA:: ExpQ -> ExpQ 
filtA x = FiltFIR.flt0 [1, 0, 1, 3, 8] x

filtB:: ExpQ -> ExpQ 
filtB x = FiltFIR.flt0 [2, -1, 0, 7, 4] x

filtC:: ExpQ -> ExpQ 
filtC x = FiltFIR.flt0 [2, -1, 0, 7, 4] x

thePipe = \x -> filtA $ filtB $ filtC x

optPipe::ExpQ
optPipe = [| \x -> $(simplify $ thePipe [| x |]) |]

normPipe:: ExpQ 
normPipe = [| \x -> $(thePipe [| x |]) |]

firstOpenCLPipe::ExpQ 
firstOpenCLPipe = [| \x -> $(simplify $ FiltFIR.flt0 [1, 2] $ FiltFIR.flt0 [1, 2] [| x |]) |]




