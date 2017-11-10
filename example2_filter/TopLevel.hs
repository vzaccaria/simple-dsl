{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TopLevel where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L
import Filt
import SimplifyExp (iterateConstProp, iterateConstantFolding)

import Utils
        (justZero, isPlus, isTimes, one, zero, isNum,
        getNum, isDelayedSignal, isDelay, getDelayValue, getDelayedSignal,
        getNestedL, getNestedR, getNestedO, printAST, printCode)

import DSL
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
--myPipe = \x -> Filt.flt0 [1, 2] $ Filt.flt0 [3, 4] x

filtA:: ExpQ -> ExpQ
filtA  = Filt.flt0 [1, 0, 1, 3, 8]

filtB:: ExpQ -> ExpQ
filtB  = Filt.flt0 [2, -1, 0, 7, 4]

filtC:: ExpQ -> ExpQ
filtC  = Filt.flt0 [2, -1, 0, 7, 4]

thePipe = filtA . filtB . filtC

optPipe::ExpQ
optPipe = [| \x -> $(simplify $ thePipe [| x |]) |]

normPipe:: ExpQ
normPipe = [| \x -> $(thePipe [| x |]) |]

firstOpenCLPipe::ExpQ
firstOpenCLPipe = [| \x -> $(simplify $ Filt.flt0 [1, 2] $ Filt.flt0 [1, 2] [| x |]) |]
