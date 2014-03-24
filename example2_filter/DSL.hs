module DSL where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L

import Utils 
        (justZero, isPlus, isTimes, one, zero, isNum, 
        getNum, isDelayedSignal, isDelay, getDelayValue, getDelayedSignal,
        getNestedL, getNestedR, getNestedO, printAST, printCode, isPlusExp, 
        isTimesRightConstant, isTimesLeftConstant, isExp, isInteger, getInteger) 


fuseDelays:: Exp -> Exp -> Exp -> Exp 
fuseDelays e1 op e2 = 
            let d = getDelayValue e1
                s = getDelayedSignal e1
                c = getInteger e2
                in (InfixE (Just s) op (Just (LitE (IntegerL (d + c)))))

delayExp:: Exp -> Exp -> Exp
delayExp x n = (InfixE (Just x) (VarE '(L.!!)) (Just n))

distributeDelay:: Exp -> Exp  -> Exp 
distributeDelay e1 e2 =
    let l = getNestedL e1
        r = getNestedR e1
        o = getNestedO e1
        in (InfixE (Just (delayExp l e2)) o (Just (delayExp r e2)))

distributeDelayLeft:: Exp -> Exp  -> Exp 
distributeDelayLeft e1 e2 =
    let l = getNestedL e1
        r = getNestedR e1
        o = getNestedO e1
        in (InfixE (Just (delayExp l e2)) o (Just r))

distributeDelayRight:: Exp -> Exp  -> Exp 
distributeDelayRight e1 e2 =
    let l = getNestedL e1
        r = getNestedR e1
        o = getNestedO e1
        in (InfixE (Just l) o (Just (delayExp r e2)))


delayOpt :: Exp -> Exp 
delayOpt (InfixE (Just e1) op (Just e2)) 
    | isDelay(op) && isDelayedSignal(e1) && isInteger(e2)   = fuseDelays e1 op e2  -- (x@a)@b = x@(a+b)
    | isDelay(op) && justZero(e2) && (isExp e1) 			= e1				   -- (...)@0 = (...) only if not free variable
    | isDelay(op) && isNum(e1) && isInteger(e2)             = e1				   -- (1@n)   = 1
    | otherwise = (InfixE (Just (delayOpt e1)) op (Just (delayOpt e2)))

delayOpt e = e

iterateDelayOpt :: Exp -> Exp 
iterateDelayOpt = until (\x -> delayOpt x == x) delayOpt

distOpt :: Exp -> Exp 
distOpt (InfixE (Just e1) op (Just e2)) 
    | isDelay(op) && isPlusExp(e1) && isInteger(e2)           	  = distributeDelay e1 e2  		-- (a + b) @ c = (a@c + b@c)
    | isDelay(op) && isTimesRightConstant(e1) && isInteger(e2)    = distributeDelayLeft e1 e2	-- (a * c) @ d = (a@d * c)
    | isDelay(op) && isTimesLeftConstant(e1) && isInteger(e2)     = distributeDelayRight e1 e2  -- (c * a) @ d = (c   * a@d)
    | otherwise = (InfixE (Just (distOpt e1)) op (Just (distOpt e2)))
distOpt e = e

iterateDistOpt :: Exp -> Exp 
iterateDistOpt = until (\x -> distOpt x == x) distOpt 