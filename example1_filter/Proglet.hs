{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L
import Filt
import SimplifyExp (iterateConstProp, justZero, isPlus, isTimes, one, zero) 

printAST :: ExpQ -> IO ()
printAST  ast = runQ ast >>= putStrLn . show

printCode :: Q Exp -> IO ()
printCode ast = runQ ast >>= putStrLn . pprint

-- DOMAIN SPECIFIC OPTIMIZATION

isNum :: Exp -> Bool
isNum ((LitE (RationalL (a)))) = True 
isNum ((LitE (IntegerL (a)))) = True
isNum _ = False 

getNum :: Exp -> Rational 
getNum ((LitE (RationalL (a)))) = toRational a
getNum ((LitE (IntegerL (a)))) = toRational a
getNum _ = 0

isDelay :: Exp -> Bool 
isDelay op 
    | op == (VarE '(L.!!)) = True 
    | otherwise           = False

isDelayedSignal:: Exp -> Bool
isDelayedSignal (InfixE (Just e1) op (Just e2))
    | isDelay(op) && isNum(e2) = True
    | otherwise = False

isDelayedSignal e = False

getNestedL (InfixE (Just e1) op (Just e2)) = e1
getNestedR (InfixE (Just e1) op (Just e2)) = e2
getNestedO (InfixE (Just e1) op (Just e2)) = op


getDelayValue:: Exp -> Rational
getDelayValue (InfixE (Just e1) op (Just e2))
    | isDelay(op) = getNum(e2)
    | otherwise = 0

getDelayedSignal:: Exp -> Exp
getDelayedSignal (InfixE (Just e1) op (Just e2))
    | isDelay(op) = e1 
    | otherwise = (InfixE (Just e1) op (Just e2)) 

fuseDelays:: Exp -> Exp -> Exp -> Exp 
fuseDelays e1 op e2 = 
            let d = getDelayValue e1
                s = getDelayedSignal e1
                c = getNum e2
                in (InfixE (Just s) op (Just (LitE (RationalL (d + c)))))

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

isPlusExp:: Exp -> Bool 
isPlusExp (InfixE (Just e1) op (Just e2)) 
    | isPlus(op) = True
    | otherwise = False 
isPlusExp e = False

isTimesRightConstant:: Exp -> Bool 
isTimesRightConstant (InfixE (Just e1) op (Just e2)) 
    | isTimes(op) && isNum(e2)  = True
    | otherwise                 = False 
isTimesRightConstant e = False

isTimesLeftConstant:: Exp -> Bool 
isTimesLeftConstant (InfixE (Just e1) op (Just e2)) 
    | isTimes(op) && isNum(e1)  = True
    | otherwise                 = False 
isTimesLeftConstant e = False


delayOpt :: Exp -> Exp 
delayOpt (InfixE (Just e1) op (Just e2)) 
    | isDelay(op) && isDelayedSignal(e1) && isNum(e2)       = fuseDelays e1 op e2
    | isDelay(op) && justZero(e2)                           = e1
    | isDelay(op) && isNum(e1) && isNum(e2)                 = e1
    | otherwise = (InfixE (Just (delayOpt e1)) op (Just (delayOpt e2)))

delayOpt e = e

iterateDelayOpt :: Exp -> Exp 
iterateDelayOpt = until (\x -> delayOpt x == x) delayOpt

distOpt :: Exp -> Exp 
distOpt (InfixE (Just e1) op (Just e2)) 
    | isDelay(op) && isPlusExp(e1) && isNum(e2)               = distributeDelay e1 e2  
    | isDelay(op) && isTimesRightConstant(e1) && isNum(e2)    = distributeDelayLeft e1 e2
    | isDelay(op) && isTimesLeftConstant(e1) && isNum(e2)     = distributeDelayRight e1 e2
    | otherwise = (InfixE (Just (distOpt e1)) op (Just (distOpt e2)))
distOpt e = e

iterateDistOpt :: Exp -> Exp 
iterateDistOpt = until (\x -> distOpt x == x) distOpt 

-- MAIN OPTIMIZATION FUNCTIONS

simplifyPass:: Exp -> Exp 
simplifyPass eq = do
    iterateConstProp $ iterateDelayOpt $ iterateDistOpt eq 
    
--    delayOpt $ iterateConstProp eq

      --iterateConstProp eq
--Uncomment this when you have finished to write the post.
--do 



simplifyAll = until (\x -> simplifyPass x == x) simplifyPass 

simplify :: ExpQ -> ExpQ
simplify eq = do
    e <- eq 
    return (simplifyAll e)
    --return (iterateConstProp e)

-- EXAMPLE PIPE

myPipe:: ExpQ -> ExpQ 
myPipe x = 
    Filt.flt0 [0, 1] 
    $ Filt.flt0 [0, 1] x

--myPipe x = 
--    Filt.flt0 [0, 1] x

wrappedPipe::ExpQ
--wrappedPipe = [| \x -> $(myPipe [| x |]) |]
--wrappedPipe = [| \x -> $(simplify $ myPipe [| x |]) |]
--wrappedPipe = [| \x -> $(simplify $ myPipe [| x |]) |]
wrappedPipe = [| \x -> $(simplify $ Filt.flt0 [1, 2] $ Filt.flt0 [2, 1]$ Filt.flt0 [4, 1]$ Filt.flt0 [8, 1]$ Filt.flt0 [9, 1] $[| x |]) |]

--myfilter = [| \x -> $(Filt.flt0 [0, 1] [| x |]) |]
--myfilter2 = [| \x -> $(Filt.flt0 [2, 3] $ Filt.flt0 [0, 1] [| x |]) |]

main = printCode $ wrappedPipe 
    --do
    --printAST $ wrappedPipe 
    --printCode $ wrappedPipe

--main = printCode myfilter2

---- ( 2 * ( 3 * x_0)) + ( (3 * x_0) GHC.List.!! 1)
