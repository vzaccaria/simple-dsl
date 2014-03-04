{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L
import Filt
import SimplifyExp (iterateConstProp, justZero, zero) 

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

delayOpt :: Exp -> Exp 
delayOpt (InfixE (Just e1) op (Just e2)) 
    | isDelay(op) && isDelayedSignal(e1) && isNum(e2)   = fuseDelays e1 op e2
    | otherwise = (InfixE (Just (delayOpt e1)) op (Just (delayOpt e2)))

delayOpt e = e



-- MAIN OPTIMIZATION FUNCTIONS

simplifyPass:: Exp -> Exp 
simplifyPass eq = do 
    delayOpt $ iterateConstProp eq


simplifyAll = until (\x -> simplifyPass x == x) simplifyPass 

simplify :: ExpQ -> ExpQ
simplify eq = do
    e <- eq 
    return (simplifyAll e)

-- EXAMPLE PIPE

myPipe:: ExpQ -> ExpQ 
myPipe x = 
    Filt.flt0 [1, 1] 
    $ Filt.flt0 [0, 1] x

wrappedPipe::ExpQ
--wrappedPipe = [| \x -> $(myPipe [| x |]) |]
wrappedPipe = [| \x -> $(simplify $ myPipe [| x |]) |]


main = do
    printAST $ wrappedPipe 
    printCode $ wrappedPipe

-- ( 2 * ( 3 * x_0)) + ( (3 * x_0) GHC.List.!! 1)
