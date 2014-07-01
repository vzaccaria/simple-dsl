{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExpSimplifyUtils where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import GHC.List as L

printAST :: ExpQ -> IO ()
printAST  ast = runQ ast >>= putStrLn . show

printCode :: Q Exp -> IO ()
printCode ast = runQ ast >>= putStrLn . pprint

justZero :: Exp -> Bool
justZero ((LitE (RationalL (0)))) = True 
justZero ((LitE (IntegerL (0)))) = True
justZero _ = False	

justOne :: Exp -> Bool
justOne ((LitE (RationalL (1)))) = True 
justOne ((LitE (IntegerL (1)))) = True
justOne _ = False	

zero :: Exp
zero = (LitE (RationalL (0)))

one :: Exp 
one = (LitE (RationalL (1)))

-- From: http://stackoverflow.com/questions/12075530/how-can-haskell-quasiquotation-be-used-for-replacing-tokens-on-the-haskell-level

isPlus :: Exp -> Bool 
isPlus op 
	| op == (VarE '(N.+)) = True 
	| otherwise 		  = False

isTimes:: Exp -> Bool 
isTimes op 
	| op == (VarE '(N.*)) = True 
	| otherwise 		  = False

isNum :: Exp -> Bool
isNum ((LitE (RationalL (a)))) = True 
isNum ((LitE (IntegerL (a)))) = True
isNum _ = False 

isInteger :: Exp -> Bool 
isInteger ((LitE (IntegerL (a)))) = True
isInteger _ = False

getNum :: Exp -> Rational 
getNum ((LitE (RationalL (a)))) = toRational a
getNum ((LitE (IntegerL (a)))) = toRational a
getNum _ = 0

getInteger :: Exp -> Integer
getInteger ((LitE (IntegerL (a)))) = toInteger a
getInteger _ = 0



isDelay :: Exp -> Bool 
isDelay op 
    | op == (VarE '(L.!!)) = True 
    | otherwise           = False

isDelayedSignal:: Exp -> Bool
isDelayedSignal (InfixE (Just e1) op (Just e2))
    | isDelay(op) && isInteger(e2) = True
    | otherwise = False
isDelayedSignal e = False

isExp:: Exp -> Bool 
isExp (InfixE (Just e1) op (Just e2)) = True
isExp e = isNum e 

getNestedL (InfixE (Just e1) op (Just e2)) = e1
getNestedR (InfixE (Just e1) op (Just e2)) = e2
getNestedO (InfixE (Just e1) op (Just e2)) = op

getRightConstant = getNestedR
getLeftConstant = getNestedL

getDelayValue:: Exp -> Integer
getDelayValue (InfixE (Just e1) op (Just e2))
    | isDelay(op) = getInteger(e2)
    | otherwise = 0

getDelayedSignal:: Exp -> Exp
getDelayedSignal (InfixE (Just e1) op (Just e2))
    | isDelay(op) = e1 
    | otherwise = (InfixE (Just e1) op (Just e2)) 


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


