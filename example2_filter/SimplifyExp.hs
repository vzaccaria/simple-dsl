{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module SimplifyExp where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import Filt
import Utils (justZero, isPlus, isTimes, one, zero, isNum, 
        getNum, isDelayedSignal, isDelay, getDelayValue, getDelayedSignal,
        getNestedL, getNestedR, getNestedO, printAST, printCode, isPlusExp, 
        isTimesRightConstant, isTimesLeftConstant, justOne, getRightConstant, getLeftConstant) 


-- C O N S T A N T   P R O P A G A T I O N

symNum :: Rational -> Exp
symNum x = (LitE (RationalL x))

sumOf e1 e2 = (InfixE (Just e1) (VarE '(N.+)) (Just e2))
mulOf e1 e2 = (InfixE (Just e1) (VarE '(N.*)) (Just e2))


distributeConstantOnSum :: Exp -> Exp -> Exp
distributeConstantOnSum e1 c1 =  
	let a = getNestedL e1
		in let b = getNestedR e1
        	in sumOf (mulOf c1 a) (mulOf c1 b)

distributeConstantRight :: Exp -> Exp -> Exp 
distributeConstantRight e1 c1 = 
	let e = getNestedL e1
		in let c2 = getRightConstant e1
			in (mulOf (e) (symNum ((getNum c1) * (getNum c2))))

distributeConstantLeft :: Exp -> Exp -> Exp 
distributeConstantLeft e1 c1 = 
	let e = getNestedR e1
		in let c2 = getLeftConstant e1
			in (mulOf (e) (symNum ((getNum c1) * (getNum c2))))


constProp :: Exp -> Exp 

constProp (InfixE (Just e1) op (Just e2)) 
	| (justZero e1) && (isPlus op)                     = e2
	| (justZero e2) && (isPlus op)                     = e1
	| ((justZero e1) || (justZero e2)) && (isTimes op) = zero
	| ((justOne e1)) && (isTimes op)                   = e2
	| ((justOne e2)) && (isTimes op)                   = e1
	| otherwise                      = (InfixE (Just (constProp e1)) op (Just (constProp e2)))

constProp e = e

iterateConstProp:: Exp -> Exp
iterateConstProp = until (\x -> constProp x == x) constProp 

constantFolding :: Exp -> Exp 

constantFolding (InfixE (Just e1) op (Just e2)) 
	| isTimes(op) && isPlusExp(e1) && isNum(e2)        		= distributeConstantOnSum e1 e2  
	| isTimes(op) && isPlusExp(e2) && isNum(e1)        		= distributeConstantOnSum e2 e1  
	| isTimes(op) && isTimesRightConstant(e1) && isNum(e2) 	= distributeConstantRight e1 e2
	| isTimes(op) && isTimesRightConstant(e2) && isNum(e1) 	= distributeConstantRight e2 e1
	| isTimes(op) && isTimesLeftConstant(e1) && isNum(e2) 	= distributeConstantLeft e1 e2
	| isTimes(op) && isTimesLeftConstant(e2) && isNum(e1) 	= distributeConstantLeft e2 e1
	| otherwise = (InfixE (Just (constantFolding e1)) op (Just (constantFolding e2)))

constantFolding e = e

iterateConstantFolding:: Exp -> Exp 
iterateConstantFolding = until (\x -> constantFolding x == x) constantFolding

applyConstProp :: ExpQ -> ExpQ
applyConstProp eq = do
	e <- eq 
	return (iterateConstProp e)


