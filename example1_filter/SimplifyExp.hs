{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module SimplifyExp where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import Filt

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

getNum :: Exp -> Rational 
getNum ((LitE (RationalL (a)))) = toRational a
getNum ((LitE (IntegerL (a)))) = toRational a
getNum _ = 0


-- C O N S T A N T   P R O P A G A T I O N

symNum :: Rational -> Exp
symNum x = (LitE (RationalL x))



constProp :: Exp -> Exp 

constProp (InfixE (Just e1) op (Just e2)) 
	| (justZero e1) && (isPlus op)                     = e2
	| (justZero e2) && (isPlus op)                     = e1
	| ((justZero e1) || (justZero e2)) && (isTimes op) = zero
	| ((justOne e1)) && (isTimes op)                   = e2
	| ((justOne e2)) && (isTimes op)                   = e1
	| (isTimes op) && (isNum e1) && (isNum e2) 		   = (symNum ((getNum e1) * (getNum e2)))
	| (isPlus op) && (isNum e1) && (isNum e2) 		   = (symNum ((getNum e1) + (getNum e2)))
	| otherwise                      = (InfixE (Just (constProp e1)) op (Just (constProp e2)))

constProp e = e

iterateConstProp:: Exp -> Exp
iterateConstProp = until (\x -> constProp x == x) constProp 


applyConstProp :: ExpQ -> ExpQ
applyConstProp eq = do
	e <- eq 
	return (iterateConstProp e)

--main = do 
--	printAST $ applyConstProp [| 1 + 4 * 0 + 3 + 1 * 0 |]

