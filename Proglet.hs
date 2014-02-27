{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import GHC.Num as N
import Filt

printAST :: ExpQ -> IO ()
printAST  ast = runQ ast >>= putStrLn . show

printCode :: Q Exp -> IO ()
printCode ast = runQ ast >>= putStrLn . pprint

--main = printCode (mk_power 3)

--main = printCode (mk_power 3)

--main = printAST $ Filt.flt0 [0] [| 'x' |]

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



--constProp (InfixE (Just e1) (*) (Just e2)) 
--	| ((justZero e1) || (justZero e2)) = zero
--	| ((justOne e1))                   = e2
--	| ((justOne e2))                   = e1
--	| otherwise                        = (InfixE (Just (constProp e1)) (*) (Just (constProp e2)))


-- From: http://stackoverflow.com/questions/12075530/how-can-haskell-quasiquotation-be-used-for-replacing-tokens-on-the-haskell-level

isPlus :: Exp -> Bool 
isPlus op 
	| op == (VarE '(N.+)) = True 
	| otherwise 		  = False


isTimes:: Exp -> Bool 
isTimes op 
	| op == (VarE '(N.*)) = True 
	| otherwise 		  = False

-- C O N S T A N T   P R O P A G A T I O N

constProp :: Exp -> Exp 

constProp (InfixE (Just e1) op (Just e2)) 
	| (justZero e1) && (isPlus op)                     = e2
	| (justZero e2) && (isPlus op)                     = e1
	| ((justZero e1) || (justZero e2)) && (isTimes op) = zero
	| ((justOne e1)) && (isTimes op)                   = e2
	| ((justOne e2)) && (isTimes op)                   = e1
	| (isPlus op) || (isTimes op)                      = (InfixE (Just (constProp e1)) op (Just (constProp e2)))

constProp e = e

iterateConstProp:: Exp -> Exp
iterateConstProp = until (\x -> constProp x == x) constProp 


applyConstProp :: ExpQ -> ExpQ
applyConstProp eq = do
	e <- eq 
	return (iterateConstProp e)

main = do 
	printAST $ applyConstProp [| 1 + 4 * 0 + 3 + 1 * 0 |]
--main = printAST  [| 2 + 3 |]

