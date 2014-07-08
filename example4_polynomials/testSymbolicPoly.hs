
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



-- |
-- Module      :  Signal Playground
-- Copyright   :  Vittorio Zaccaria - 2014
-- License     :  BSD
--
-- Maintainer  :  vittorio.zaccaria@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Let's give a try to symbolic computation with 
-- Numeric prelude
--



import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.FloatingPoint as FP
import qualified MathObj.Polynomial as MOP

-- NP.Float is a field
import qualified NumericPrelude as NP

import qualified Number.Ratio as NR
import qualified Algebra.IntegralDomain as ID 
import qualified Algebra.PrincipalIdealDomain as PID
import Numeric as N


type Delays = MOP.T NP.Float
type Signal = MOP.T Delays


fromImpulseResponse:: [NP.Float] -> Signal 
fromImpulseResponse x = MOP.fromCoeffs [(MOP.fromCoeffs x)]

impulse = fromImpulseResponse [1]

fromSignal:: [NP.Float] -> Signal 
fromSignal x = let z = map (\q -> MOP.fromCoeffs [q]) x
			   in (MOP.fromCoeffs z) 

h = fromImpulseResponse [1,1]
x = fromSignal [1,1]

class CanDisplaySignal a where 
	showSig:: a -> String
	size:: a -> Int
	isUnit:: a -> Bool 
	isZero:: a -> Bool

printExp:: (CanDisplaySignal a) => [a] -> Integer -> String -> String
printExp (x:[]) g var = printCoeff x g var 
printExp (x:xs) g var =
	let prv = printCoeff x g var 
 	in 	let nx = (printExp xs (g+1) var)
 		in 	let op = if (length nx > 0 && nx!!0 /= '+' && prv /= "") then "+" else ""
 			in prv ++ op ++ nx
 		

printCoeff:: (CanDisplaySignal a) => a -> Integer -> String -> String
printCoeff c g var 
 	| isUnit c = rm
 	| isZero c = ""
 	| otherwise               = let cc = showSig c
 							     in if cc == "" then "" else "(" ++ cc ++ ")" ++ rm 
    where rm = var ++ "_" ++ (show g)

instance CanDisplaySignal Signal where
	showSig s = printExp (MOP.coeffs (s)) 0 "x"
	size s = length $ MOP.coeffs (s)
	isUnit s = False
	isZero s = False 

instance CanDisplaySignal Delays where
	showSig s = printExp (MOP.coeffs (s)) 0 "h"
	size s = length $ MOP.coeffs (s)
	isUnit s = False
	isZero s = False

instance CanDisplaySignal NP.Float where
	showSig s = showFFloat (Just 0) s ""
	size s = 1
	isUnit s = (s==1.0) 
	isZero s = (s==0.0)

-- printExp [x:xs] g var
-- | g == 0 	= ""
-- | otherwise = var ++ "^" ++ (show g)


--showSignal:: Signal -> String 
--showSignal y = 


--f a = (a+1)^10 
main = do
	putStrLn $ showSig (x*h)
