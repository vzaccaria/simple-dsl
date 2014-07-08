{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Symbolic Convolution Playground
-- Copyright   :  Vittorio Zaccaria - 2014
-- License     :  BSD
--
-- Maintainer  :  vittorio.zaccaria@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Giving a try to symbolic convolution with 
-- Numeric prelude
--

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.FloatingPoint as FP
import qualified Algebra.ZeroTestable as AZT
import qualified MathObj.Polynomial as MOP
import qualified MathObj.Matrix as M
import MathObj.Matrix (transpose)

-- NP.Float is a field
import qualified NumericPrelude as NP
import NumericPrelude 
import qualified Number.Ratio as NR
import qualified Algebra.IntegralDomain as ID 
import qualified Algebra.PrincipalIdealDomain as PID
import Numeric as N
import qualified Data.List as DL 
import Data.Maybe
import qualified Prelude as P

type Matrix = M.T NP.Float
type MatrixPolynomial = MOP.T Matrix 
type FloatPolynomial = MOP.T Float

-- Represents the presence of a coefficient in position `datum` in a 
-- Sequence of coefficients of length `total`.
data Coeff 	 = Coeff 	{ datum :: Int, total :: Int }

-- Creates a matrix with a 1 in the position of the coefficient. 
-- For example, if we have a signal 
fromCoeff::Coeff -> Matrix 
fromCoeff c = M.fromList (total c) 1 [ if x == (datum c) then 1.0 else 0.0 | x <- [1 .. (total c)]]

--                            /- from Coeff.
-- x_0 + x_1p + x_2p^2 => [ 1 0 0 ] + [ 0 1 0 ]p + [ 0 0 1 ]p^2
signal::Int -> MatrixPolynomial 
signal n = MOP.fromCoeffs $ [ (fromCoeff $ Coeff x n) | x <- [ 1 .. n] ]

--                            /- from Coeff.
-- h_0 + h_1p + h_2p^2 => [ 1 ] + [ 0 ]p + [ 0 ]p^2
-- 						  [ 0 ] + [ 1 ]  + [ 0 ]
-- 						  [ 0 ] + [ 0 ]  + [ 1 ]
transfer::Int -> MatrixPolynomial 
transfer n = MOP.fromCoeffs $ [ transpose $ (fromCoeff $ Coeff x n) | x <- [ 1 .. n] ]


-- Get a specified coefficient in the polynomial
-- 
getCoeff :: MOP.T a -> Int -> Maybe a 
getCoeff p n  
	| length (MOP.coeffs p) > n = Just ((MOP.coeffs p) !! n)
	| otherwise = Nothing

-- Given:
-- (f_0(h,x)) + ((f_1(h,x)))p + ((f_2(h,x))p^2 + ...
-- returns for n=2: 
--
-- ((f_2(h,x))p^2
-- i.e., a fully qualified monomial
--
getMonomial::MatrixPolynomial -> Int -> MatrixPolynomial 
getMonomial s n 
    | getCoeff s n == Nothing  = (zero s) 
    | otherwise = let element = fromJust $ getCoeff s n
    			  in  let prologue = replicate (n-1) (zero s) 
    			  	  	in 	let t = MOP.fromCoeffs ((prologue) ++ [ element ])
    			  	  	 	in t


showf::Float -> String 
showf 0.0 = "0"
showf (1.0) = "1"
showf (-1.0) = "-1"
showf x = show x

-- showFFloat (Just 0) j "" 

formatMatrix::Matrix -> String 
formatMatrix m = do i <- M.rows m;
					(DL.intercalate " & " [ showf j | j<- i]) ++ "\\\\\n"

print::Matrix -> Int -> String
print coeff n 
	| isZero coeff = ""
	| otherwise = 	"\\left[\\begin{array}{" ++ [ 'c' | x <- columnSequence ] ++ "}" ++ (formatMatrix coeff) ++ "\\end{array}\n\\right]" ++ "p^" ++ (show n) 
			  	   	where columnSequence = [1 .. M.numColumns coeff]

recursive_print::[Matrix] -> Int -> String 
recursive_print (x:[]) n = (Main.print x n) ++ "\\\\"
recursive_print (x:xs) n = (Main.print x n) ++ "  " ++ recursive_print xs (n+1)


toString::MatrixPolynomial -> String 
toString s = (recursive_print (MOP.coeffs s) 0)


signalZero::MatrixPolynomial -> MatrixPolynomial
signalZero s = zero (M.numRows fm) (M.numColumns fm)
		   where fm = (MOP.coeffs s) !! 0


pdegree :: MOP.T a -> Int
pdegree p = length (MOP.coeffs p)


multiplyCoeffMatrixByPoly :: Maybe Matrix -> FloatPolynomial -> Maybe MatrixPolynomial 
multiplyCoeffMatrixByPoly Nothing _ = Nothing
multiplyCoeffMatrixByPoly (Just cm) (pl) = 
	let scaleMatrix = \x -> fromJust $ scaleMatrixByFactor (Just cm) (Just x) 1.0
	in  let scaled = map scaleMatrix (MOP.coeffs (pl))
		in Just (MOP.fromCoeffs scaled)


shiftPoly::  FloatPolynomial -> Int ->  FloatPolynomial
shiftPoly x n =   (MOP.fromCoeffs ((replicate n 0.0) ++ (MOP.coeffs x)))

discardHigherDegreeCoefficient::  (MOP.T a) ->  (MOP.T a) 
discardHigherDegreeCoefficient p = (MOP.fromCoeffs (init (MOP.coeffs p)))

scaleMatrixByFactor :: Maybe Matrix -> Maybe Float -> Float -> Maybe Matrix
scaleMatrixByFactor Nothing _ _ = Nothing 
scaleMatrixByFactor _ Nothing _ = Nothing 
scaleMatrixByFactor (Just cm) (Just f) c = Just (M.scale (c*f) cm)


modulo'::MatrixPolynomial -> FloatPolynomial -> MatrixPolynomial 
modulo' s p 
	| pdegree p > pdegree s 	= s 
	| otherwise 				= 	let n = pdegree p
									in 	let d = pdegree s - pdegree p 
										in 	let frontf = getCoeff s (n+d-1)
											in 	let frontb = getCoeff p (n-1)
												in 	let b_p = shiftPoly (discardHigherDegreeCoefficient p) d
													in 	let ab = scaleMatrixByFactor frontf frontb (-1.0)
														in 	let tmpmod = maybe (zero s) id (multiplyCoeffMatrixByPoly ab b_p)
															in tmpmod + modulo (discardHigherDegreeCoefficient s) p

instance AZT.C (M.T Float) where 
	isZero m = P.all (\x -> x == 0.0) (concat $ M.rows m)



modulo::MatrixPolynomial -> FloatPolynomial -> MatrixPolynomial
modulo s p 
	| s == y = s 
	| otherwise = y `modulo` p
	where y = s `modulo'` p

--t::FloatPolynomial
t1 = MOP.fromCoeffs [0, 1::NP.Float]
t2 = MOP.fromCoeffs [-1, 1::NP.Float]
t3 = MOP.fromCoeffs [1, 0, 1::NP.Float]


convo::MatrixPolynomial 
convo = (transfer 8) * (signal 8)

main = do
	putStrLn $ toString $ (convo `modulo` t1) 
	putStrLn $ toString $ (convo `modulo` t2) 
	putStrLn $ toString $ (convo `modulo` t3) 


	--putStrLn $ show $  t
