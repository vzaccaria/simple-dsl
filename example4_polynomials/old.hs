import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring

import           Numeric.AD

import qualified MathObj.Polynomial as MOP
import qualified NumericPrelude as NP

newtype Poly a = Poly {runPoly :: MOP.T a}

instance (Additive.C a, Ring.C a) => Num (Poly a) where
  (Poly a) + (Poly b)= Poly $ a NP.+ b
  (Poly a) - (Poly b)= Poly $ a NP.- b
  (Poly a) * (Poly b)= Poly $ a NP.* b
  negate (Poly a) = Poly (NP.negate a)
  fromInteger = Poly . NP.fromInteger

x :: Poly Int
x = Poly $ MOP.fromCoeffs [0,1]

showP = ($"") . MOP.showsExpressionPrec 0 "x" . runPoly

f a = (a+1)^3
main = putStrLn $ showP $ diff f x