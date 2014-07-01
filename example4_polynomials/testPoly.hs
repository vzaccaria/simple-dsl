import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.FloatingPoint as FP
import qualified MathObj.Polynomial as MOP

-- NP.Float is a field
import qualified NumericPrelude as NP

import qualified Number.Ratio as NR
import qualified Algebra.IntegralDomain as ID 
import qualified Algebra.PrincipalIdealDomain as PID

type Polynomial      = MOP.T NP.Float
type PolynomialRatio = NR.T (Polynomial)

x::Polynomial
x = (MOP.fromCoeffs [-6,11,1::NP.Float])

y::Polynomial
y = (MOP.fromCoeffs[1,1::NP.Float])

-- Original polynomial ratio
q::PolynomialRatio
q = x NR.:% y


-- Remainder 
m::Polynomial
m = ID.mod x y

-- Quotient 
z::Polynomial
z = ID.div x y

showP::Polynomial -> String
showP = ($"") . MOP.showsExpressionPrec 0 "x"

--f a = (a+1)^10 
main = do
	putStrLn $ "\\frac{"
	putStrLn $ showP $ NR.numerator q 
	putStrLn $ "}{"
	putStrLn $ showP $ NR.denominator q 
	putStrLn $ "} \\rightarrow \\textrm{Q} = "
	putStrLn $ showP $ z 
	putStrLn $ ", \\textrm{R} =  "
	putStrLn $ showP $ m 
