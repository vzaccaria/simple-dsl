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

x::Polynomial -- (x-1)
x = (MOP.fromCoeffs [1,0,1 ::NP.Float])

y::Polynomial -- (x^2-1)
y = (MOP.fromCoeffs[1,1 ::NP.Float])

gcdz::[Polynomial] -> Polynomial
gcdz = fst . PID.extendedGCDMulti 

coeffz::[Polynomial] -> [Polynomial]
coeffz = snd . PID.extendedGCDMulti 



showP::Polynomial -> String
showP = ($"") . MOP.showsExpressionPrec 0 "x"

printPolyPair::(Polynomial, Polynomial) -> String
printPolyPair x = ((showP $ fst x) ++ " & " ++ (showP $ snd x) ++ " \\\\ ")::String

zipPolys:: [Polynomial] -> [Polynomial] -> [(Polynomial, Polynomial)]
zipPolys x y = NP.zipWith (\a b -> (a,b)) x y

showGCD:: ([Polynomial], [Polynomial]) -> [String]
showGCD (p,c) = map printPolyPair (zipPolys p c)

displayGCD:: [Polynomial] -> IO ()
displayGCD l =  mapM_ putStrLn $ showGCD (l, coeffz l)

main = do {
			putStrLn "\\begin{array}{ll}";
			putStrLn " m_j & (m_j)^{-1} \\\\";
			displayGCD [x, y];
			putStrLn "\\end{array}";
}
