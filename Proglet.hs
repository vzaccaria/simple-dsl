module Main where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import A

--power3 :: Int -> Int
--power3 = $(mk_power 3)

-- Utilities from https://www.cs.kent.ac.uk/people/rpg/cmb21/PTH.pdf

printAST :: ExpQ -> IO ()
printAST  ast = runQ ast >>= putStrLn . show

printCode :: Q Exp -> IO ()
printCode ast = runQ ast >>= putStrLn . pprint

--main = printCode (mk_power 3)



main = printCode(A.flt [1.0,0.0,0.0])


--main = print $ fil [1,2] [3,4] 
