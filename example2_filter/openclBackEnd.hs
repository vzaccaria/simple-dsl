{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
{- Copyright (c) 2011 Luis Cabellos -}


import Language.Haskell.TH.Syntax
import Language.Haskell.TH hiding (ppr)
import Language.Haskell.TH.Lib

import GHC.Num as N
import Control.Parallel.OpenCL

import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )

import Language.C.Quote.OpenCL
import qualified Language.C.Syntax as C

import Text.PrettyPrint.Mainland

import TopLevel (optPipe,firstOpenCLPipe)
import Utils (printCode, printAST, isDelay, isTimes, isPlus)


programSource :: Integer -> String
programSource n = show $ ppr [cunit|
__kernel void duparray(__global float *in, __local float *out)
{
  int id = get_global_id(0);
  out[id] = $(n) * in[id];
}
|]

kernelSource name sigin sigout exp =  [cunit| 

__kernel void $id:name (__global float *$id:(sigin), __local float *$id:(sigout))
{
  int t = get_global_id(0);
  $id:(sigout)[t] = $exp;
}
|] 

convert:: Exp -> C.Exp

convert (LamE vars e) = [cexp| $(convert e)|]

convert (InfixE (Just e1) op (Just e2))  
	| isDelay(op) = [cexp| $(convert e1)[t - $(convert e2)] 	|]
	| isTimes(op) = [cexp| ($(convert e1)) * ($(convert e2)) 	|]
	| isPlus(op)  = [cexp| ($(convert e1)) + ($(convert e2)) 	|]

convert (VarE x)                        = ([cexp| $id:(showName x) |])

convert (LitE (IntegerL (a))) = ([cexp| $int:(a) |])
convert (LitE (RationalL(a))) = ([cexp| $float:(a) |])

convert e = [cexp| 333 |]

convertQ:: ExpQ -> IO ()
convertQ eq = do
	e <- runQ eq;
	putStrLn $ show $ ppr $ (kernelSource "foo" "x_0" "y_0" (convert e))

--concretize:: (Maybe C.Exp) -> C.Exp 
--concretize Just e  = e
--concretize Nothing = [cexp| 4 |]

main = convertQ firstOpenCLPipe
