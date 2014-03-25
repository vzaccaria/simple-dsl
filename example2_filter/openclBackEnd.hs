{-# LANGUAGE QuasiQuotes #-}
module Main where
{- Copyright (c) 2011 Luis Cabellos -}
import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )
import Language.C.Quote.OpenCL
import Text.PrettyPrint.Mainland


programSource :: Integer -> String
programSource n = show $ ppr [cunit|
__kernel void duparray(__global float *in, __local float *out)
{
  int id = get_global_id(0);
  out[id] = $(n) * in[id];
}
|]

main = print $ programSource 3

