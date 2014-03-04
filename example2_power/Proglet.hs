{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where


import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH
import A

printCode :: Q Exp -> IO ()
printCode ast = runQ ast >>= putStrLn . pprint

main = printCode [| \x -> $(A.expand_power_anf2 4 [| x + x |]) |]