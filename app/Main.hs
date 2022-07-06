module Main where

import MyCodeGen as CodeGen (main)

{- Use the main defined in the "MyCodeGen.hs".
 - To run the program with a file "fib.rgl" in folder "examples" run `stack run "./examples/fib.rgl"`-}
main :: IO ()
main = CodeGen.main
