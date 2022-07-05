module Main where

-- import MyParser (parser, parseProg)
-- import MyTypeCheck (typeCheckProg)
import MyCodeGen as CodeGen (main)
-- import Sprockell (run, Instruction)

-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
-- compile :: String -> [Instruction]
-- compile txt = do
--     ast <- typeCheckProg (parser parseProg txt)
--     pure $ codeGen ast

-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main = CodeGen.main
