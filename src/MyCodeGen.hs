module MyCodeGen (codeGen) where

import Sprockell
import MyTypeCheck
import MyParser

codeGen :: Integer -> [Instruction]
codeGen n = [ 
         Load (ImmValue $ fromInteger n) regE  -- upper bound is hardcoded
       , Load (ImmValue 0) regA                -- first number
       , Load (ImmValue 1) regB                -- second number

       -- "beginloop"
       , Compute Gt regA regE regC             -- regA > regE ?
       , Branch regC (Abs 12)                  -- then jump to target "end"
       , WriteInstr regA numberIO              -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC             -- regB > regE
       , Branch regC (Abs 12)                  -- target "end"
       , WriteInstr regB numberIO              -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))                       -- target "beginloop"

       -- "end"
       , EndProg
       ]

fibGen :: Integer -> [Instruction]
fibGen n = [
          Load (ImmValue $ fromInteger n) regA,     -- n = (value from the argument)
          Load (ImmValue 0) regB,                   -- i = 0
          Load (ImmValue 0) regC,                   -- fst = 0
          Load (ImmValue 1) regD,                   -- snd = 1
          Compute LtE regB regA regE,               -- i <= n 
          Branch regE (Abs 11),                     -- go to the end
          Load (IndAddr regD) regF,                 -- temp = snd
          Compute Add regC regD regD,               -- snd = fst + snd
          Compute Add regF reg0 regC,               -- fst = temp
          Compute Incr regB regB regB,              -- i = i + 1
          Jump (Rel (-6)),                          -- back to the start of the while loop
          WriteInstr regE numberIO,                 -- ouput snd

          -- end
          EndProg
          ]

fprog :: [Instruction]
fprog = [ ReadInstr numberIO            -- ask the user for a number
       , Receive regE                  -- save the number in regE

       , Load (ImmValue 0) regA        -- first number
       , Load (ImmValue 1) regB        -- second number

       -- "beginloop"
       , Compute Gt regA regE regC     -- regA > regE ?
       , Branch regC (Abs 13)          -- then jump to target "end"
       , WriteInstr regA numberIO      -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC     -- regB > regE
       , Branch regC (Abs 13)          -- target "end"
       , WriteInstr regB numberIO      -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))               -- target "beginloop"

       -- "end"
       , EndProg
       ]

go = run [fprog, fprog, fprog]

test :: [Instruction]
test = [
      Load (ImmValue 5) (0),
      Load (ImmValue 5) (9),
      WriteInstr (0) numberIO,
      WriteInstr (9) numberIO,
      EndProg
  ]

scopeTable = typeCheckProg <$> (parser parseProg <$> (readFile "../examples/genTest.rgl"))

-- Table: [(name, scope, regAddr, memAddr)]
-- if regAddr or memAddr = -1 its not stored in the memory
data VarTable = VarTable [(String, Int, Int, Int)]
              deriving Show

get3rd :: (a,b,c) -> c
get3rd (_,_,x) = x

-- program to generate -> addr counter -> (instructions to perform, addr counter, variable table)
progGen :: Program -> Int -> VarTable -> ([Instruction], Int, VarTable)
progGen (Program [stm]) addr vt = instrs
                                   where
                                          genrstm = (stmGen stm addr vt)
                                          instrs = fst genrstm
                                          naddr = snd genrstm 
                                          nvt = get3rd genrstm
progGen (Program (stm:stms)) addr vt = instrs ++ (progGen (Program stms) naddr nvt)
                                   where
                                          genrstm = (stmGen stm addr vt)
                                          instrs = fst genrstm
                                          naddr = snd genrstm 
                                          nvt = get3rd genrstm

-- statement to generate -> addr counter -> (instructions to perform, addr counter, variable table)
stmGen :: Statement -> Int -> VarTable -> ([Instruction], Int, VarTable)
stmGen (CreateVariable dtype name (I n)) addr vt = ([Load (ImmValue (fromIntegral n)) addr], addr+1, (vt++[(name, 0, -1, addr)]))