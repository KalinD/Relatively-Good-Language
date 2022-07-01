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
      Load (ImmValue 0) 2,
      WriteInstr 2 numberIO,
      Jump (Rel (-1)),
      EndProg
  ]

-- Table: [(name, scope, regAddr, memAddr)]
-- if regAddr or memAddr = -1 its not stored in the memory
type VarTable = [(String, Int, Int, Int)]

getMemAddress :: (String, Int, Int, Int) -> Int
getMemAddress (_, _, _, addr) = addr

progGen :: Program -> [Instruction]
progGen (Program []) = []
progGen (Program stms) = getInstructions (statementsToInstructions stms 0 []) ++ [EndProg]

p :: ([Instruction], Int, VarTable) -> [Instruction] -> ([Instruction], Int, VarTable)
p (insts, addr, vt) instructions = (instructions ++ insts, addr, vt)

statementsToInstructions :: [Statement] -> Int -> VarTable -> ([Instruction], Int, VarTable)
statementsToInstructions statements addr rows = (instructions, ac, vt)
       where
              (instructions, ac, vt) = foldl (\(accInst, accAc, accVt) x -> p (stmGen x accAc accVt) accInst) ([], addr, rows) statements


getInstructions :: ([Instruction], Int, VarTable) -> [Instruction]
getInstructions (i, _, _) = i
getmemAddrCounter :: ([Instruction], Int, VarTable) -> Int
getmemAddrCounter (_, ac, _) = ac
getVarTable :: ([Instruction], Int, VarTable) -> VarTable
getVarTable (_, _, vt) = vt

getVarAddress :: String -> VarTable -> Int
getVarAddress name rows | length vars > 0 = getMemAddress (head vars)
                        | otherwise       = error ("Variable '" ++ name ++ "' not found in memory!")
       where 
              vars = filter (\(n, scope, regAddr, memAddr) -> n == name) rows

-- statement to generate -> addr counter -> (instructions to perform, addr counter, variable table)
stmGen :: Statement -> Int -> VarTable -> ([Instruction], Int, VarTable)
stmGen (CreateVariable dtype name (I n)) addr rows = ([Load (ImmValue (fromIntegral n)) regA, Store regA (DirAddr addr)], addr+1, (rows++[(name, 0, -1, addr)]))
stmGen (Assign name expr) addr rows = (instructions, addr, rows)
       where
              varAddress = getVarAddress name rows
              instructions = (getInstructions (exprGen expr 2 addr rows)) ++ [Store 2 (DirAddr varAddress)]
stmGen (Print expr) addr rows = (instructions, addr, rows)
       where
              instructions = getInstructions (exprGen expr 2 addr rows) ++ [WriteInstr 2 numberIO]
stmGen (While cmp stms) addr rows = (instructions, newAddr, newRows)
       where
              (stmsInstructions, newAddr, newRows) = statementsToInstructions stms addr rows
              numOfInstructions = length stmsInstructions
              (compareInstructions, _, _)= compareGen cmp addr rows
              numOfComp = length compareInstructions
              instructions = compareInstructions ++ [Branch 2 (Rel (numOfInstructions + 1))] ++ stmsInstructions ++ [(Jump (Rel (-(numOfInstructions + 1 + numOfComp))))]

           
compareGen :: Comparison -> Int -> VarTable -> ([Instruction], Int, VarTable)
compareGen (Smaller expr1 expr2) addr rows = (instructions, addr, rows)
       where
              (expr1Instr, _, _) = exprGen expr1 2 addr rows  
              (expr2Instr, _, _) = exprGen expr2 3 addr rows
              instructions = expr1Instr ++ expr2Instr ++ [Compute GtE 2 3 2]

exprGen :: Expression -> Int -> Int -> VarTable -> ([Instruction], Int, VarTable)
exprGen (Addition expr1 expr2) opNum addr rows = (instructions, addr, rows)
       where
              instructions = (getInstructions (exprGen expr1 opNum addr rows)) ++ (getInstructions (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Add opNum (opNum + 1) opNum]
exprGen (Subtraction expr1 expr2) opNum addr rows = (instructions, addr, rows)
       where
              instructions = (getInstructions (exprGen expr1 opNum addr rows)) ++ (getInstructions (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Sub opNum (opNum + 1) opNum]
exprGen (Multiplication expr1 expr2) opNum addr rows = (instructions, addr, rows)
       where
              instructions = (getInstructions (exprGen expr1 opNum addr rows)) ++ (getInstructions (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Mul opNum (opNum + 1) opNum]
exprGen (Identifier id) opNum addr rows = ([Load (DirAddr varAddress) opNum], addr, rows)
       where
              varAddress = getVarAddress id rows
exprGen (I i) opNum addr rows = ([Load (ImmValue (fromIntegral i)) opNum], addr, rows)