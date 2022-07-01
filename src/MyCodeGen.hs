module MyCodeGen (codeGen) where

import Sprockell
import MyTypeCheck
import MyParser

import Debug.Trace

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
    EndProg]

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
statementsToInstructions statements addr rows = (trace("In statementsToInstructions"))(instructions, ac, vt)
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
stmGen (CreateVariable dtype name (B b)) addr rows | b         = ([Load (ImmValue (fromIntegral 1)) regA, Store regA (DirAddr addr)], addr+1, (rows++[(name, 0, -1, addr)]))
                                                   | otherwise = ([Load (ImmValue (fromIntegral 0)) regA, Store regA (DirAddr addr)], addr+1, (rows++[(name, 0, -1, addr)]))
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
       (compareInstructions, _, _) = compareGen cmp 2 addr rows
       numOfComp = length compareInstructions
       instructions = compareInstructions ++ [Branch 2 (Rel (numOfInstructions + 2))] ++ stmsInstructions ++ [(Jump (Rel (-(numOfInstructions + 1 + numOfComp))))]
stmGen (If cmp stms1 elseIfs stms2) addr rows | numOfElse == 0 && numOfElseIf == 0 = (ifInstructions, newAddr, newRows)
                                              | numOfElse > 0 && numOfElseIf > 0   = (elseInstructions, elseAddr, elseRows)
                                              | numOfElse > 0 && numOfElseIf == 0  = (elseInstructions, elseAddr, elseRows)
                                              | numOfElse == 0 && numOfElseIf > 0  = (elseIfInstruction, elseIfAddr, elseIfRows)
                                              
    where
       (ifInstructions, newAddr, newRows) = statementsToInstructions stms1 addr rows
       numOfIf = length ifInstructions
       (compareInstructions, _, _) = compareGen cmp 2 addr rows
       numOfComp = length compareInstructions
       compWithBranch = compareInstructions ++ [Branch 2 (Rel (numOfIf + 2))]
       afterIfInstr | numOfElse > 0 && numOfElseIf > 0   = compWithBranch ++ ifInstructions ++ [Jump (Rel (numOfElseIf + numOfElse))]
                    | numOfElse > 0 && numOfElseIf == 0  = compareInstructions ++ [Branch 2 (Rel (numOfIf + 2 + numOfElse))] ++ ifInstructions
                    | numOfElse == 0 && numOfElseIf > 0  = compWithBranch ++ ifInstructions ++ [Jump (Rel numOfElseIf)]
                    | numOfElse == 0 && numOfElseIf == 0 = compWithBranch ++ ifInstructions
       (elseIfInstruction, elseIfAddr, elseIfRows) = foldl (\(accInst, accAc, accVt) x -> p (elseIfGen x accAc accVt) accInst) (afterIfInstr, newAddr, newRows) elseIfs
       numOfElseIf = length elseIfInstruction
       (elseInstructions, elseAddr, elseRows) = statementsToInstructions stms2 elseIfAddr elseIfRows
       numOfElse = length elseInstructions


elseIfGen :: (Comparison, [Statement]) -> Int -> VarTable -> ([Instruction], Int, VarTable)
elseIfGen (cmp, stms) addr rows = (instructions, newAddr, newRows)
    where
       (compareInstructions, _, _) = compareGen cmp 2 addr rows
       numOfComp = length compareInstructions
       (stmsInstructions, newAddr, newRows) = statementsToInstructions stms addr rows
       numOfInstructions = length stmsInstructions
       instructions = compareInstructions ++ [Branch 2 (Rel (numOfInstructions + 2))] ++ stmsInstructions
-- If Comparison [Statement] [(Comparison, [Statement])] [Statement]
-- % While Comparison [Statement]
-- Parallel [Statement]
-- LockStart Integer
-- LockEnd Integer
-- SequentialThread [Statement]
-- % Assign String Expression
-- % CreateVariable String String Expression
-- % Print Expression
-- % Shared Statement

     
compareGen :: Comparison -> Int -> Int -> VarTable -> ([Instruction], Int, VarTable)
compareGen (Smaller expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute GtE opNum nextOpNum opNum]
compareGen (Bigger expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute LtE opNum nextOpNum opNum]
compareGen (SmallerEqual expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute Gt opNum nextOpNum opNum]
compareGen (BiggerEqual expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute Lt opNum nextOpNum opNum]
compareGen (Eq expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute NEq opNum nextOpNum opNum]
compareGen (NotEqual expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       (expr1Instr, _, _) = exprGen expr1 opNum addr rows  
       (expr2Instr, _, _) = exprGen expr2 nextOpNum addr rows
       nextOpNum = opNum + 1
       instructions = expr1Instr ++ expr2Instr ++ [Compute Equal opNum nextOpNum opNum]
compareGen (Boolean b) opNum addr rows | b         = ([Load (ImmValue 1) opNum], addr, rows)
                                       | otherwise = ([Load (ImmValue 0) opNum], addr, rows)
compareGen (AndOp comp1 comp2) opNum addr rows = (instructions, addr, rows)
    where
       comp1Instr = getInstructions (compareGen comp1 opNum addr rows)
       newOpNum = opNum + 1
       comp2Instr = getInstructions (compareGen comp2 newOpNum addr rows)
       instructions = comp1Instr ++ comp2Instr ++ [Compute Or opNum newOpNum opNum]
compareGen (OrOp comp1 comp2) opNum addr rows = (instructions, addr, rows)
    where
       comp1Instr = getInstructions (compareGen comp1 opNum addr rows)
       newOpNum = opNum + 1
       comp2Instr = getInstructions (compareGen comp2 newOpNum addr rows)
       instructions = comp1Instr ++ comp2Instr ++ [Compute And opNum newOpNum opNum]

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
exprGen (B b) opNum addr rows | b         = ([Load (ImmValue (fromIntegral 1)) opNum], addr, rows)
                              | otherwise = ([Load (ImmValue (fromIntegral 0)) opNum], addr, rows)
exprGen (BComp comp) opNum addr rows = (instructions ++ [(Load (DirAddr opNum) opNum)], addr, rows)
    where
       (instructions, _, _) = compareGen comp (opNum + 1) addr rows