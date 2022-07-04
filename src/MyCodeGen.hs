module MyCodeGen  where-- (codeGen) where

import Sprockell
import MyTypeCheck
import MyParser

import Debug.Trace
import System.Environment   

-- Table: [(name, type, isShared, scope, regAddr, memAddr)]
-- if regAddr or memAddr = -1 its not stored in the memory
type VarTable = [(String, String, Bool, Int, Int, Int)]

getVarName :: (String, String, Bool, Int, Int, Int) -> String
getVarName (name, _, _, _, _, _) = name
getType :: (String, String, Bool, Int, Int, Int) -> String
getType (_, t, _, _, _, _) = t
getIsShared :: (String, String, Bool, Int, Int, Int) -> Bool
getIsShared (_, _, shared, _, _, _) = shared
getScope :: (String, String, Bool, Int, Int, Int) -> Int
getScope (_, _, _, scope, _, _) = scope
getRegAddress :: (String, String, Bool, Int, Int, Int) -> Int
getRegAddress (_, _, _, _, addr, _) = addr
getMemAddress :: (String, String, Bool, Int, Int, Int) -> Int
getMemAddress (_, _, _, _, _, addr) = addr

progGen :: Program -> [Instruction]
progGen (Program []) = []
progGen (Program stms) = getInstructions (statementsToInstructions stms 0 [])

-- TODO: Fix
combineInstructions :: ([Instruction], Int, VarTable) -> [Instruction] -> ([Instruction], Int, VarTable)
combineInstructions (i, addr, vt) instructions = (i ++ instructions, addr, vt)

statementsToInstructions :: [Statement] -> Int -> VarTable -> ([Instruction], Int, VarTable)
statementsToInstructions statements addr rows = (instructions, ac, vt)
    where
       (instructions, ac, vt) = foldl (\(accInst, accAc, accVt) x -> combineInstructions (stmGen x accAc accVt) accInst) ([], addr, rows) statements


getInstructions :: ([Instruction], Int, VarTable) -> [Instruction]
getInstructions (i, _, _) = i
getMemAddrCounter :: ([Instruction], Int, VarTable) -> Int
getMemAddrCounter (_, ac, _) = ac
getVarTable :: ([Instruction], Int, VarTable) -> VarTable
getVarTable (_, _, vt) = vt

getVarAddress :: String -> VarTable -> Int
getVarAddress name rows | length vars > 0 = getMemAddress (head vars)
                    | otherwise       = error ("Variable '" ++ name ++ "' not found in memory!")
    where 
       vars = filter (\x -> (getVarName x) == name) rows

-- % If Comparison [Statement] [(Comparison, [Statement])] [Statement]
-- % While Comparison [Statement]
-- Parallel [Statement]
-- LockStart Integer
-- LockEnd Integer
-- SequentialThread [Statement]
-- % Assign String Expression
-- % CreateVariable String String Expression
-- % Print Expression
-- Shared Statement

-- statement to generate -> addr counter -> (instructions to perform, addr counter, variable table)
stmGen :: Statement -> Int -> VarTable -> ([Instruction], Int, VarTable)
stmGen (CreateVariable dtype name (I n)) addr rows = ([Load (ImmValue (fromIntegral n)) regA, Store regA (DirAddr addr)], addr+1, (rows++[(name, dtype, False, 0, -1, addr)]))
stmGen (CreateVariable dtype name (B b)) addr rows = ([Load (ImmValue (fromIntegral val)) regA, Store regA (DirAddr addr)], addr+1, (rows++[(name, dtype, False, 0, -1, addr)]))
    where
        val | b         = 1
            | otherwise = 0
stmGen (Shared (CreateVariable dtype name (I n))) addr rows = ([Load (ImmValue (fromIntegral n)) regA, WriteInstr regA (DirAddr sharedFreePosition)], addr, (rows++[(name, dtype, True, 0, -1, sharedFreePosition)]))
    where
        sharedVars = filter (\x -> getIsShared x) rows
        sharedFreePosition = (foldl (\acc x -> max acc x) 7 (fmap getMemAddress sharedVars)) + 1
stmGen (Shared (CreateVariable dtype name (B b))) addr rows | b         = ([Load (ImmValue (fromIntegral val)) regA, WriteInstr regA (DirAddr sharedFreePosition)], addr, (rows++[(name, dtype, True, 0, -1, sharedFreePosition)]))
    where
        val | b         = 1
            | otherwise = 0
        sharedVars = filter (\x -> getIsShared x) rows
        sharedFreePosition = (foldl (\acc x -> max acc x) 7 (fmap getMemAddress sharedVars)) + 1
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
stmGen (If cmp stms1 elseIfs stms2) addr rows = (finalInstructions, finalAddr, finalRows)
    where
        ifCmpInstructions = getInstructions (compareGen cmp 2 addr rows)
        numOfCmp = length ifCmpInstructions
        (ifInstructions, newAddr, newRows) = statementsToInstructions stms1 addr rows
        numOfIf = length ifInstructions
        (elseIfInstructions, elseIfAddr, elseIfRows) = elseIfGen elseIfs numOfElse newAddr newRows
        numOfElseIf = length elseIfInstructions
        (elseInstructions, finalAddr, finalRows) = statementsToInstructions stms2 elseIfAddr elseIfRows
        numOfElse = length elseInstructions
        toSkip = numOfElseIf + numOfElse
        ifInstr | toSkip == 0 = ifCmpInstructions ++ [Branch 2 (Rel (numOfIf + 1))] ++ ifInstructions
                | otherwise   = ifCmpInstructions ++ [Branch 2 (Rel (numOfIf + 2))] ++ ifInstructions ++ [Jump (Rel (toSkip + 1))]
        finalInstructions = ifInstr ++ elseIfInstructions ++ elseInstructions
-- stmGen (LockStart n) addr rows = ([TestAndSet (DirAddr (fromIntegral n)), Receive regA, Compute NEq reg0 regA regA, Branch regA (Rel 2), Jump (Rel (-4))], addr, rows)

-- stmGen (LockEnd n) addr rows = ([WriteInstr reg0 (DirAddr n)], addr, rows)

-- stmGen (Parallel stms) addr rows = 

elseIfGen :: [(Comparison, [Statement])] -> Int -> Int -> VarTable -> ([Instruction], Int, VarTable)
elseIfGen elseIfs elseLength addr rows = (instructions, newAddr, newRows)
    where
       (instructions, newAddr, newRows, _) = foldr (\x (accInstr, accAddr, accRows, accLen) -> combineElseIfInstructions (singleElseIfGen x elseLength accLen accAddr accRows) accInstr) ([], addr, rows, 0) elseIfs

singleElseIfGen :: (Comparison, [Statement]) -> Int -> Int -> Int -> VarTable -> ([Instruction], Int, VarTable, Int)
singleElseIfGen (cmp, stms) elseLength prevLength addr rows = (instructions, newAddr, newRows, length instructions)
    where
        cmpInstructions = getInstructions (compareGen cmp 2 addr rows)
        numOfCmp = length cmpInstructions
        (stmsInstructions, newAddr, newRows) = statementsToInstructions stms addr rows
        numOfInstructions = length stmsInstructions
        toSkip = prevLength + elseLength
        instructions | toSkip == 0 = cmpInstructions ++ [Branch 2 (Rel (numOfInstructions + 1))] ++ stmsInstructions
                     | toSkip > 0  = cmpInstructions ++ [Branch 2 (Rel (numOfInstructions + 2))] ++ stmsInstructions ++ [Jump (Rel (toSkip + 1))]
                     | otherwise   = error "Incorrect elseLength!" 

combineElseIfInstructions :: ([Instruction], Int, VarTable, Int) -> [Instruction] -> ([Instruction], Int, VarTable, Int)
combineElseIfInstructions (inst, addr, rows, len) newInstr = (inst ++ newInstr, addr, rows, len)

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

main = do
    fileDest <- getArgs
    file <- readFile (head fileDest)
    let sprockell = progGen (typeCheckProg (parser parseProg file))
    run [sprockell]
