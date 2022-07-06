module MyCodeGen where

import Sprockell 
import MyTypeCheck
import MyParser

import Debug.Trace
import System.Environment
import Data.List

-- Table: [(name, type, isShared, scope, memAddr)]
type Address = Int
type VarTable = [(String, String, Bool, Int, Address)]
type LockTable = [(Address, Bool)] -- lock (True) or latch (False)

latchStart = (4 :: Int)
shVarStart = (8 :: Int)
numLatches = (4 :: Int)
numLocks   = (4 :: Int)

getVarName :: (String, String, Bool, Int, Int) -> String
getVarName (name, _, _, _, _) = name
getIsShared :: (String, String, Bool, Int, Int) -> Bool
getIsShared (_, _, shared, _, _) = shared
getMemAddress :: (String, String, Bool, Int, Int) -> Int
getMemAddress (_, _, _, _, addr) = addr

progGen :: Program -> [[Instruction]]
progGen (Program []) = []
progGen (Program stms) = res
    where
        intermediate = getInstructions (statementsToInstructions stms 0 [] [])
        res = fmap (\x -> x ++ [EndProg]) intermediate

statementsToInstructions :: [Statement] -> Int -> VarTable -> LockTable -> ([[Instruction]], Int, VarTable, LockTable)
statementsToInstructions statements addr rows locks = (instructions, ac, vt, lt)
    where
       (instructions, ac, vt, lt) = foldl (\(accInst, accAc, accVt, accLt) x -> (stmGen x accAc accVt accLt accInst)) ([], addr, rows, locks) statements

getInstructions :: (a, Int, VarTable, LockTable) -> a
getInstructions (i, _, _, _) = i
getInstructionsWithoutLock :: (a, Int, VarTable) -> a
getInstructionsWithoutLock (i, _, _) = i
getmemAddrCounter :: ([Instruction], Int, VarTable, LockTable) -> Int
getmemAddrCounter (_, ac, _, _) = ac
getVarTable :: ([Instruction], Int, VarTable, LockTable) -> VarTable
getVarTable (_, _, vt, _) = vt
getLockTable :: ([Instruction], Int, VarTable, LockTable) -> LockTable
getLockTable (_, _, _, lt) = lt

getVarAddress :: String -> VarTable -> Int
getVarAddress name rows | length vars > 0 = getMemAddress (head vars)
                        | otherwise       = error ("Variable '" ++ name ++ "' not found in memory!")
    where 
       vars = filter (\x -> (getVarName x) == name) rows

-- statement to generate -> addr counter -> (instructions to perform, addr counter, variable table)
stmGen :: Statement -> Int -> VarTable -> LockTable -> [[Instruction]] -> ([[Instruction]], Int, VarTable, LockTable)
stmGen (CreateVariable dtype name expr) addr rows locks oldInstructions = (finalInstructions, addr+1, (rows++[(name, dtype, False, 0, addr)]), locks)
    where
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions = ois ++ (getInstructionsWithoutLock (exprGen expr regA addr rows)) ++ [Store regA (DirAddr addr)]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (Shared (CreateVariable dtype name expr)) addr rows locks oldInstructions = (finalInstructions, addr, (rows++[(name, dtype, True, 0, sharedFreePosition)]), locks)
    where
        sharedVars = filter (\x -> getIsShared x) rows
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        sharedFreePosition = (foldl (\acc x -> max acc x) 7 (fmap getMemAddress sharedVars)) + 1
        instructions = ois ++ (getInstructionsWithoutLock (exprGen expr regA addr rows)) ++ [WriteInstr regA (DirAddr sharedFreePosition)]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (Assign name expr) addr rows locks oldInstructions = (finalInstructions, addr, rows, locks)
    where
        varAddress = getVarAddress name rows
        isShared = getIsShared (head (filter (\x -> getVarName x == name) rows))
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions | not isShared = ois ++ (getInstructionsWithoutLock (exprGen expr 2 addr rows)) ++ [Store 2 (DirAddr varAddress)]
                     | otherwise    = ois ++ (getInstructionsWithoutLock (exprGen expr 2 addr rows)) ++ [WriteInstr 2 (DirAddr varAddress)]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (Print expr) addr rows locks oldInstructions = (finalInstructions, addr, rows, locks)
    where
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions = ois ++ (getInstructionsWithoutLock (exprGen expr 2 addr rows)) ++ [WriteInstr 2 numberIO]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (While cmp stms) addr rows locks oldInstructions = (finalInstructions, newAddr, newRows, locks)
    where
        (stmsInstructions, newAddr, newRows, _) = statementsToInstructions stms addr rows locks
        numOfInstructions = length (head stmsInstructions)
        compareInstructions = getInstructionsWithoutLock (compareGen cmp 2 addr rows)
        numOfComp = length compareInstructions
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions = ois ++ compareInstructions ++ [Branch 2 (Rel (numOfInstructions + 2))] ++ (head stmsInstructions) ++ [(Jump (Rel (-(numOfInstructions + 1 + numOfComp))))]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (If cmp stms1 elseIfs stms2) addr rows locks oldInstructions = (finalInstructions, finalAddr, finalRows, locks)
    where
        ifCmpInstructions = getInstructionsWithoutLock (compareGen cmp 2 addr rows)
        numOfCmp = length ifCmpInstructions
        (ifInstructions, newAddr, newRows, _) = statementsToInstructions stms1 addr rows locks
        numOfIf = length (head ifInstructions)
        (elseIfInstructions, elseIfAddr, elseIfRows, _) = elseIfGen elseIfs numOfElse newAddr newRows locks
        numOfElseIf = length elseIfInstructions
        (elseInstructions, finalAddr, finalRows, _) = statementsToInstructions stms2 elseIfAddr elseIfRows locks
        numOfElse = length (head elseInstructions)
        toSkip = numOfElseIf + numOfElse
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        ifInstr | toSkip == 0 = ifCmpInstructions ++ [Branch 2 (Rel (numOfIf + 1))] ++ (head ifInstructions)
                | otherwise   = ifCmpInstructions ++ [Branch 2 (Rel (numOfIf + 2))] ++ (head ifInstructions) ++ [Jump (Rel (toSkip + 1))]
        instructions = ois ++ ifInstr ++ elseIfInstructions ++ (head elseInstructions)
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (LockStart n) addr rows locks oldInstructions = (finalInstructions, addr, rows, newLocks)
    where
        newLocks = locks ++ [((fromIntegral n), True)]
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions = ois ++ [TestAndSet (DirAddr (fromIntegral n)), Receive regA, Compute Equal reg0 regA regA, Branch regA (Rel 2), Jump (Rel (-4))]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (LockEnd n) addr rows locks oldInstructions = (finalInstructions, addr, rows, newLocks)
    where
        newLocks = filter (\(addr, b) -> (fromIntegral addr) /= n) locks
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        instructions = ois ++ [WriteInstr reg0 (DirAddr (fromIntegral n))]
        finalInstructions = [instructions] ++ (oldInstructions \\ [ois])
stmGen (Parallel stms) addr rows locks oldInstructions = (finalInstr, newAddr, newRows, finalLocks)
    where
        (instr, newAddr, newRows, newLocks) = foldl (\(accInstr, accAddr, accRows, accLocks) x -> combineInstructions (stmGen x accAddr accRows accLocks []) accInstr) ([], addr, rows, locks) stms
        latches = filter (\(addr, b) -> not b) newLocks
        firstFreeLatch | length latches == numLatches = error ("Can't create more than " ++ show numLatches ++ " parallel blocks!")
                       | length latches >= 0  = length latches
        locksList = filter (\(addr, b) -> b) newLocks
        firstFreeLock | (length locksList) == numLocks = error "Not enough memory for another parallel block!"
                      | (length locksList) >= 0 = length locksList
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []
        latchAddr = firstFreeLatch + latchStart
        lockAddr = firstFreeLock
        numChildren = length stms
        -- finalLocks = newLocks ++ [(latchAddr, False)]
        finalLocks = newLocks ++ [(lockAddr, True),(latchAddr, False)]
        latchCheck = [ReadInstr (DirAddr latchAddr), Receive regA, Compute NEq reg0 regA regA, Branch regA (Rel 2), Jump (Rel (-4))]
        -- notifyParent = (aLockGen lockAddr) ++ 
        finalInstr = [(ois ++ [TestAndSet (DirAddr latchAddr)])] ++ ((fmap (\x -> latchCheck ++ x) instr))
stmGen (SequentialThread stms) addr rows locks oldInstructions = ([instructions], finalAddr, finalRows, newLock)
    where
        (instr, finalAddr, finalRows, newLock) = foldl (\(accInstr, accAddr, accRows, accLock) x -> stmGen x accAddr accRows accLock accInstr) ([], addr, rows, locks) stms
        ois | length oldInstructions > 0 = head oldInstructions
            | otherwise                  = []    
        instructions = ois ++ (head instr)

combineInstructions :: ([[Instruction]], Int, VarTable, LockTable) -> [[Instruction]] -> ([[Instruction]], Int, VarTable, LockTable)
combineInstructions (oldInstructions, addr, rows, locks) newInstructions = (oldInstructions ++ newInstructions, addr, rows, locks)

elseIfGen :: [(Comparison, [Statement])] -> Int -> Int -> VarTable -> LockTable -> ([Instruction], Int, VarTable, LockTable)
elseIfGen elseIfs elseLength addr rows locks = (instructions, newAddr, newRows, newLock)
    where

       (instructions, newAddr, newRows, newLock, _) = foldr (\x (accInstr, accAddr, accRows, accLock, accLen) -> combineElseIfInstructions (singleElseIfGen x elseLength accLen accAddr accRows accLock) accInstr) ([], addr, rows, locks, 0) elseIfs

singleElseIfGen :: (Comparison, [Statement]) -> Int -> Int -> Int -> VarTable -> LockTable -> ([[Instruction]], Int, VarTable, LockTable, Int)
singleElseIfGen (cmp, stms) elseLength prevLength addr rows locks = ([instructions], newAddr, newRows, newLock, length instructions)
    where
        cmpInstructions = getInstructionsWithoutLock (compareGen cmp 2 addr rows)
        numOfCmp = length cmpInstructions
        (stmsInstructions, newAddr, newRows, newLock) = statementsToInstructions stms addr rows locks
        numOfInstructions = length stmsInstructions
        toSkip = prevLength + elseLength
        instructions | toSkip == 0 = cmpInstructions ++ [Branch 2 (Rel (numOfInstructions + 1))] ++ (head stmsInstructions)
                     | toSkip > 0  = cmpInstructions ++ [Branch 2 (Rel (numOfInstructions + 2))] ++ (head stmsInstructions) ++ [Jump (Rel (toSkip + 1))]
                     | otherwise   = error "Incorrect elseLength!" 

combineElseIfInstructions :: ([[Instruction]], Int, VarTable, LockTable, Int) -> [Instruction] -> ([Instruction], Int, VarTable, LockTable, Int)
combineElseIfInstructions (inst, addr, rows, locks, len) newInstr = ((head inst) ++ newInstr, addr, rows, locks, len)

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
       comp1Instr = getInstructionsWithoutLock (compareGen comp1 opNum addr rows)
       newOpNum = opNum + 1
       comp2Instr = getInstructionsWithoutLock (compareGen comp2 newOpNum addr rows)
       instructions = comp1Instr ++ comp2Instr ++ [Compute Or opNum newOpNum opNum]
compareGen (OrOp comp1 comp2) opNum addr rows = (instructions, addr, rows)
    where
       comp1Instr = getInstructionsWithoutLock (compareGen comp1 opNum addr rows)
       newOpNum = opNum + 1
       comp2Instr = getInstructionsWithoutLock (compareGen comp2 newOpNum addr rows)
       instructions = comp1Instr ++ comp2Instr ++ [Compute And opNum newOpNum opNum]

exprGen :: Expression -> Int -> Int -> VarTable -> ([Instruction], Int, VarTable)
exprGen (Addition expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       instructions = (getInstructionsWithoutLock (exprGen expr1 opNum addr rows)) ++ (getInstructionsWithoutLock (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Add opNum (opNum + 1) opNum]
exprGen (Subtraction expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       instructions = (getInstructionsWithoutLock (exprGen expr1 opNum addr rows)) ++ (getInstructionsWithoutLock (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Sub opNum (opNum + 1) opNum]
exprGen (Multiplication expr1 expr2) opNum addr rows = (instructions, addr, rows)
    where
       instructions = (getInstructionsWithoutLock (exprGen expr1 opNum addr rows)) ++ (getInstructionsWithoutLock (exprGen expr2 (opNum + 1) addr rows)) ++ [Compute Mul opNum (opNum + 1) opNum]
exprGen (Identifier id) opNum addr rows | isShared = ([ReadInstr (DirAddr varAddress), Receive opNum], addr, rows)
                                        | otherwise = ([Load (DirAddr varAddress) opNum], addr, rows)
    where
       varAddress = getVarAddress id rows
       isShared = getIsShared (head (filter (\x -> getVarName x == id) rows)) --------------------------------------------------------
exprGen (I i) opNum addr rows = ([Load (ImmValue (fromIntegral i)) opNum], addr, rows)
exprGen (B b) opNum addr rows | b         = ([Load (ImmValue (fromIntegral 1)) opNum], addr, rows)
                              | otherwise = ([Load (ImmValue (fromIntegral 0)) opNum], addr, rows)
exprGen (BComp comp) opNum addr rows = (instructions ++ [(Load (DirAddr opNum) opNum)], addr, rows)
    where
       (instructions, _, _) = compareGen comp (opNum + 1) addr rows 

-- Generates SPRIL to acquire a lock at the given address.
aLockGen :: Int -> [Instruction]
aLockGen addr = [TestAndSet (DirAddr (fromIntegral addr)), Receive regA, Compute Equal reg0 regA regA, Branch regA (Rel 2), Jump (Rel (-4))]

-- Generates SPRIL to release a lock at the given address.
rLockGen :: Int -> [Instruction]
rLockGen addr = [WriteInstr reg0 (DirAddr (fromIntegral addr))]

main = do
    fileDest <- getArgs
    file <- readFile (head fileDest)
    -- let parsed = (parser parseProg file)
    -- putStrLn ("Parsed: " ++ show parsed)
    -- let typed = typeCheckProg (parser parseProg file)
    -- putStrLn ("Typed: " ++ show typed)
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    putStrLn ("Sprockell: " ++ show sprockells)
    run sprockells

-- test code
runS :: String -> [[Instruction]]
runS = progGen . typeCheckProg . parser parseProg