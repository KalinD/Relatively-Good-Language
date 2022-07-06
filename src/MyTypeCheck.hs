module MyTypeCheck where -- (typeCheck) where

import MyParser as Parser

-- Additional libraries
import Data.Typeable (typeOf, TypeRep)
import Debug.Trace

data Program = Program [Statement] deriving Show

data Statement = If Comparison [Statement] [(Comparison, [Statement])] [Statement]
               | While Comparison [Statement]
               | Parallel [Statement]
               | LockStart Integer
               | LockEnd Integer
               | SequentialThread [Statement]
               | Assign String Expression
               | CreateVariable String String Expression
               | Print Expression
               | Shared Statement
    deriving Show

data Expression = Addition       Expression Expression  -- TODO: Optional to +, -, * of booleans
                | Subtraction    Expression Expression
                | Multiplication Expression Expression
                | I  Integer    -- Integer
                | B  Bool       -- Boolean
                | BComp Comparison
                | Identifier String
    deriving Show

data Comparison = Smaller      Expression Expression
                | Bigger       Expression Expression
                | Eq           Expression Expression
                | NotEqual     Expression Expression
                | SmallerEqual Expression Expression
                | BiggerEqual  Expression Expression
                | AndOp        Comparison Comparison
                | OrOp         Comparison Comparison
                | Boolean Bool
    deriving Show

type VarsMap = [(Int, String, String)]

-- level, type, name
-- scope = [] :: [(Int, String, String)]
getLevel :: (Int, String, String) -> Int
getLevel (level, _, _) = level
getType :: (Int, String, String) -> String
getType  (_, t, _)     = t
getName :: (Int, String, String) -> String
getName  (_, _, name)  = name

typeCheckProg :: Prog -> Program
typeCheckProg (Prog []) = (Program [])
typeCheckProg (Prog stms) = (Program (fixStatements stms 0 []))

fixStatements :: [Stm] -> Int -> VarsMap -> [Statement]
fixStatements stms level vars = fmap (\x -> typeCheckStatement x level newVars) stms
    where
        newVars = vars ++ getVars stms level

getVars :: [Stm] -> Int -> VarsMap
getVars [] _ = []
getVars (x:xs) level = getVar x level ++ (getVars xs level) 


getVar :: Stm -> Int -> VarsMap
getVar (CreateVar (Name t) name _) level = newVars
    where
        newVars = [(level, t, name)]
getVar (Shrd (CreateVar (Name t) name _)) level = newVars
    where
        newVars = [(level, t, name)]
getVar _ _ = []

typeCheckStatement :: Stm -> Int -> VarsMap -> Statement
typeCheckStatement (IfStm cmp stms1 elseIfs stms2) level vars =
        (If (typeCheckCompare cmp level vars) -- If comparison
            (fmap (\x -> typeCheckStatement x newLevel ifVars) stms1)  -- If statements
            (fmap (\x -> typeCheckElseIf x newLevel vars) elseIfs)   -- elseIfs
            (fmap (\x -> typeCheckStatement x newLevel elseVars) stms2)) -- else statements
    where 
        newLevel = level + 1
        ifVars = vars ++ getVars stms1 newLevel
        elseVars = vars ++ getVars stms2 newLevel
typeCheckStatement (WhileLP cmp stms) level vars =
        (While (typeCheckCompare cmp level vars) (fmap (\x -> typeCheckStatement x newLevel newVars) stms))
    where 
        newLevel = level + 1
        newVars = vars ++ getVars stms newLevel
typeCheckStatement (ParallelT stms) level vars = (Parallel (fmap (\x -> typeCheckStatement x newLevel newVars) stms))
    where
        newLevel = level + 1
        newVars = vars ++ getVars stms newLevel 
typeCheckStatement (LckStart n) _ _ = (LockStart n)
typeCheckStatement (LckEnd n) _ _ = (LockEnd n)
typeCheckStatement (SeqThread stms) level vars = (SequentialThread (fmap (\x -> typeCheckStatement x newLevel newVars) stms))
    where 
        newLevel = level + 1
        newVars = vars ++ getVars stms newLevel
typeCheckStatement (AssignVal name expr) level vars | varExists && typeCorrect     = (Assign name (typeCheckExpr expr level vars))
                                                    | varExists && not typeCorrect = error ("Variable '" ++ name ++ "' is not of correct type")
                                                    | otherwise                    = error ("Variable '" ++ name ++ "' doesn't exists")
    where 
        var = filter (\x -> getName x == name) vars
        varExists = length var > 0
        typeCorrect = (getTypeOfExpr expr vars) == stringToType (getType (head var))
typeCheckStatement (CreateVar (Name t) name expr) level vars | varExists   = error ("Variable '" ++ show name ++  "' esists already!")
                                                             | correctType = CreateVariable t name (typeCheckExpr expr level newVars)
                                                             | otherwise   = error ("Assignment of variable '" ++ name ++ "' is of the incorrext type. It should be " ++ t ++ "!")
    where
        newVars = vars ++ [(level, t, name)]
        exprType = getTypeOfExpr expr vars
        boolType = typeOf True
        intType = typeOf (0 :: Integer)
        correctType = (exprType == boolType && t == "bool") || (exprType == intType && t == "int")
        varExists = length (filter (\x -> getName x == name) vars) > 0

typeCheckStatement (Prnt expr) level vars = (Print (typeCheckExpr expr level vars))
typeCheckStatement (Shrd stm) level vars = (Shared (typeCheckStatement stm level vars))

typeCheckElseIf :: (Cmp, [Stm]) -> Int -> [(Int, String, String)] -> (Comparison, [Statement])
typeCheckElseIf (cmp, stms) level vars = ((typeCheckCompare cmp level vars), (fmap (\x -> typeCheckStatement x newLevel newVars) stms))
    where 
        newLevel = level + 1
        newVars = vars ++ getVars stms newLevel

 
typeCheckExpr :: Expr -> Int -> [(Int, String, String)] -> Expression
typeCheckExpr (MyAdd expr1 expr2) level vars = (Addition (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars)) 
typeCheckExpr (Mult expr1 expr2) level vars = (Multiplication (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars)) 
typeCheckExpr (MySub expr1 expr2) level vars = (Subtraction (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars)) 
typeCheckExpr (IVal val) _ _ = (I val) 
typeCheckExpr (BVal (BoolVal val)) _ _ = (B val) 
typeCheckExpr (BVal cmp) level vars = (BComp (typeCheckCompare cmp level vars))
typeCheckExpr (Id name) _ _ = (Identifier name)

typeCheckCompare :: Cmp -> Int -> [(Int, String, String)] -> Comparison
typeCheckCompare (Sm expr1 expr2) level vars | type1 == type2 && type1 == typeInteger = (Smaller (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise                              = error "Type error in '<'"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (SE expr1 expr2) level vars | type1 == type2 && type1 == typeInteger = (SmallerEqual (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise                              = error "Type error in '<='"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (Bi expr1 expr2) level vars | type1 == type2 && type1 == typeInteger = (Bigger (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise                              = error "Type error in '>'"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (BE expr1 expr2) level vars | type1 == type2 && type1 == typeInteger = (BiggerEqual (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise                              = error "Type error in '>='"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (MyEq expr1 expr2) level vars | type1 == type2 = (Eq (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise      = error "Type error in '=='"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (NE expr1 expr2) level vars | type1 == type2 = (NotEqual (typeCheckExpr expr1 level vars) (typeCheckExpr expr2 level vars))
                                             | otherwise      = error "Type error in '=='"
    where
        typeInteger = typeOf (0 :: Integer)
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
typeCheckCompare (A cmp1 cmp2) level vars = (AndOp (typeCheckCompare cmp1 level vars) (typeCheckCompare cmp2 level vars))
typeCheckCompare (O cmp1 cmp2) level vars = (OrOp (typeCheckCompare cmp1 level vars) (typeCheckCompare cmp2 level vars))
typeCheckCompare (BoolVal b) _ _ = Boolean b

getTypeOfExpr :: Expr -> VarsMap -> TypeRep
getTypeOfExpr (IVal val) _ = typeOf val
getTypeOfExpr (BVal _) _ = typeOf True
getTypeOfExpr (Id name) vars | length var > 0 = t
                             | otherwise      = error ("Variable '" ++ name ++ "' not found!")
    where
        var = filter (\x -> getName x == name) vars
        t = stringToType (getType (head var))
getTypeOfExpr (ListVals exprs) vars | allSameType = typeFirst
                                    | otherwise   =  error "Array doesn't have same type"
    where
        typeFirst = getTypeOfExpr (head exprs) vars
        types = fmap (\x -> getTypeOfExpr x vars) exprs
        allSameType = all (\x -> typeOf x == typeFirst) types
getTypeOfExpr (MyAdd expr1 expr2) vars | type1 == type2 && type1 == typeBool    = typeBool
                                       | type1 == type2 && type1 == typeInteger = typeInteger
                                       | otherwise                              = error "Type missmatch in addition!"
    where
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
        typeInteger = typeOf (0 :: Integer)
        typeBool = typeOf True
getTypeOfExpr (Mult expr1 expr2) vars | type1 == type2 && type1 == typeBool    = typeBool
                                      | type1 == type2 && type1 == typeInteger = typeInteger
                                      | otherwise                              = error "Type missmatch in multiplication!"
    where
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
        typeInteger = typeOf (0 :: Integer)
        typeBool = typeOf True
getTypeOfExpr (MySub expr1 expr2) vars | type1 == type2 && type1 == typeBool    = typeBool
                                     | type1 == type2 && type1 == typeInteger = typeInteger
                                     | otherwise                              = error "Type missmatch in subtraction!"
    where
        type1 = getTypeOfExpr expr1 vars
        type2 = getTypeOfExpr expr2 vars
        typeInteger = typeOf (0 :: Integer)
        typeBool = typeOf True

stringToType :: String -> TypeRep
stringToType "int"  = typeOf (0 :: Integer)
stringToType "bool" = typeOf True