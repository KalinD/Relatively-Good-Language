module MyTypeCheck (typeCheck) where

import MyParser as Parser

data Program = Program [Statement]

data Statement = If Comparison [Statement] [(Comparison, [Statement])] [Statement]
               | While Comparison [Statement]
               | Parallel [Statement]
               | LockStart Integer
               | LockEnd Integer
               | SequentialThread [Statement]
               | Assign String Expression
               | AssignArr String Expression Expression
               | CreateVariable Type String Expression
               | Print Expression
               | Shared Statement
    deriving Show

data Expression = Addition       Expression Expression  -- TODO: Optional to +, -, * of booleans
                | Subtraction    Expression Expression
                | Multiplication Expression Expression
                | Shared Expression
                | I  String Integer    -- Integer
                | B  String Bool       -- Boolean
                | LI String [Integer]  -- List of Integers
                | LB String [Bool]     -- List of Booleans
                | LL String [Vars]     -- List of Lists
            deriving Show
    deriving Show

-- scope = [] :: [(Int, Expression)]
getName (_, (I name _)) = name
getName (_, (B name _)) = name
getLevel = fst
getName (_, (I _ val)) = val
getName (_, (B _ val)) = val

reduceProg :: Prog -> Program
reduceProg (Prog []) = (Prog [])
reduceProg (Prog (x:xs)) = (Program (parseStatement 0 [] x):(reduceProg (Prog xs)))
    where
        newScope = getVars x

reduceStatement :: Statement -> Int ->  [(String, Int, T)] -> Statement
reduceStatement (IfStm cmp stms1 elseIfs stms2) = (If (reduceCompare cmp) (reduceStatement stms1) (fmap reduceElseIf elseIfs) (reduceStatement stms2)) 
reduceStatement (WhileLP cmp smts) = (While (reduceCompare cmp) (fmap reduceStatement stms))
reduceStatement (ParallelT stms) = (Parallel (fmap reduceStatement stms)) 
reduceStatement (LckStart n) = (LockStart n)
reduceStatement (LckEnd n) = (LockEnd n)
reduceStatement (SeqThread stms) = (SequentialThread (fmap reduceStatement stms))
reduceStatement (AssignVal name expr) = (Assign name (reduceExpr expr))
reduceStatement (AssignArrVal name expr1 expr2) = (Assign name (reduceExpr expr1) (reduceExpr expr2))

reduceStatement (CreateVar t name expr) = (CreateVariable t name (reduceExpr expr))

reduceStatement (Prnt expr) = (Print (reduceExpr expr))
reduceStatement (Shrd stm) = (Shared (reduceStatement stm))

reduceExpr :: Expr -> Expression
reduceExpr (Add expr1 expr2) = (Addition (reduceExpr expr1) (reduceExpr expr2)) 
    --       | Sub  Expr Expr
    --       | Mult Expr Expr
    --       | ListVals [Expr]
    --       | IVal Integer
    --       | BVal Comparison
    --       | ArrVal String Expr
    --       | Id String
    -- deriving Show

reduceList :: Expr -> String -> Vars
reduceList (ListVals []) name = (LL name []) 
reduceList (ListVals ((IVal x):[])) name = (LI name [x]) 
reduceList (ListVals ((BVal (BoolVal x)):[])) name = (LB name [x]) 
reduceList (ListVals ((IVal x):(BVal xs):xss)) _  = error "List not of the same type"
reduceList (ListVals ((BVal x):(IVal xs):xss)) _  = error "List not of the same type"
reduceList (ListVals ((BVal (BoolVal x)):xs:xss)) name = (LB name x) ++ (reduceExpr (ListVals [xs:xss]) name)
reduceList (ListVals ((IVal x):xs:xss)) name = (LI name x) ++ (reduceExpr (ListVals [xs:xss]) name)

-- reduceCompare (S expr1 expr2) =
-- reduceCompare (SE expr1 expr2) =
-- reduceCompare (B expr1 expr2) =
-- reduceCompare (BE expr1 expr2) =
-- reduceCompare (Eq expr1 expr2) =
-- reduceCompare (BoolVal b) =
-- reduceCompare (And comp1 comp2) = 
-- reduceCompare (Or comp1 copm2) = 