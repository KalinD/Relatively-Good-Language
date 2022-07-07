import MyParser
import MyTypeCheck
import Test.Hspec
import Test.QuickCheck
import Data.Typeable (typeOf)

arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

main :: IO ()
main = hspec $ do
    describe "Parsing Expressions" $ do
        -- Test to verify numbers are parsed correctly.
        it "Parses numbers" $ do
            property $ \n -> (parser parseExpr (show n)) `shouldBe` (IVal n)
        -- Test to verify booleans are parsed correctly.
        it "Parses booleans" $ do
            (parser parseExpr "true") `shouldBe` (BVal (BoolVal True))
            (parser parseExpr "false") `shouldBe` (BVal (BoolVal False))
        -- Test to verify additions are parsed correctly.
        it "Parses addition operator" $ do
            property $ \x y -> (parser parseExpr (show x ++ " + " ++ show y)) `shouldBe` (MyAdd (IVal x) (IVal y))
        -- Test to verify multiplications are parsed correctly.
        it "Parses multiplication operator" $ do
            property $ \x y -> (parser parseExpr (show x ++ " * " ++ show y)) `shouldBe` (Mult (IVal x) (IVal y))
        -- Test to verify substractions are parsed correctly.
        it "Parses subtraction operator" $ do
            property $ \x y -> (parser parseExpr (show x ++ " - " ++ show y)) `shouldBe` (MySub (IVal x) (IVal y))
        -- Test priority of arithmetic operations
        it "Parses priority of operations" $ do
            property $ \w x y z -> (parser parseExpr (show w ++ " + " ++ show x ++ " - " ++ show y ++ " * " ++ show z)) `shouldBe` (MySub (MyAdd (IVal w) (IVal x)) (Mult (IVal y) (IVal z)))
    describe "Parsing Comparisons" $ do
        -- Test comparison operations (>, >=, <, <=, ==, !=)
        it "Parses smaller than comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " < " ++ show y)) `shouldBe` (Sm (IVal x) (IVal y))
        it "Parses smaller or equal to comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " <= " ++ show y)) `shouldBe` (SE (IVal x) (IVal y))
        it "Parses larger than comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " > " ++ show y)) `shouldBe` (Bi (IVal x) (IVal y))
        it "Parses larger or equal to comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " >= " ++ show y)) `shouldBe` (BE (IVal x) (IVal y))
        it "Parses equality comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " == " ++ show y)) `shouldBe` (MyEq (IVal x) (IVal y))
        it "Parses different comparison" $ do
            property $ \x y -> (parser parseCmp (show x ++ " != " ++ show y)) `shouldBe` (NE (IVal x) (IVal y))
        -- Test && and || operators
        it "Parses && operator" $ do
            property $ \x y -> (parser parseCmp (show x ++ " == " ++ show y ++ " && " ++ show x ++ " != " ++ show y)) `shouldBe` (A (MyEq (IVal x) (IVal y)) (NE (IVal x) (IVal y)))
        it "Parses || operator" $ do
            property $ \x y -> (parser parseCmp (show x ++ " == " ++ show y ++ " || " ++ show x ++ " != " ++ show y)) `shouldBe` (O (MyEq (IVal x) (IVal y)) (NE (IVal x) (IVal y)))
        it "Priority between && and ||" $ do
            (parser parseCmp "true && false || false && true") `shouldBe` (O (A (BoolVal True) (BoolVal False)) (A (BoolVal False) (BoolVal True)))
            (parser parseCmp "true || false && false || true") `shouldBe` (O (BoolVal True) (O (A (BoolVal False) (BoolVal False)) (BoolVal True)))
    describe "Parsing Statements" $ do
        -- Test to verify variable creation are parsed correctly.
        it "Parses integer declaration" $ do
            property $ \n -> (parser parseStm ("int x = " ++ show n)) `shouldBe` (CreateVar "int" "x" (IVal n))
        it "Parses complex integer declaration" $ do
            property $ \x y -> (parser parseStm ("int x = " ++ show x ++ " + " ++ show y)) `shouldBe` (CreateVar "int" "x" (MyAdd (IVal x) (IVal y)))
        it "Parses boolean declaration" $ do
            (parser parseStm "bool b = true") `shouldBe` (CreateVar "bool" "b" (BVal (BoolVal True)))
            (parser parseStm "bool b = false") `shouldBe` (CreateVar "bool" "b" (BVal (BoolVal False)))
        it "Parses complex boolean declaration" $ do
            (parser parseStm "bool b = 1 > 0") `shouldBe` (CreateVar "bool" "b" (BVal (Bi (IVal 1) (IVal 0))))
            (parser parseStm "bool b = 10 <= 9") `shouldBe` (CreateVar "bool" "b" (BVal (SE (IVal 10) (IVal 9))))
        -- Test to verify shared variable creation are parsed correctly.
        it "Parses shared integer declaration" $ do
            property $ \n -> (parser parseStm ("shared int x = " ++ show n)) `shouldBe` (Shrd (CreateVar "int" "x" (IVal n)))
        it "Parses shared boolean declaration" $ do
            (parser parseStm "shared bool b = true") `shouldBe` (Shrd (CreateVar "bool" "b" (BVal (BoolVal True))))
            (parser parseStm "shared bool b = false") `shouldBe` (Shrd (CreateVar "bool" "b" (BVal (BoolVal False))))
        -- Test to verify variable assignments are parsed correctly.
        it "Parses integer assignment" $ do
            property $ \n -> (parser parseStm $ ("x = " ++ show n)) `shouldBe` (AssignVal "x" (IVal n))
        it "Parses boolean assignment" $ do
            (parser parseStm "b = true") `shouldBe` (AssignVal "b" (BVal (BoolVal True)))
            (parser parseStm "b = false") `shouldBe` (AssignVal "b" (BVal (BoolVal False)))
        it "Parses 'if' statements" $ do
            (parser parseStm "whatIf(x == 1){ }") `shouldBe` (IfStm (MyEq (Id "x") (IVal 1)) [] [] [])
            (parser parseStm "whatIf(x == 1){ } butWhatIf(x == 2){ }") `shouldBe` (IfStm (MyEq (Id "x") (IVal 1)) [] [((MyEq (Id "x") (IVal 2)), [])] [])
            (parser parseStm "whatIf(x == 1){ } butWhatIf(x == 2){ } else { x = 10 }") `shouldBe` (IfStm (MyEq (Id "x") (IVal 1)) [] [((MyEq (Id "x") (IVal 2)), [])] [(AssignVal "x" (IVal 10))])
            (parser parseStm "whatIf(x == 1){ } else { x = 10 }") `shouldBe` (IfStm (MyEq (Id "x") (IVal 1)) [] [] [(AssignVal "x" (IVal 10))])
        it "Parses 'while' loop" $ do
            (parser parseStm "pleaseDoWhile(i < 10){ i = i + 1}") `shouldBe` (WhileLP (Sm (Id "i") (IVal 10)) [(AssignVal "i" (MyAdd (Id "i") (IVal 1)))])
        it "Parses parallel block" $ do
            (parser parseStm "allAtOnce{ x = 10 }") `shouldBe` (ParallelT [AssignVal "x" (IVal 10)])
        it "Parses sequential block" $ do
            (parser parseStm "hangingByAThread{ x = 10 }") `shouldBe` (SeqThread [AssignVal "x" (IVal 10)])
        it "Parses lock" $ do
            property $ \n -> (parser parseStm ("doNotTouchThis(" ++ show n ++ ")")) `shouldBe` (LckStart n)
        it "Parses unlock" $ do
            property $ \n -> (parser parseStm ("nowYouCanTouchThis(" ++ show n ++ ")")) `shouldBe` (LckEnd n)
        it "Parses print" $ do
            (parser parseStm "print(i)") `shouldBe` (Prnt (Id "i"))
            (parser parseStm "print(i + 1)") `shouldBe` (Prnt (MyAdd (Id "i") (IVal 1)))
            (parser parseStm "print(i == 10)") `shouldBe` (Prnt (BVal (MyEq (Id "i") (IVal 10))))
    describe "Parsing Programs" $ do
        {- Testing if the fibonacci program from "./examples/fib.rgl" is parsed properly.
        - The program prints the nth fibonacci number. -}
        it "Parsing the whole fibonacci program" $ do
            fibFile <- readFile "./examples/fib.rgl"
            (parser parseProg fibFile) `shouldBe` (Prog [
                CreateVar "int" "n" (IVal 10),
                CreateVar "int" "i" (IVal 0),
                CreateVar "int" "fst" (IVal 0),
                CreateVar "int" "snd" (IVal 1),
                WhileLP (Sm (Id "i") (Id "n")) [
                    (CreateVar "int" "temp" (Id "snd")),
                    (AssignVal "snd" (MyAdd (Id "fst") (Id "snd"))),
                    (AssignVal "fst" (Id "temp")),
                    (AssignVal "i" (MyAdd (Id "i") (IVal 1)))
                ],
                (Prnt (Id "fst"))])

    describe "TypeChecking - getting the tpye from Expr" $ do
        it "Checking the type of integer" $ do 
            property $ \n -> (getTypeOfExpr (IVal n) []) `shouldBe` (typeOf (0 :: Integer))
        it "Checking the type of booleans" $ do
            (getTypeOfExpr (BVal (BoolVal True)) []) `shouldBe` (typeOf True)
            (getTypeOfExpr (BVal (BoolVal False)) []) `shouldBe` (typeOf True)
        it "Checking the type of variable from the vartable" $ do
            (getTypeOfExpr (Id "x") [(0, "int", "x"), (0, "bool", "b")]) `shouldBe` (typeOf (0 :: Integer))
            (getTypeOfExpr (Id "b") [(0, "int", "x"), (0, "bool", "b")]) `shouldBe` (typeOf True)
        it "Type of addition" $ do
            property $ \x y -> (getTypeOfExpr (MyAdd (IVal x) (IVal y)) []) `shouldBe` (typeOf (0 :: Integer))
        it "Type of multiplication" $ do
            property $ \x y -> (getTypeOfExpr (Mult (IVal x) (IVal y)) []) `shouldBe` (typeOf (0 :: Integer))
        it "Type of subtraction" $ do
            property $ \x y -> (getTypeOfExpr (MySub (IVal x) (IVal y)) []) `shouldBe` (typeOf (0 :: Integer))
        it "Type of arithmetic operations with variables" $ do
            (getTypeOfExpr (MyAdd (Id "x") (Id "y"))  [(0, "int", "x"), (0, "int", "y")]) `shouldBe` (typeOf (0 :: Integer))
            (getTypeOfExpr (Mult (Id "x") (Id "y"))  [(0, "int", "x"), (0, "int", "y")]) `shouldBe` (typeOf (0 :: Integer))
            (getTypeOfExpr (MySub (Id "x") (Id "y"))  [(0, "int", "x"), (0, "int", "y")]) `shouldBe` (typeOf (0 :: Integer))
    {- TypeChecking converts the IR to a more convenient one. The tests verify that conversion from
     - one IR to another is successful. -}
    describe "TypeChecking converting Expr to Expression" $ do
        it "Checking the type of Integer" $ do
            property $ \n -> (typeCheckExpr (IVal n) 0 []) `shouldBe` (I n)
        it "Checking the type of addition" $ do
            property $ \x y -> (typeCheckExpr (MyAdd (IVal x) (IVal y)) 0 []) `shouldBe` (Addition (I x) (I y))
        it "Checking the type of multiplication" $ do
            property $ \x y -> (typeCheckExpr (Mult (IVal x) (IVal y)) 0 []) `shouldBe` (Multiplication (I x) (I y))
        it "Checking the type of subtraction" $ do
            property $ \x y -> (typeCheckExpr (MySub (IVal x) (IVal y)) 0 []) `shouldBe` (Subtraction (I x) (I y))
        it "Checking the type of Boolean" $ do
            (typeCheckExpr (BVal (BoolVal True)) 0 []) `shouldBe` (B True)
            (typeCheckExpr (BVal (BoolVal False)) 0 []) `shouldBe` (B False)
    describe "TypeChecking converting Cmp to Comparison" $ do
        it "Converting <" $ do
            property $ \x y -> (typeCheckCompare (Sm (IVal x) (IVal y)) 0 []) `shouldBe` (Smaller (I x) (I y))
        it "Converting <=" $ do
            property $ \x y -> (typeCheckCompare (SE (IVal x) (IVal y)) 0 []) `shouldBe` (SmallerEqual (I x) (I y))
        it "Converting >" $ do
            property $ \x y -> (typeCheckCompare (Bi (IVal x) (IVal y)) 0 []) `shouldBe` (Bigger (I x) (I y))
        it "Converting >=" $ do
            property $ \x y -> (typeCheckCompare (BE (IVal x) (IVal y)) 0 []) `shouldBe` (BiggerEqual (I x) (I y))
        it "Converting ==" $ do
            property $ \x y -> (typeCheckCompare (MyEq (IVal x) (IVal y)) 0 []) `shouldBe` (Eq (I x) (I y))
        it "Converting !=" $ do
            property $ \x y -> (typeCheckCompare (NE (IVal x) (IVal y)) 0 []) `shouldBe` (NotEqual (I x) (I y))
        it "Converting && and ||" $ do
            (typeCheckCompare (A (BoolVal True) (BoolVal False)) 0 []) `shouldBe` (AndOp (Boolean True) (Boolean False))
            (typeCheckCompare (O (BoolVal True) (BoolVal False)) 0 []) `shouldBe` (OrOp (Boolean True) (Boolean False))
    describe "TypeChecking converting Stm to Statement" $ do
        it "Asignment Integers" $ do
            property $ \n -> (typeCheckStatement (AssignVal "i" (IVal n)) 0 [(0, "int", "i")]) `shouldBe` (Assign "i" (I n))
        it "Asignment Booleans" $ do
            (typeCheckStatement (AssignVal "b" (BVal (BoolVal True))) 0 [(0, "bool", "b")]) `shouldBe` (Assign "b" (B True))
            (typeCheckStatement (AssignVal "b" (BVal (BoolVal False))) 0 [(0, "bool", "b")]) `shouldBe` (Assign "b" (B False))
        it "Creating Integers" $ do
            property $ \n -> (typeCheckStatement (CreateVar "int" "i" (IVal n)) 0 []) `shouldBe` (CreateVariable "int" "i" (I n))
        it "Creating Booleans" $ do
            (typeCheckStatement (CreateVar "bool" "b" (BVal (BoolVal True))) 0 []) `shouldBe` (CreateVariable "bool" "b" (B True))
            (typeCheckStatement (CreateVar "bool" "b" (BVal (BoolVal False))) 0 []) `shouldBe` (CreateVariable "bool" "b" (B False))
        it "If statements" $ do
            (typeCheckStatement (IfStm (BoolVal True) [] [] []) 0 []) `shouldBe` (If (Boolean True) [] [] [])
            (typeCheckStatement (IfStm (BoolVal False) [] [(BoolVal True, [AssignVal "x" (IVal 5)])] []) 0 [(0, "int", "x")]) `shouldBe` (If (Boolean False) [] [(Boolean True, [Assign "x" (I 5)])] [])
        it "While loop" $ do
            (typeCheckStatement (WhileLP (BoolVal True) []) 0 []) `shouldBe` (While (Boolean True) [])
            (typeCheckStatement (WhileLP (BoolVal True) [(AssignVal "x" (IVal 10))]) 0 [(0, "int", "x")]) `shouldBe` (While (Boolean True) [(Assign "x" (I 10))])
        it "Parallel Block" $ do
            (typeCheckStatement (ParallelT []) 0 []) `shouldBe` (Parallel [])
            (typeCheckStatement (ParallelT [(AssignVal "x" (IVal 10))]) 0 [(0, "int", "x")]) `shouldBe` (Parallel [(Assign "x" (I 10))])
        it "Sequential Block" $ do
            (typeCheckStatement (SeqThread []) 0 []) `shouldBe` (SequentialThread [])
            (typeCheckStatement (SeqThread [(AssignVal "x" (IVal 10))]) 0 [(0, "int", "x")]) `shouldBe` (SequentialThread [(Assign "x" (I 10))])
        it "Locks" $ do
            (typeCheckStatement (LckStart 10) 0 []) `shouldBe` (LockStart 10)
            (typeCheckStatement (LckEnd 10) 0 []) `shouldBe` (LockEnd 10)
        it "Print" $ do
            (typeCheckStatement (Prnt (IVal 1)) 0 []) `shouldBe` (Print (I 1))
            (typeCheckStatement (Prnt (Id "i")) 0 [(0, "int", "i")]) `shouldBe` (Print (Identifier "i"))