import MyParser
import MyTypeCheck
import MyCodeGen hiding (main)
import Sprockell
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
    describe "Program TypeCheck" $ do
        it "Parallel program example" $ do
            file <- readFile "./examples/parallel.rgl"
            (typeCheckProg (parser parseProg file)) `shouldBe` (Program [
                (Shared (CreateVariable "int" "i" (I 0))),
                (Parallel [
                    (SequentialThread [
                        (LockStart 2),
                        (Assign "i" (Addition (Identifier "i") (I 1))),
                        (Print (Subtraction (I 100) (Identifier "i"))),
                        (LockEnd 2),
                        (Parallel [
                            (SequentialThread [
                                (Print (Addition (Identifier "i") (I 2)))
                            ]),
                            (SequentialThread [
                                (Print (Addition (Identifier "i") (I 1)))
                            ])
                        ])
                    ]),
                    (SequentialThread [
                        (LockStart 2),
                        (Assign "i" (Addition (Identifier "i") (I 1))),
                        (Print (Identifier "i")),
                        (LockEnd 2)
                    ])
                ]),
                (Print (Identifier "i"))
                ])
    describe "Sprockell Code generation for Expressions" $ do
        it "Code generation for Integer" $ do
            property $ \n -> (exprGen (I n) regA 0 [])  `shouldBe` ([(Load (ImmValue (fromIntegral n)) regA)], 0, [])
        it "Code generation for Boolean" $ do
            (exprGen (B True) regA 0 [])  `shouldBe` ([(Load (ImmValue (fromIntegral 1)) regA)], 0, [])
            (exprGen (B False) regA 0 [])  `shouldBe` ([(Load (ImmValue (fromIntegral 0)) regA)], 0, [])
        it "Code generation for variable" $ do
            -- Variable "i" that is an int, it is not in shared memory, it is on scope level 0, and it is in memory 0
            (exprGen (Identifier "i") regA 0 [("i", "int", False, 0, 0)]) `shouldBe` ([(Load (DirAddr 0) regA)], 0, [("i", "int", False, 0, 0)])
            -- Variable "b" that is a boolean, it is in shared memory, it is on scope level 0, and it is in memory 0
            (exprGen (Identifier "b") regA 0 [("b", "bool", True, 0, 0)]) `shouldBe` ([(ReadInstr (DirAddr 0)), Receive regA], 0, [("b", "bool", True, 0, 0)])
        -- Code generation for arithmetic operations
        it "Code generation for addition" $ do
            property $ \x y -> (exprGen (Addition (I x) (I y)) regA 0 []) `shouldBe` ([(Load (ImmValue (fromIntegral x)) regA), (Load (ImmValue (fromIntegral y)) regB), (Compute Add regA regB regA)], 0, [])
        it "Code generation for subtraction" $ do
            property $ \x y -> (exprGen (Subtraction (I x) (I y)) regA 0 []) `shouldBe` ([(Load (ImmValue (fromIntegral x)) regA), (Load (ImmValue (fromIntegral y)) regB), (Compute Sub regA regB regA)], 0, [])
        it "Code generation for multiplication" $ do
            property $ \x y -> (exprGen (Multiplication (I x) (I y)) regA 0 []) `shouldBe` ([(Load (ImmValue (fromIntegral x)) regA), (Load (ImmValue (fromIntegral y)) regB), (Compute Mul regA regB regA)], 0, [])
    describe "Sprockell Code generation for Statements" $ do
        it "Code generation for new integer variables" $ do
            -- Create variable "i" in address 0
            property $ \n -> (stmGen (CreateVariable "int" "i" (I n)) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral n)) regA), (Store regA (DirAddr 0))]], 1, [("i", "int", False, 0, 0)], []) 
        it "Code generation for new boolean variables" $ do
            (stmGen (CreateVariable "bool" "b" (B True)) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 1)) regA), (Store regA (DirAddr 0))]], 1, [("b", "bool", False, 0, 0)], [])
            (stmGen (CreateVariable "bool" "b" (B False)) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (Store regA (DirAddr 0))]], 1, [("b", "bool", False, 0, 0)], [])
        -- Shared variables are stored in shared memory
        it "Code generation for creation of shared integer values" $ do
            -- Create a shared variable "i" of type integer.
            property $ \n -> (stmGen (Shared (CreateVariable "int" "i" (I n))) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral n)) regA), (WriteInstr regA (DirAddr 8))]], 0, [("i", "int", True, 0, 8)], []) 
        it "Code generation for creation of shared boolean values" $ do
            -- Create a shared variable "b" of type boolean. 
            (stmGen (Shared (CreateVariable "bool" "b" (B True))) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 1)) regA), (WriteInstr regA (DirAddr 8))]], 0, [("b", "bool", True, 0, 8)], [])
            -- Create a shared variable "b" of type boolean when there already exists a boolean value called "a".
            (stmGen (Shared (CreateVariable "bool" "b" (B False))) 0 [("a", "bool", True, 0, 8)] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (WriteInstr regA (DirAddr 9))]], 0, [("a", "bool", True, 0, 8), ("b", "bool", True, 0, 9)], [])
        it "Code generation for assignment of a new value for an integer variable" $ do
            -- Changing the value of the variable "i" that is in memory address 0, is of type integer and it is not in shared memory.
            property $ \n -> (stmGen (Assign "i" (I n)) 0 [("i", "int", False, 0, 0)] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral n)) regA), (Store regA (DirAddr 0))]], 0, [("i", "int", False, 0, 0)], [])
        it "Code generation for assignment of a new value for a boolean variable" $ do
            -- Changing the value of a boolean that is saved in local memory.
            (stmGen (Assign "b" (B True)) 0 [("b", "bool", False, 0, 0)] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 1)) regA), (Store regA (DirAddr 0))]], 0, [("b", "bool", False, 0, 0)], [])
            -- Changing the value of a boolean that is saved in shared memory.
            (stmGen (Assign "b" (B False)) 0 [("b", "bool", True, 0, 8)] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (WriteInstr regA (DirAddr 8))]], 0, [("b", "bool", True, 0, 8)], [])
        it "Code generation for printing" $ do
            (stmGen (Print (I 10)) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO)]], 0, [], [])
        it "Code generation for a while loop" $ do
            -- Loop is done by computein the value in comparison while it reutrns 1 the branching fails, so the jump returns it to the start.
            -- If the value is 0 the branching jumps over all instructions in the loop and the jump so that te code continues.
            (stmGen (While (Boolean True) [(Print (I 10))]) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 1)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 4)), (Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO), (Jump (Rel (-5)))]], 0, [], [])
        it "Code generation for an 'if' statement" $ do
            (stmGen (If (Boolean True) [(Print (I 10))] [] []) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 1)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 3)), (Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO)]], 0, [], [])  
            (stmGen (If (Boolean False) [(Print (I 10))] [] [(Print (I 5))]) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 4)), (Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO), (Jump (Rel 3)), (Load (ImmValue (fromIntegral 5)) regA), (WriteInstr regA numberIO)]], 0, [], [])  
            (stmGen (If (Boolean False) [(Print (I 10))] [((Boolean True), [(Print (I 5))])] []) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 4)), (Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO), (Jump (Rel 6)), (Load (ImmValue (fromIntegral 1)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 3)), (Load (ImmValue (fromIntegral 5)) regA), (WriteInstr regA numberIO)]], 0, [], [])  
            (stmGen (If (Boolean False) [(Print (I 10))] [((Boolean False), [(Print (I 5))])] [(Print (I 1))]) 0 [] [] []) `shouldBe` ([[(Load (ImmValue (fromIntegral 0)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 4)), (Load (ImmValue (fromIntegral 10)) regA), (WriteInstr regA numberIO), (Jump (Rel 9)), (Load (ImmValue (fromIntegral 0)) regA), (Compute Equal reg0 regA regA), (Branch regA (Rel 4)), (Load (ImmValue (fromIntegral 5)) regA), (WriteInstr regA numberIO), (Jump (Rel 3)), (Load (ImmValue (fromIntegral 1)) regA), (WriteInstr regA numberIO)]], 0, [], [])  
        it "Code generation for locking" $ do
            -- Creating a new Lock. If it already exists wait for it to become available.
            property $ \n -> (stmGen (LockStart n) 0 [] [] []) `shouldBe` ([[(TestAndSet (DirAddr (fromIntegral n))), (Receive regA), (Compute NEq reg0 regA regA), (Branch regA (Rel 2)), (Jump (Rel (-4)))]], 0, [], [((fromIntegral n), True)])
        it "Code generation for unlocking" $ do
            -- Unlocking a lock that already exists.
            property $ \n -> (stmGen (LockEnd n) 0 [] [((fromIntegral n), True)] []) `shouldBe` ([[(WriteInstr reg0 (DirAddr (fromIntegral n)))]], 0, [], [])

{- Run to see the result of the fibonacci program written in RGL -}
testFib :: IO ()
testFib = do
    file <- "./examples/fib.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells

{- Run to see the result of the parallel program written in RGL. It demonstrates a simple program with multithreading. -}
testParallel :: IO ()
testParallel = do
    file <- "./examples/parallel.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells

{- Run to see the result of the banking program written in RGL. -}
testBanking :: IO ()
testBanking = do
    file <- "./examples/banking.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells

{- FAILING TESTS -}
{- The following test will throw errors due to incorrect parsing. -}
-- Declaration of an already existing variable
testVarExists :: IO ()
testVarExists = do
    file <- "./failingExamples/varExists.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells

-- Assigning a value of a variable that doesn't exist
testVarNotEx :: IO ()
testVarNotEx = do
    file <- "./failingExamples/varNotEx.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells

-- Assignment of a variable using an incorrect value
testWrongType :: IO ()
testWrongType = do
    file <- "./failingExamples/wrongType.rgl"
    let sprockells = progGen (typeCheckProg (parser parseProg file))
    run sprockells