import MyParser
import MyTypeCheck
import Test.Hspec
import Test.QuickCheck

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
  -- describe "TypeChecking" $ do
  --   it "should not assign int to bool" $ do
  --     property $ \n -> (typeCheckProg (parser parseProg ("bool b = " ++ show n))) `shouldThrow` anyException
  --   it "should not assign bool to int" $ do
  --     property $ (typeCheckProg (parser parseProg ("int i = true"))) `shouldThrow` anyException
  --     property $ (typeCheckProg (parser parseProg ("int i = false"))) `shouldThrow` anyException
      
