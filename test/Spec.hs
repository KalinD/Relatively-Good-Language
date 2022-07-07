import MyParser
import MyTypeCheck
import Test.Hspec
import Test.QuickCheck

arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    -- Test to verify numbers are parsed correctly.
    it "Parses numbers" $ do
        property $ \n -> ((parser parseExpr) $ show n) `shouldBe` (IVal n)
    -- Test to verify booleans are parsed correctly.
    it "Parses booleans" $ do
        (parser parseExpr "true") `shouldBe` (BVal (BoolVal True))
        (parser parseExpr "false") `shouldBe` (BVal (BoolVal False))
    -- Test to verify variable assignments are parsed correctly.
    it "Parses variable declaration" $ do
      property $ \n -> (parser parseStm $ ("int x = " ++ show n)) `shouldBe` (CreateVar "int" "x" (IVal n))
    -- Test to verify identifiers are parsed correctly.
    -- it "Parse Id" $ do
    --   id = []
    --   property $ \id -> ((parser parseExpr) $ id) `shouldBe` (Id id)
    -- Test to verify additions are parsed correctly.
    it "Parses addition operator" $ do
        property $ \x y -> (parser parseExpr (show x ++ " + " ++ show y)) `shouldBe` (MyAdd (IVal x) (IVal y))
    -- Test to verify multiplications are parsed correctly.
    it "Parses multiplication operator" $ do
        property $ \x y -> (parser parseExpr (show x ++ " * " ++ show y)) `shouldBe` (Mult (IVal x) (IVal y))
    -- Test to verify substractions are parsed correctly.
    it "Parses subtraction operator" $ do
        property $ \x y -> (parser parseExpr (show x ++ " - " ++ show y)) `shouldBe` (MySub (IVal x) (IVal y))
  -- describe "TypeChecking" $ do
  --   it "should not assign int to bool" $ do
  --     property $ \n -> (typeCheckProg (parser parseProg ("bool b = " ++ show n))) `shouldThrow` anyException
  --   it "should not assign bool to int" $ do
  --     property $ (typeCheckProg (parser parseProg ("int i = true"))) `shouldThrow` anyException
  --     property $ (typeCheckProg (parser parseProg ("int i = false"))) `shouldThrow` anyException
      
