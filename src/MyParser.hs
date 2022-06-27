module MyParser (parseMyLang) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char 
import Text.Parsec.Combinator
import Text.Parsec.Token (commaSep, commaSep1)
-- import Text.ParserCombinators.Parsec.Token (commaSep)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Arrow

import Data.Either

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x

fromRight' :: Either l r -> r
fromRight' (Right x) = x

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
            | otherwise  = fromRight' res
    where res = parse p "" xs

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseNumUntilEnd :: String -> Either ParseError Integer
parseNumUntilEnd = parse (num <* eof) ""

parseMyLang s = left show $ parseNumUntilEnd s

languageDef =
  emptyDef {
    Token.reservedOpNames = ["(" ,")", "{", "}", "+", "*", "=", "==", "<", ">", "<=", ">=", "&&", "||"],
    Token.reservedNames = ["whatIf", "butWhatIf", "else", "pleaseDoWhile",
        "allAtOnce", "hangingByAThread", "shared", "int", "bool", "aFewOf", "true", "false", "print", "doNotTouchThis", "nowYouCanTouchThis"], -- true and false might be redundant in here
    Token.caseSensitive = True,
    Token.commentStart = "%-",
    Token.commentEnd = "-%",
    Token.commentLine = "%"
  }


lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
brackets   = Token.brackets lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer
comSep     = commaSep lexer


data Prog = Prog [Statement] deriving Show

               -- if (cmp)     { stms }     else if (cmp) { stms }     else { stms } 
data Statement = If Comparison [Statement] [(Comparison, [Statement])] [Statement]
               | While Comparison [Statement]
               | Parallel [Statement]
               | LockStart Integer
               | LockEnd Integer
               | SequentialThread [Statement]
               | Assign String Expr
               | AssignArr String Integer Expr
               | CreateVar Type String Expr
               | Print Expr
               | Shared Statement
    deriving Show

data Type = Name String
          | List Type
    deriving Show

data Expr = Add  Expr Expr  -- TODO: Optional to +, -, * of booleans
          | Sub  Expr Expr
          | Mult Expr Expr
          | ListVals [Expr]
          | IVal Integer
          | BVal Comparison
          | ArrVal String Integer
          | Id String
    deriving Show

{- Compare type to handle comparisons. Can handle 
 - less than (<), less or equal (<=), larger than (>), larger or equal(>=), and equals (==) comparisons.
 - The constructors are in the same order as above.
 -}
data Comparison = S  Expr Expr
             | SE Expr Expr  -- Extra
             | B  Expr Expr
             | BE Expr Expr  -- Extra
             | Eq Expr Expr
             | BoolVal Bool
             | And Comparison Comparison
             | Or Comparison Comparison
    deriving Show


parseProg :: Parser Prog
parseProg = Prog <$> (many parseStatement)

-- For Assign try to parse a nl or ; at the end. If it is not the end, try to parse again.
parseStatement :: Parser Statement
parseStatement =  try (Assign <$> identifier <*> (symbol "=" *> parseExpr))
              <|> try (AssignArr <$> identifier <*> (brackets integer) <*> (symbol "=" *> parseExpr))
              <|> CreateVar <$> parseType <*> (identifier <* symbol "=") <*> parseExpr
              <|> While <$> (reserved "pleaseDoWhile" *> (parens parseComparison)) <*> braces (many parseStatement)
              <|> Parallel <$> (reserved "allAtOnce" *> braces (many parseStatement))
              <|> SequentialThread <$> (reserved "hangingByAThread" *> braces (many parseStatement))
              <|> If <$> (reserved "whatIf" *> (parens parseComparison)) <*> (braces (many parseStatement)) <*> (many ((,) <$> (reserved "butWhatIf" *> (parens parseComparison)) <*> (brackets (many parseStatement)))) <*> ((reserved "else" *> brackets (many parseStatement)))
              <|> If <$> (reserved "whatIf" *> (parens parseComparison)) <*> (braces (many parseStatement)) <*> (many ((,) <$> (reserved "butWhatIf" *> (parens parseComparison)) <*> (brackets (many parseStatement)))) <*> pure []
              <|> Print <$> (reserved "print" *> parens (parseExpr))
              <|> LockStart <$> (reserved "doNotTouchThis" *> (parens integer))
              <|> LockEnd <$> (reserved "nowYouCanTouchThis" *> (parens integer))
              <|> Shared <$> (reserved "shared" *> parseStatement)

parseType :: Parser Type
parseType =  (Name <$> symbol "int")
         <|> (Name <$> symbol "bool")
         <|> (List <$> (reserved "aFewOf" *> (brackets parseType)))

parseExpr :: Parser Expr
parseExpr =  parseExprHelp1 `chainr1` ((\_ -> Sub) <$> reserved "-") -- TODO: to reservedOP

parseExprHelp1 :: Parser Expr
parseExprHelp1 = parseExprHelp2 `chainr1` ((\_ -> Add) <$> reserved "+")

parseExprHelp2 :: Parser Expr
parseExprHelp2 = parseExprHelp3 `chainr1` ((\_ -> Mult) <$> reserved "*")

parseExprHelp3 :: Parser Expr
parseExprHelp3 =  (parens parseExpr)
              <|> try (BVal <$> parseComparison)
              <|> (parseValue)
    
parseValue :: Parser Expr
parseValue = (IVal <$> integer)
          <|> try (ArrVal <$> identifier <*> (brackets integer))
          <|> (Id <$> identifier)
          <|> (ListVals <$> brackets (comSep parseValue))
          <|> (BVal <$> parseComparison)
          
parseComparison :: Parser Comparison
parseComparison = parseComparisonHelp1 `chainr1` ((\_ -> Or) <$> reserved "||")

parseComparisonHelp1 :: Parser Comparison
parseComparisonHelp1 = parseComparisonHelp2 `chainr1` ((\_ -> And) <$> reserved "&&")

parseComparisonHelp2 :: Parser Comparison
parseComparisonHelp2 =  try (BoolVal <$> (stringToBool <$> (symbol "true" <|> symbol "false")))
                    <|> try (S  <$> (parseValue <* reserved "<")  <*> parseValue)
                    <|> try (SE <$> (parseValue <* reserved "<=") <*> parseValue)
                    <|> try (B  <$> (parseValue <* reserved ">")  <*> parseValue)
                    <|> try (BE <$> (parseValue <* reserved ">=") <*> parseValue)
                    <|> try (Eq <$> (parseValue <* reserved "==") <*> parseValue)

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False
stringToBool a = error ("Can't convert '" ++ a ++ "' to boolean")