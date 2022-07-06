module MyParser where -- (parseMyLang) where

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

{- 
 First IR of the language. The file parses the given file into an IR without type checking or improving the
 parsed input.
 -}


-- From lab files
-- Returns the left side of an Either type
fromLeft' :: Either l r -> l
fromLeft' (Left x) = x

-- From lab files
-- Returns the right side of an Either type
fromRight' :: Either l r -> r
fromRight' (Right x) = x

-- From lab files
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

-- Language definition, specifies reserved names, operators, and symbols
languageDef =
  emptyDef {
    Token.reservedOpNames = ["(" ,")", "{", "}", "-", "+", "*", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||"],
    Token.reservedNames = ["whatIf", "butWhatIf", "else", "pleaseDoWhile",
        "allAtOnce", "hangingByAThread", "shared", "int", "bool", "aFewOf", "true", "false", "print", "doNotTouchThis", "nowYouCanTouchThis"], -- true and false might be redundant in here
    Token.caseSensitive = True,
    Token.commentStart = "%-",
    Token.commentEnd = "-%",
    Token.commentLine = "%",
    Token.nestedComments = False
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


-- EDSL for Relatively-Good-Language, to create an AST from a given source code input
data Prog = Prog [Stm] deriving Show

               -- if (cmp)     { stms }     else if (cmp) { stms }     else { stms } 
data Stm = IfStm Cmp [Stm] [(Cmp, [Stm])] [Stm]
         | WhileLP Cmp [Stm]
         | ParallelT [Stm]
         | LckStart Integer
         | LckEnd Integer
         | SeqThread [Stm]
         | AssignVal String Expr
         | CreateVar Type String Expr
         | Prnt Expr
         | Shrd Stm
    deriving Show

data Type = Name String
          | List Type
    deriving Show

data Expr = MyAdd  Expr Expr  -- TODO: Optional to +, -, * of booleans
          | MySub  Expr Expr
          | Mult Expr Expr
          | ListVals [Expr]
          | IVal Integer
          | BVal Cmp
          | Id String
    deriving Show

{- Compare type to handle comparison. Can handle 
 - less than (<), less or equal (<=), larger than (>), larger or equal(>=), and equals (==) comparisons.
 - The constructors are in the same order as above.
 -}
data Cmp = Sm Expr Expr
         | SE Expr Expr  -- Extra
         | Bi Expr Expr
         | BE Expr Expr  -- Extra
         | MyEq Expr Expr
         | NE Expr Expr
         | BoolVal Bool
         | A Cmp Cmp
         | O Cmp Cmp
    deriving Show

-- Parses a source code into a type Prog with a list of Stm
parseProg :: Parser Prog
parseProg = Prog <$> (many parseStm)

-- For Assign try to parse a nl or ; at the end. If it is not the end, try to parse again.
parseStm :: Parser Stm
parseStm =  try (AssignVal <$> identifier <*> (symbol "=" *> parseExpr))
        <|> CreateVar <$> parseType <*> (identifier <* symbol "=") <*> parseExpr
        <|> WhileLP <$> (reserved "pleaseDoWhile" *> (parens parseCmp)) <*> braces (many parseStm)
        <|> ParallelT <$> (reserved "allAtOnce" *> braces (many parseStm))
        <|> SeqThread <$> (reserved "hangingByAThread" *> braces (many parseStm))
        <|> try (IfStm <$> (reserved "whatIf" *> (parens parseCmp)) <*> (braces (many parseStm)) <*> (many ((,) <$> (reserved "butWhatIf" *> (parens parseCmp)) <*> (braces (many parseStm)))) <*> ((reserved "else" *> braces (many parseStm))))
        <|> try (IfStm <$> (reserved "whatIf" *> (parens parseCmp)) <*> (braces (many parseStm)) <*> (many ((,) <$> (reserved "butWhatIf" *> (parens parseCmp)) <*> (braces (many parseStm)))) <*> pure [])
        <|> IfStm <$> (reserved "whatIf" *> (parens parseCmp)) <*> (braces (many parseStm)) <*> pure [] <*> pure []
        <|> Prnt <$> (reserved "print" *> parens (parseExpr))
        <|> LckStart <$> (reserved "doNotTouchThis" *> (parens integer))
        <|> LckEnd <$> (reserved "nowYouCanTouchThis" *> (parens integer))
        <|> Shrd <$> (reserved "shared" *> parseStm)

parseType :: Parser Type
parseType =  (Name <$> symbol "int")
         <|> (Name <$> symbol "bool")
         <|> (List <$> (reserved "aFewOf" *> (brackets parseType)))

-- Parser for expression
parseExpr :: Parser Expr
parseExpr = parseExprHelp1 `chainr1` ((\_ -> MyAdd) <$> reserved "+") 

-- Helper functions to parse expression
parseExprHelp1 :: Parser Expr
parseExprHelp1 = parseExprHelp2 `chainr1` ((\_ -> MySub) <$> reserved "-")

parseExprHelp2 :: Parser Expr
parseExprHelp2 = parseExprHelp3 `chainr1` ((\_ -> Mult) <$> reserved "*")

parseExprHelp3 :: Parser Expr
parseExprHelp3 =  (parens parseExpr)
              <|> try (BVal <$> parseCmp)
              <|> (parseValue)

-- Parser for value
parseValue :: Parser Expr
parseValue = (IVal <$> integer)
          <|> try (Id <$> identifier)
          <|> (ListVals <$> brackets (comSep parseValue))
          <|> (BVal <$> parseCmp)
          
-- Parser for comparison
parseCmp :: Parser Cmp
parseCmp = parseCmpHelp1 `chainr1` ((\_ -> O) <$> reserved "||")

-- Helper functions to parse comparison
parseCmpHelp1 :: Parser Cmp
parseCmpHelp1 = parseCmpHelp2 `chainr1` ((\_ -> A) <$> reserved "&&")

parseCmpHelp2 :: Parser Cmp
parseCmpHelp2 =  try (BoolVal <$> (stringToBool <$> (symbol "true" <|> symbol "false")))
             <|> try (Sm <$> (parseValue <* reserved "<")  <*> parseValue)
             <|> try (SE <$> (parseValue <* reserved "<=") <*> parseValue)
             <|> try (Bi <$> (parseValue <* reserved ">")  <*> parseValue)
             <|> try (BE <$> (parseValue <* reserved ">=") <*> parseValue)
             <|> try (MyEq <$> (parseValue <* reserved "==") <*> parseValue)
             <|> try (NE <$> (parseValue <* reserved "!=") <*> parseValue)

-- Converts string representation of a boolean in RGL into Haskell's Bool type
stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False
stringToBool a = error ("Can't convert '" ++ a ++ "' to boolean")