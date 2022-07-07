module MyParser where

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

{- First IR of the language. The module parses a given string into an IR without type checking or improving the parsed input. -}

-- From lab files
fromLeft' :: Either l r -> l
fromLeft' (Left x) = x

-- From lab files
fromRight' :: Either l r -> r
fromRight' (Right x) = x

-- From lab files
parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
            | otherwise  = fromRight' res
    where res = parse p "" xs

-- Language definition, specifying reserved operations, names, and comment syntax
languageDef =
  emptyDef {
    Token.reservedOpNames = ["(" ,")", "{", "}",                            -- Brackets and parenthesis
                             "-", "+", "*", "=",                            -- Arithmetic operations
                             "==", "!=", "<", ">", "<=", ">=", "&&", "||"], -- Boolean operations
    Token.reservedNames = ["whatIf", "butWhatIf", "else",             -- if/else if/ else statements
                           "pleaseDoWhile",                           -- while loop
                           "allAtOnce", "hangingByAThread", "shared", -- Multithreading definitions
                           "doNotTouchThis", "nowYouCanTouchThis",    -- Locks
                           "int", "bool", "true", "false",            -- Types and values
                           "print"],                                  -- Print
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

{- EDSL for Relatively Good Language, a program is of type Prog with a list of Stm. -}
data Prog = Prog [Stm] deriving Show

               
data Stm = IfStm Cmp [Stm] [(Cmp, [Stm])] [Stm] -- whatIf (cmp) { (stm)+ } (butWhatIf (cmp) { (stm)+ })* (else { stm })?
         | WhileLP Cmp [Stm]                    -- pleaseDoWhile (cmp) { (stm)+ }
         | ParallelT [Stm]                      -- allAtOnce { (stm)+ }
         | LckStart Integer                     -- doNotTouchThis (int)
         | LckEnd Integer                       -- nowYouCanTouchThis (int)
         | SeqThread [Stm]                      -- hangingByAThread { (stm)+ }
         | AssignVal String Expr                -- str = expr
         | CreateVar String String Expr         -- type str = expr
         | Prnt Expr                            -- print(expr)
         | Shrd Stm                             -- shared stm 
    deriving (Show, Eq)

data Expr = MyAdd  Expr Expr  -- expr + expr
          | MySub  Expr Expr  -- expr - expr
          | Mult Expr Expr    -- expr * expr
          | IVal Integer      -- int
          | BVal Cmp          -- cmp
          | Id String         -- identifier
    deriving (Show, Eq)

{- Compare type to handle comparison. Can handle 
 - less or equal (<=), less than (<), larger or equal(>=), larger than (>), and equals (==) comparisons.
 - The constructors are in the same order as above. -}
data Cmp = Sm Expr Expr    -- expr < expr
         | SE Expr Expr    -- expr <= expr
         | Bi Expr Expr    -- expr > expr
         | BE Expr Expr    -- expr >= expr
         | MyEq Expr Expr  -- expr == expr
         | NE Expr Expr    -- expr != expr
         | BoolVal Bool    -- bool
         | A Cmp Cmp       -- cmp && cmp
         | O Cmp Cmp       -- cmp || cmp
    deriving (Show, Eq)

{- Main parser. Parses the whole program. Start from here -}
parseProg :: Parser Prog
parseProg = Prog <$> (many parseStm)

{- Parser of a statement. Statements are separated by new lines. -}
parseStm :: Parser Stm
parseStm =  try (AssignVal <$> identifier <*> (symbol "=" *> parseExpr))
        <|> CreateVar <$> (symbol "int" <|> symbol "bool") <*> (identifier <* symbol "=") <*> parseExpr
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

{- Parse expressions. Priority on arithmetic operations is as follows: multiplication > subtraction > addition. -}
parseExpr :: Parser Expr
parseExpr = parseExprHelp1 `chainr1` ((\_ -> MySub) <$> reserved "-") 

parseExprHelp1 :: Parser Expr
parseExprHelp1 = parseExprHelp2 `chainr1` ((\_ -> MyAdd) <$> reserved "+")

parseExprHelp2 :: Parser Expr
parseExprHelp2 = parseExprHelp3 `chainr1` ((\_ -> Mult) <$> reserved "*")

parseExprHelp3 :: Parser Expr
parseExprHelp3 =  (parens parseExpr)
              <|> try (BVal <$> parseCmp)
              <|> (parseValue)
    
{- Value parser for a value - integer, identifier, or boolean. -}
parseValue :: Parser Expr
parseValue = (IVal <$> integer)
          <|> try (Id <$> identifier)
          <|> (BVal <$> parseCmp)
          
{- Parser for comparisons. "And" operations has higher priority than "Or". -}
parseCmp :: Parser Cmp
parseCmp = parseCmpHelp1 `chainr1` ((\_ -> O) <$> reserved "||")

parseCmpHelp1 :: Parser Cmp
parseCmpHelp1 = parseCmpHelp2 `chainr1` ((\_ -> A) <$> reserved "&&")

{- Parsers for different conditional operators such as ">", "<", "==", "!=. -}
parseCmpHelp2 :: Parser Cmp
parseCmpHelp2 =  try (BoolVal <$> (stringToBool <$> (symbol "true" <|> symbol "false")))
             <|> try (SE <$> (parseValue <* reserved "<=") <*> parseValue)
             <|> try (Sm <$> (parseValue <* reserved "<")  <*> parseValue)
             <|> try (BE <$> (parseValue <* reserved ">=") <*> parseValue)
             <|> try (Bi <$> (parseValue <* reserved ">")  <*> parseValue)
             <|> try (MyEq <$> (parseValue <* reserved "==") <*> parseValue)
             <|> try (NE <$> (parseValue <* reserved "!=") <*> parseValue)

{- Converts a boolean in the RGL EDSL as represented by a string into a Haskell boolean. -}
stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False
stringToBool a = error ("Can't convert '" ++ a ++ "' to boolean")
