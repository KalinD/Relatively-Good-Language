module MyParser (parseMyLang) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)
import Data.Either

fromLeft :: Either l r -> l
fromLeft (Left x) = x

fromRight :: Either l r -> r
fromRight (Right x) = x

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft res
            | otherwise  = fromRight res
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
    Token.reservedOpNames = ["(" ,")", "+", "*", "=", "==", "<", ">", "<=", ">="],
    Token.reservedNames = ["whatIf", "butWhatIf", "pleaseDoWhile",
        "allAtOnce", "hangingByAThread", "shared", "int", "bool", "aFewOf", "true", "false"]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer