module Parsim.Parsers
  ( sym,
    anysym,
    string,
    digit,
    digits,
    num,
    sigNum,
    space,
    tab,
    newline,
    alpha,
    alphaNum,
    ws,
    upper,
    lower,
    trim,
    trim1,
    triml,
    trimr,
    noneOf,
    anyOf,
  )
where

import           Data.Char    (isAlpha, isAlphaNum, isDigit, isLower, isUpper)
import           Parsim.Error
import           Parsim.Pos
import           Parsim.Prim

-- sym :: Char -> Parsim String Char
-- sym ch =
--   token
--     updatePosChar
--     (\t -> if t == ch then (Just ch) else Nothing)
--     (\pos t -> ParsimError pos [Unexpect $ "Unexpected token " <> show t <> " | expected token " <> show ch])

satisfy :: String -> (Char -> Bool) -> Parsim String Char
satisfy name pred =
  token
    (\t -> if pred t then (Just t) else Nothing)
    updatePosChar
    ( \src t ->
        ParsimError
          (sourcePos src)
          [Unexpect $ "Failed parser: " <> name <> "\nUnexpected character " <> show t <> "\n" <> show (source src)]
    )

sym :: Char -> Parsim String Char
sym s = satisfy ("character " <> show s) (== s)

space, tab, newline, ws, anysym, digit, alpha, alphaNum, upper, lower :: Parsim String Char
space = satisfy "space" (== ' ')
tab = satisfy "tab" (== '\t')
newline = satisfy "newline" (== '\n')
ws = satisfy "whitespace" (\c -> elem c "\t\n ")
anysym = satisfy "any character" (const True)
digit = satisfy "digit" isDigit
alpha = satisfy "alpha" isAlpha
alphaNum = satisfy "alpha-numeric" isAlphaNum
upper = satisfy "upper" isUpper
lower = satisfy "lower" isLower

trim, trim1, triml, trimr :: Parsim String o -> Parsim String o
trim p = (many ws) *> p <* (many ws)
trim1 p = (many1 ws) *> p <* (many1 ws)
triml p = (many1 ws) *> p
trimr p = p <* many ws

string :: String -> Parsim String String
string s = traverse (\c -> satisfy ("string " <> show s) (== c)) s

digits :: Parsim String String
digits = many1 $ satisfy "digits" isDigit

anyOf :: [Char] -> Parsim String Char
anyOf cs = satisfy ("any of " <> show cs) (flip elem cs)

noneOf :: [Char] -> Parsim String Char
noneOf cs = satisfy ("none of " <> show cs) (not . flip elem cs)

num :: Parsim String Int
num = read <$> digits

sigNum :: Parsim String Int
sigNum = read <$> (digits <|> (sym '+' *> digits) <|> ((<>) <$> string "-" <*> digits))
