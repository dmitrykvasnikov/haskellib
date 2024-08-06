module Parsim.Parsers
  ( sym,
    anysym,
    string,
    digit,
    digits,
    num,
    sigNum,
    eof,
    anyOf,
    noneOf,
    sepBy,
    sepBy1,
    space,
    tab,
    newline,
    alpha,
    alphaNum,
    ws,
    upper,
    lower,
    between,
    trim,
    trim1,
    triml,
    trimr,
    count,
    choice,
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
    updatePosChar
    (\t -> if pred t then (Just t) else Nothing)
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

anyOf :: [Char] -> Parsim String Char
anyOf cs = satisfy ("any of " <> show cs) (flip elem cs)

noneOf :: [Char] -> Parsim String Char
noneOf cs = satisfy ("none of " <> show cs) (not . flip elem cs)

eof :: Parsim String ()
eof = Parsim $ \s@(Source pos src) ->
  case peek src of
    Nothing  -> Right $ Consumed () s
    (Just _) -> Left $ ParsimError pos [Expect "Expected end of file"]

digits :: Parsim String String
digits = many1 $ satisfy "digits" isDigit

num :: Parsim String Int
num = read <$> digits

sigNum :: Parsim String Int
sigNum = read <$> (digits <|> (sym '+' *> digits) <|> ((<>) <$> string "-" <*> digits))

sepBy :: Parsim String a -> Parsim String b -> Parsim String [a]
sepBy p sep = liftA2 (:) p (many (sep *> p)) <|> pure []

sepBy1 :: Parsim String a -> Parsim String b -> Parsim String [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

between :: Parsim i o1 -> Parsim i o2 -> Parsim i o -> Parsim i o
between p1 p2 p = p1 *> p <* p2

count :: Int -> Parsim String o -> Parsim String [o]
count n p
  | n < 1 = pure []
  | otherwise = sequence $ replicate n p

choice :: [Parsim o i] -> Parsim o i
choice = foldr (<|>) mzero
