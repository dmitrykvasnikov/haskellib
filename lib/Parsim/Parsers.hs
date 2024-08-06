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
  )
where

import           Data.Char    (isDigit)
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
    (\pos t -> ParsimError pos [Unexpect $ "Failed parser: " <> name <> "\nUnexpected character " <> show t])

sym :: Char -> Parsim String Char
sym s = satisfy ("character " <> show s) (== s)

anysym :: Parsim String Char
anysym = satisfy "any character" (const True)

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

digit :: Parsim String Char
digit = satisfy "digit" isDigit

digits :: Parsim String String
digits = many1 $ satisfy "digits" isDigit

num :: Parsim String Int
num = read <$> digits

sigNum :: Parsim String Int
sigNum = read <$> ((sym '+' *> digits) <|> (string "-" <?> digits))

sepBy :: Parsim String a -> Parsim String b -> Parsim String [a]
sepBy p sep = liftA2 (:) p (many (sep *> p)) <|> pure []

sepBy1 :: Parsim String a -> Parsim String b -> Parsim String [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
