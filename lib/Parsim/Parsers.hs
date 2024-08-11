module Parsim.Parsers
  ( sym,
    anySym,
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
    anyTok,
    anyTok',
    eof,
    many1,
    sepBy,
    sepBy1,
    between,
    withDefault,
    withDefaultCons,
    followedBy,
    notFollowedBy,
    choice,
    best,
    chain,
    count,
  )
where

import           Data.Char    (isAlpha, isAlphaNum, isDigit, isLower, isUpper)
import           Parsim.Error
import           Parsim.Pos
import           Parsim.Prim

-- common Parsers

many1 :: Parsim i o -> Parsim i [o]
many1 = some

anyTok :: (Stream i o) => (SourcePos -> o -> SourcePos) -> Parsim i o
anyTok toPos = token Just toPos undefined

anyTok' :: (Stream i o) => Parsim i o
anyTok' = anyTok (flip $ const increasePos)

-- returns default value if parser fails

-- * doesn't consume input stream

withDefault :: o -> Parsim i o -> Parsim i o
withDefault def p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed def src
    r      -> r

-- * consume input stream

withDefaultCons :: o -> (Source i -> o -> Source i) -> Parsim i o -> Parsim i o
withDefaultCons def toPos p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed def $ toPos src def
    r      -> r

-- end of file

eof :: (Stream i o) => Parsim i ()
eof = Parsim $ \s@(Source pos src) ->
  case peek src of
    Nothing  -> Right $ Consumed () s
    (Just _) -> Left $ ParsimError pos [Expect $ "Expected end of file\n"]

-- chain with provided function. Raise error in case of empty parser list
-- Execut the only parser if only one parser in the list
chain :: (o -> o -> o) -> [Parsim i o] -> Parsim i o
chain _ []       = undefined
chain _ [p]      = p
chain f (p : ps) = foldl (liftA2 f) p ps

-- choice is alternative to <|>
choice :: [Parsim o i] -> Parsim o i
choice = foldr (<|>) mzero

-- best is like choice, but returns pareser which consumes the most part in inout stream
best :: [Parsim o i] -> Parsim o i
best = foldl choose err
  where
    err = mkParsimError "msg" "Error for 'best' parser initial value"
    choose :: Parsim i o -> Parsim i o -> Parsim i o
    choose p1 p2 = Parsim $ \src ->
      case unParsim p1 src of
        Left _ -> unParsim p2 src
        res1@(Right (Consumed o1 rest1)) ->
          case unParsim p2 src of
            Left _ -> res1
            res2@(Right (Consumed o1 rest2)) ->
              case compare (sourcePos rest1) (sourcePos rest2) of
                LT -> res2
                _  -> res1

-- try parser N times
count :: Int -> Parsim i o -> Parsim i [o]
count n p
  | n < 1 = pure []
  | otherwise = sequence $ replicate n p

sepBy :: Parsim i a -> Parsim i b -> Parsim i [a]
sepBy p sep = liftA2 (:) p (many (sep *> p)) <|> pure []

sepBy1 :: Parsim i a -> Parsim i b -> Parsim i [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

between :: Parsim i o1 -> Parsim i o2 -> Parsim i o -> Parsim i o
between p1 p2 p = p1 *> p <* p2

-- check parser without consuming input
followedBy :: (Stream i o) => Parsim i o -> Parsim i ()
followedBy p = Parsim $ \src ->
  case unParsim p src of
    Right _ -> Right $ Consumed () src
    Left e  -> Left e

notFollowedBy :: (Stream i o) => Parsim i o -> Parsim i ()
notFollowedBy p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed () src
    Right _ -> Left $ mkError "msg" (sourcePos src) ("notFollowedBy parser failed")

-- string Parsers
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

space, tab, newline, ws, anySym, digit, alpha, alphaNum, upper, lower :: Parsim String Char
space = satisfy "space" (== ' ')
tab = satisfy "tab" (== '\t')
newline = satisfy "newline" (== '\n')
ws = satisfy "whitespace" (\c -> elem c "\t\n ")
anySym = satisfy "any character" (const True)
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
