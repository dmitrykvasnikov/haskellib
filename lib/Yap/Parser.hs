module Yap.Parser where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       (MonadPlus (..))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isLower,
                                      isUpper)
import           Data.Text           as T (index, length, unpack)
import           GHC.Read            (list)
import           Yap.Config
import           Yap.Error
import           Yap.Input

newtype Parser a = Parser { runParser :: Input -> (Either Error a, Input) }

instance Functor Parser where
  fmap f p = Parser $ \i ->
    case runParser p i of
      (Left e, _)   -> (Left e, i)
      (Right t, i') -> (Right . f $ t, i')

instance Applicative Parser where
  pure t = Parser $ \i -> (Right t, i)
  pf <*> pt = Parser $ \i ->
    case runParser pf i of
      (Left e, _) -> (Left e, i)
      (Right f, i') -> case runParser pt i' of
        (Left e, _)    -> (Left e, i)
        (Right t, i'') -> (Right . f $ t, i'')

instance Monad Parser where
  return = pure
  mt >>= f = Parser $ \i ->
    case runParser mt i of
      (Left e, _)   -> (Left e, i)
      (Right t, i') -> runParser (f t) i'

instance Alternative Parser where
  empty = Parser $ \i -> (Left $ InternalError (getErrPos i) "Internal error for Alternative instance" (getErrorSrc i), i)
  p1 <|> p2 = Parser $ \i ->
    case runParser p1 i of
      (Right t, i') -> (Right t, i')
      (Left e1, _) -> case runParser p2 i of
        (Right t, i'') -> (Right t, i'')
        (Left e2, _)   -> (Left $ e1 <> e2, i)

instance MonadPlus Parser where
  mzero = Parser $ \i -> (Left $ InternalError (getErrPos i) "Internal error for MonadPlus instance" (getErrorSrc i), i)

-- base parser for symbols
satisfy :: Message -> (Char -> Bool) -> Parser Char
satisfy msg cond = Parser $ \i ->
  case peek i of
    Just ch -> case cond ch of
      True  -> (Right ch, updatePos i)
      False -> (Left $ UnexpectedChar ch (getErrPos i) msg (getErrorSrc i), i)
    Nothing -> (Left $ EndOfInput (getErrPos i) msg (getErrorSrc i), i)

-- return empty list in case of fail
many1 :: Parser p -> Parser [p]
many1 p = some'
  where
    many' = some' <|> return []
    some' = liftA2 (:) p many'

-- return an error in case of fail
many :: Parser p -> Parser [p]
many p = many'
  where
    many' = some' <|> return []
    some' = liftA2 (:) p many'

-- parser to check for end of input
eof :: Parser ()
eof = Parser $ \i -> case peek i of
  (Just ch) -> (Left $ UnexpectedChar ch (getErrPos i) "Expected end of input" (getErrorSrc i), i)
  Nothing -> (Right (), i)

-- parse symbol
sym :: Char -> Parser Char
sym ch = satisfy ("Parser: char '" <> [ch] <> "'") (== ch)

-- parse symbol with condition, without description
pSym :: (Char -> Bool) -> Parser Char
pSym = satisfy "Custom parser with condition"

-- spaces, whitespaces, tabs etc
sp, ws, tab, nl :: Parser Char
sp = satisfy "Parser: space" (== ' ')
ws = satisfy "Parser: whitespace" (flip elem " \n\t")
tab = satisfy "Parser: tab" (== '\t')
nl = satisfy "Parser: new line" (== '\n')

sps, wss :: Parser String
sps = many1 $ satisfy "Parser: spaces" (== ' ')
wss = many1 $ satisfy "Parser: whitespaces" (flip elem " \n\t")

-- trim, triml, trimr - removes whitespaces from both sides, left or right
trim, triml, trimr :: Parser a -> Parser a
trim p = wss *> p <* wss
triml p = wss *> p
trimr p = p <* wss

-- choice is alternative to <|>
choice :: [Parser a] -> Parser a
choice = foldr (<|>) (Parser $ \i -> (Left $ InternalError (getErrPos i) "Choice parser applied to empty list of parsers" (getErrorSrc i), i))

best :: [Parser a] -> Parser a
best ps = Parser $ \i ->
  foldr (go i) (Left $ InternalError (getErrPos i) "Best parser applied to empty list of parsers" (getErrorSrc i), i) ps
  where
    go :: Input -> Parser t -> (Either Error t, Input) -> (Either Error t, Input)
    go i p r =
      let r' = runParser p i
       in case (r', r) of
            ((Left e1, _), (Left e2, _)) -> if e1 < e2 then (Left e2, i) else (Left e1, i)
            ((Left _, _), r1) -> r1
            (r1, (Left _, _)) -> r1
            ((r1, i1), (r2, i2)) -> if i1 < i2 then (r2, i2) else (r1, i1)

-- parser for up / lower / alpha / alphanum char
upSym, lowSym, alSym, alnumSym :: Parser Char
upSym = satisfy "Parser: symbol in upper case" isUpper
lowSym = satisfy "Parser: symbol in lower case" isLower
alSym = satisfy "Parser: alphabetic symbol" isAlpha
alnumSym = satisfy "Parser: alphanumeric symbol" isAlphaNum

-- strings from lower / upper symbol
upSyms, lowSyms :: Parser String
upSyms = many1 $ satisfy "Parser: string of symbols in upper case" isUpper
lowSyms = many1 $ satisfy "Parser: string of symbols in lower case" isLower

-- parser char which is presented / not presented in string
oneOf, noneOf :: String -> Parser Char
oneOf str = satisfy ("Parser: oneOf '" <> str <> "'") (flip elem str)
noneOf str = satisfy ("Parser: noneOf '" <> str <> "'") (not . flip elem str)

-- any symbol or error in case of end of input
anySym :: Parser Char
anySym = satisfy "Parser: any symbol" (const True)

-- parser for string
string :: String -> Parser String
string s = traverse (satisfy ("Parser: string '" <> s <> "'") . (==)) s

-- helper for number parses
numS :: String -> Parser String
numS msg = many1 $ satisfy msg isDigit

-- parser for integer number
num, sigNum :: Parser Int
num = read <$> numS "Parser: integer number"
sigNum = num' <|> (sym '+' *> num') <|> (sym '-' *> (negate <$> num'))
  where
    num' :: Parser Int
    num' = read <$> numS "Parser: signed integer number"

-- parser for double number
double, sigDouble :: Parser Double
double = double' "Parser: double number"
sigDouble = double' sd <|> (sym '+' *> double' sd) <|> (sym '-' *> (negate <$> double' sd))
  where
    sd = "Parser: signed double number"

double' :: String -> Parser Double
double' msg =
  asks doubleSep >>= \sep ->
    read . mconcat <$> sequence [numS msg, string sep, numS msg]

-- parser with default value, doesn't consume input in case of defaut value
withDefault :: t -> Parser t -> Parser t
withDefault t p = Parser $ \i ->
  case runParser p i of
    (Right t', i') -> (Right t', i')
    (Left _, _)    -> (Right t, i)

-- get parser and if it's sucessfull - return another value
replace :: Parser a -> b -> Parser b
replace p t = p >> return t

-- followedBy / notFollowedBy - doesn't consume second parser
followedBy :: Parser a -> Parser b -> Parser a
followedBy pt pf = Parser $ \i ->
  case runParser pt i of
    r@(Right _, i') -> case runParser pf i' of
      (Right _, _) -> r
      (Left e, _)  -> (Left e, i)
    (Left e, _) -> (Left e, i)

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy pt pf = Parser $ \i ->
  case runParser pt i of
    r@(Right _, i') -> case runParser pf i' of
      (Right _, _) -> (Left $ InternalError (getErrPos i') "Failed with notFollowedBy parser" (getErrorSrc i'), i)
      (Left _, _) -> r
    (Left e, _) -> (Left e, i)

-- parser for list with separator, sepBy should have at least one element,
-- sepBy0 will return empty list in case of failure,
-- sepByTrail allows trailing separator after last element of list
sepBy, sepBy0, sepByTrail :: Parser t -> Parser s -> Parser [t]
sepBy p s = liftA2 (:) p (many (s *> p))
sepBy0 p s = sepBy p s <|> (pure [])
sepByTrail p s = sepBy p s <* (replace s () <|> pure ())

-- parse value between other parsers
between :: Parser b1 -> Parser b2 -> Parser t -> Parser t
between b1 b2 p = b1 *> p <* b2

-- runs parser N times and return a list, in case of 0 or negative N returns an empty list
count :: Int -> Parser t -> Parser [t]
count n p = case n > 0 of
  True  -> sequence (replicate n p)
  False -> return []

-- in case if parser fails it return Error with custom Message
(<?>) :: Parser t -> String -> Parser t
(<?>) p msg = Parser $ \i ->
  case runParser p i of
    r@(Right _, _) -> r
    _              -> (Left . CustomError (getErrPos i) msg $ getErrorSrc i, i)

-- helpers to get info from Input
peek :: Input -> (Maybe Char)
peek i =
  let s = source i
      o = offset i
   in case o < T.length s of
        True  -> Just $ T.index s o
        False -> Nothing

get :: Parser Input
get = Parser $ \i -> (Right i, i)

asks :: (Config -> a) -> Parser a
asks f = Parser $ \i -> (Right . f . config $ i, i)

getErrPos :: Input -> (Int, Int)
getErrPos = (\((_, l), c) -> (l, c)) . pos

getErrorSrc :: Input -> String
getErrorSrc i =
  let ((l, _), c) = pos i
      s = source i
      ss = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ T.unpack s)
   in ss <> "*\n" <> src <> "\n" <> ss <> "*\n"
