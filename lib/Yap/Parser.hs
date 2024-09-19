module Yap.Parser where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Text           as T (index, length, unpack)
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
  empty = Parser $ \i -> (Left $ InternalError (getErrPos i) "Internal error for empty instance" (getErrorSrc i), i)
  p1 <|> p2 = Parser $ \i ->
    case runParser p1 i of
      (Right t, i') -> (Right t, i')
      (Left e1, _) -> case runParser p2 i of
        (Right t, i'') -> (Right t, i'')
        (Left e2, _)   -> (Left $ e1 <> e2, i)

satisfy :: Message -> (Char -> Bool) -> Parser Char
satisfy msg cond = Parser $ \i ->
  case peek i of
    Just ch -> case cond ch of
      True  -> (Right ch, updatePos i)
      False -> (Left $ UnexpectedChar ch (getErrPos i) msg (getErrorSrc i), i)
    Nothing -> (Left $ EndOfInput (getErrPos i) msg (getErrorSrc i), i)

sym :: Char -> Parser Char
sym ch = satisfy ("Parser: char '" <> [ch] <> "'") (== ch)

string :: String -> Parser String
string s = traverse (satisfy ("Parser: string '" <> s <> "'") . (==)) s

-- get :: Parser Input
-- get = Parser $ \i -> (Right i, i)
--
-- modify :: (Input -> Input) -> Parser ()
-- modify f = Parser $ \i -> (Right (), f i)

peek :: Input -> (Maybe Char)
peek i =
  let s = source i
      o = offset i
   in case o < T.length s of
        True  -> Just $ T.index s o
        False -> Nothing

getErrPos :: Input -> (Int, Int)
getErrPos = (\((_, l), c) -> (l, c)) . pos

getErrorSrc :: Input -> String
getErrorSrc i =
  let ((l, _), c) = pos i
      s = source i
      ss = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ T.unpack s)
   in ss <> "*\n" <> src <> "\n" <> ss <> "*\n"

-- ErrPos :: Parser (Int, Int)
-- ErrPos = pos <$> get >>= \((_, l), c) -> return (l, c)
--
-- ErrorSrc :: Parser String
-- ErrorSrc = do
-- (l, _), c) <- pos <$> get
--  <- source <$> get
-- et ss = take (c - 1) (repeat ' ')
--    src = takeWhile (/= '\n') (drop l $ T.unpack s)
-- eturn $ ss <> "*\n" <> src <> "\n" <> ss <> "*\n"
