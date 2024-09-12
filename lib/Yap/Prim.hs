{-# LANGUAGE DeriveGeneric #-}

module Yap.Prim where

import           Control.Applicative (Alternative, empty, some, (<|>))
import           Data.Char           (isDigit)
import           Data.Text           (unpack)
import           Yap.Error
import           Yap.Input

newtype Parser token = Parser { parser :: Input -> Either Error (token, Input) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \i ->
    case p i of
      Right (t, i') -> Right (f t, i')
      Left e        -> Left e

instance Applicative Parser where
  pure t = Parser $ \i -> Right (t, i)
  (Parser af) <*> (Parser a) = Parser $ \i ->
    case af i of
      Right (f, i') -> case a i' of
        Right (a', i'') -> Right (f a', i'')
        Left e          -> Left e
      Left e -> Left e

instance Alternative Parser where
  empty = Parser $ \_ -> Left $ Internal (0, 0) '!' "Internal error for empty method in Alternative"
  (Parser p1) <|> (Parser p2) = Parser $ \i ->
    case p1 i of
      r@(Right _) -> r
      Left _      -> p2 i

instance Monad Parser where
  return = pure
  (Parser ma) >>= f = Parser $ \i ->
    case ma i of
      Right (a, i') -> parser (f a) i'
      Left e        -> Left e

satisfy :: (Char -> Bool) -> ((Int, Int) -> Char -> String -> Error) -> Parser Char
satisfy cond err = do
  ch <- gets curChar
  case ch == '\NUL' of
    True -> getErrorPos >>= \p -> getErrorSrc >>= \s -> Parser $ \_ -> Left $ EndOfInput p '!' s
    False -> case cond ch of
      True -> modify moveInput >> return ch
      False -> getErrorPos >>= \p -> getErrorSrc >>= \s -> Parser $ \_ -> Left $ err p ch s

sym :: Char -> Parser Char
sym c = satisfy (== c) (Unexpected $ "Can not parse char '" <> [c] <> "'")

anySym :: Parser Char
anySym = satisfy (const True) EndOfInput

string :: String -> Parser String
string s = traverse (\c -> satisfy (== c) (Unexpected $ "Can not parse string '" <> s <> "'")) s

numS :: Parser String
numS = (some (satisfy isDigit (Unexpected "Can not parse digit")))

num, sigNum :: Parser Int
num = read <$> numS
sigNum = num <|> sym '-' *> (negate <$> num) <|> sym '+' *> num

double, sigDouble :: Parser Double
double = read . concat <$> sequence [numS, string ".", numS]
sigDouble = read . concat <$> sequence [numS <|> string "+" *> numS <|> (concat <$> sequence [string "-", numS]), string ".", numS]

parse :: Parser t -> String -> Either Error t
parse p s = fmap fst <$> parser p $ mkInputFromString s

get :: Parser Input
get = Parser $ \i -> return (i, i)

gets :: (Input -> a) -> Parser a
gets f = Parser $ \i -> return (f i, i)

modify :: (Input -> Input) -> Parser ()
modify f = Parser $ \i -> return ((), f i)

getErrorPos :: Parser (Int, Int)
getErrorPos = gets pos >>= \((_, l), c) -> return (l, c)

getErrorSrc :: Parser String
getErrorSrc = do
  (Input _ ((l, _), c) _ s _) <- get
  let sps = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ unpack s)
  return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
