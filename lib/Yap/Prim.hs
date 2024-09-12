{-# LANGUAGE DeriveGeneric #-}

module Yap.Prim where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict (State, evalState, get, gets,
                                                   modify)
import           Data.Text                        (unpack)
import           GHC.Generics
import           Yap.Error
import           Yap.Input

-- type ParserT monad token = ExceptT Error (StateT Input monad) token

type Parser token = ExceptT Error (State Input) token

parse :: Parser token -> String -> Either Error token
parse p s = (evalState . runExceptT) p (mkInputFromString s)

satisfy :: (Char -> Bool) -> ((Int, Int) -> Char -> String -> Error) -> Parser Char
satisfy cond err = do
  ch <- lift . gets $ curChar
  case ch == '\NUL' of
    True -> getErrorPos >>= \p -> getErrorSrc >>= throwE . EndOfInput p ch
    False -> case cond ch of
      True  -> move >> return ch
      False -> getErrorPos >>= \p -> getErrorSrc >>= throwE . err p ch

sym :: Char -> Parser Char
sym ch = satisfy (== ch) (Unexpected $ "Can not parse char '" <> [ch] <> "'")

anySym :: Parser Char
anySym = satisfy (const True) Internal

string :: String -> Parser String
-- string str = traverse (\ch -> satisfy (== ch) (Unexpected $ "Can not parse string '" <> str <> "'")) str
string = traverse sym

move :: Parser ()
move = lift . modify $ peek

getErrorPos :: Parser (Int, Int)
getErrorPos = lift . gets $ pos >>= \((_, l), p) -> return (l, p)

getErrorSrc :: Parser String
getErrorSrc = do
  (Input _ ((l, _), c) _ s _) <- lift get
  let sps = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ unpack s)
  return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
