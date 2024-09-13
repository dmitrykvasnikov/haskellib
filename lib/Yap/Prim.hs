{-# LANGUAGE DeriveGeneric #-}

module Yap.Prim
  ( (<|>),
    some,
    many,
    asum,
    mkInput,
    sym,
    parse,
    string,
    --     mkInputFromFile,
    --     mkInputFromString,
  )
where

import           Control.Applicative              (Alternative (many), asum,
                                                   empty, some, (<|>))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Char                        (isDigit)
import           Data.Text                        (Text)
import qualified Data.Text                        as T

-- import           Yap.Error

data Input = Input { source :: Text
                   }
  deriving (Show)

data Error = Unexpected Char
           | EndOfInput
           | SystemError
  deriving (Show)

instance Semigroup Error where
  SystemError <> e = e
  e <> SystemError = e
  e1 <> _          = e1

instance Monoid Error where
  mappend = (<>)
  mempty = SystemError

newtype Parser t = Parser { parser :: ExceptT Error (State Input) t }
  deriving (Applicative, Functor, Monad)

instance Alternative Parser where
  empty = Parser . throwE $ SystemError
  (Parser p1) <|> (Parser p2) = Parser $ do
    i <- lift get
    let r1 = (evalState . runExceptT) p1 i
    case r1 of
      Right t -> return t
      Left _  -> p2

parse :: Parser t -> String -> Either Error t
parse (Parser p) = (evalState . runExceptT) p . mkInput

mkInput :: String -> Input
mkInput = Input . T.pack

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = Parser $ do
  c <- parser peek
  case cond c of
    True  -> return c
    False -> throwE $ Unexpected c

sym :: Char -> Parser Char
sym c = satisfy (== c)

string :: String -> Parser String
string = traverse sym

peek :: Parser Char
peek = Parser $ do
  i <- lift . gets $ source
  case T.length i == 0 of
    True -> throwE EndOfInput
    False -> do
      let (c, r) = (T.head i, T.tail i)
      lift . modify $ (\inp -> inp {source = r})
      return c

-- getErrorPos :: Parser (Int, Int)
-- getErrorPos = gets pos >>= \((_, l), c) -> return (l, c)
--
-- getErrorSrc :: Parser String
-- getErrorSrc = do
--   (Input _ ((l, _), c) _ s _) <- get
--   let sps = take (c - 1) (repeat ' ')
--       src = takeWhile (/= '\n') (drop l $ unpack s)
--   return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
