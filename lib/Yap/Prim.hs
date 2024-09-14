{-# LANGUAGE DeriveGeneric #-}

module Yap.Prim
  ( (<|>),
    some,
    many,
    asum,
    --     mkInputFromFile,
    --     mkInputFromString,
  )
where

import           Control.Applicative              (Alternative (many), asum,
                                                   empty, some, (<|>))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader       (Reader, asks, runReader)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Yap.Config
import           Yap.Error
import           Yap.Input

newtype Parser t = Parser { unparse :: ExceptT Error (StateT Input (Reader Config)) t }
  deriving (Applicative, Functor, Monad)

parser :: Parser t -> String -> Either Error t
parser (Parser p) s = runReader ((evalStateT . runExceptT) p . mkInputFromString $ s) config

-- getErrorPos :: Parser (Int, Int)
-- getErrorPos = gets pos >>= \((_, l), c) -> return (l, c)
--
-- getErrorSrc :: Parser String
-- getErrorSrc = do
--   (Input _ ((l, _), c) _ s _) <- get
--   let sps = take (c - 1) (repeat ' ')
--       src = takeWhile (/= '\n') (drop l $ unpack s)
--   return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
