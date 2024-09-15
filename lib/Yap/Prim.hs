module Yap.Prim
  ( Config (..),
    Parser (..),
    parse,
    parse',
    sym,
    anySym,
    string,
    num,
    sigNum,
    double,
    sigDouble,
    mkInputFromString,
    mkInputFromFile,
    stdConfig,
    some,
    many,
    (<|>),
  )
where

import           Control.Applicative        (many, some, (<|>))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Reader       (runReader)
import           Control.Monad.State.Strict (evalStateT, runStateT)
import           Yap.Config
import           Yap.Error
import           Yap.Input
import           Yap.Parser

-- Parser runner
parse :: Parser t -> String -> Either Error t
parse (Parser p) = (flip runReader stdConfig) . (evalStateT . runExceptT) p . mkInputFromString

parse' :: Parser t -> String -> (Either Error t, Input)
parse' (Parser p) = (flip runReader stdConfig) . (runStateT . runExceptT) p . mkInputFromString

-- Standard config
stdConfig :: Config
stdConfig =
  Config
    { tab = 8,
      doubleSep = "."
    }
