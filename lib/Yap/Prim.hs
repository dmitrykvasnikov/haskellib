{-# LANGUAGE DeriveGeneric #-}

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

import           Control.Applicative              (many, some, (<|>))
import           Control.Monad.Trans.Except       (runExceptT)
import           Control.Monad.Trans.Reader       (runReader)
import           Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import           Yap.Config
import           Yap.Config                       (Config (doubleSep))
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
