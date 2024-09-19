module Yap.Prim
  ( parse,
    parse',
    (<|>),
    sym,
    string,
  )
where

import           Control.Applicative ((<|>))
import           Yap.Error
import           Yap.Input
import           Yap.Parser

parse :: Parser t -> String -> Either Error t
parse p = fst . runParser p . mkInputFromString_

parse' :: Parser t -> String -> (Either Error t, Input)
parse' p = runParser p . mkInputFromString_
