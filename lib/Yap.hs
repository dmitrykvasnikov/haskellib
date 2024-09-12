-- Yet Another Parser
module Yap
  ( mkInputFromString,
    mkInputFromFile,
    parse,
    sym,
    anySym,
    string,
    (<|>),
    some,
    many,
  )
where

import           Control.Applicative
import           Yap.Input
import           Yap.Prim
