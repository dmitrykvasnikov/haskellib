-- Yet Another Parser
module Yap
  ( mkInputFromString,
    mkInputFromFile,
    parse,
    sym,
    anySym,
    string,
    num,
    sigNum,
    double,
    sigDouble,
    (<|>),
    some,
    many,
    asum,
  )
where

import           Control.Applicative
import           Yap.Input
import           Yap.Prim
