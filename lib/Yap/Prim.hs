module Yap.Prim
  ( parse,
    parse',
    (<|>),
    (<?>),
    sym,
    anySym,
    pSym,
    lowSym,
    upSym,
    lowSyms,
    upSyms,
    oneOf,
    noneOf,
    alSym,
    alnumSym,
    eof,
    num,
    sigNum,
    double,
    sigDouble,
    many,
    many1,
    count,
    choice,
    best,
    withDefault,
    sp,
    ws,
    tab,
    nl,
    sps,
    wss,
    trim,
    triml,
    trimr,
    sepBy,
    sepBy0,
    sepByTrail,
    replace,
    between,
    followedBy,
    notFollowedBy,
    string,
  )
where

import           Control.Applicative ((<|>))
import           Yap.Error
import           Yap.Input
import           Yap.Parser

-- helper for parsers, take string as input. parse' return pair with result and input
parse :: Parser t -> String -> Either Error t
parse p = fst . runParser p . mkInputFromString_

parse' :: Parser t -> String -> (Either Error t, Input)
parse' p = runParser p . mkInputFromString_
