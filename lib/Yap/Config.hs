module Yap.Config where

-- CONFIG for parser
-- tab - tabsize
-- doubleSep - separator for double numbers AS STRING!!!
data Config = Config { tab       :: Int
                     , doubleSep :: String
                     }
  deriving (Show)
