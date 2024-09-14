module Yap.Config where

-- tab - tabsize
-- doubleSep - separator for double numbers AS STRING!!!
data Config = Config { tab       :: Int
                     , doubleSep :: String
                     }
  deriving (Show)
