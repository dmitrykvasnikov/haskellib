module Yap.Config where

-- CONFIG for parser
-- tab - tabsize
-- doubleSep - separator for double numbers AS STRING!!!
data Config = Config { tabWidth  :: Int
                     , doubleSep :: String
                     }

instance Show Config where
  show cfg =
    "Configuration\nTab width: "
      <> (show . tabWidth $ cfg)
      <> "\nDouble number separator: "
      <> (show . doubleSep $ cfg)
