module Yap.Config where

-- tab - tabsize
data Config = Config { tab :: Int
                     }

-- Standard Config
config :: Config
config = Config {tab = 8}
