module Yap.Input where

import           Data.Text (Text)
import qualified Data.Text as T

-- pos - ((LineStartingPosition, NumberOfLine), NumberOf Column)
-- tab - tab size
data Input = Input { offset :: Int
                   , pos    :: ((Int, Int), Int)
                   , source :: Text
                   }
  deriving (Show)

-- convert line to Input structure
mkInputFromFile :: FilePath -> IO Input
mkInputFromFile filepath = mkInputFromString <$> readFile filepath

-- convert file to Input structure
mkInputFromString :: String -> Input
mkInputFromString str =
  Input
    { offset = 0,
      pos = ((0, 1), 1),
      source = T.pack str
    }
