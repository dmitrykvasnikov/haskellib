module Yap.Input where

import           Data.Text (Text)
import qualified Data.Text as T

-- pos - ((LineStartingPosition, NumberOfLine), NumberOf Column)
-- tab - tab size
data Input = Input { offset  :: Int
                   , pos     :: ((Int, Int), Int)
                   , curChar :: Char
                   , source  :: Text
                   , tab     :: Int
                   }
  deriving (Show)

mkInputFromFile :: FilePath -> IO Input
mkInputFromFile filepath = mkInputFromString <$> readFile filepath

mkInputFromString :: String -> Input
mkInputFromString str =
  Input
    { offset = 0,
      pos = ((0, 1), 1),
      curChar = if length str == 0 then '\NUL' else str !! 0,
      source = T.pack str,
      tab = 8
    }

moveInput :: Input -> Input
moveInput (Input o ((lp, l), c) ch sr t) =
  case o < (T.length sr) - 1 of
    True ->
      let newO = o + 1
          newC = T.index sr newO
       in case ch of
            '\n' -> Input newO ((newO, l + 1), 1) newC sr t
            '\t' -> Input newO ((lp, l), c + t - ((c - 1) `mod` t)) newC sr t
            _    -> Input newO ((lp, l), c + 1) newC sr t
    False -> Input o ((lp, l), c) '\NUL' sr t
