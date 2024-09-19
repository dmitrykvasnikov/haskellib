module Yap.Input where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           Yap.Config

type Col = Int

type Line = Int

type Offset = Int

-- Pos = ((Offset at which current line starts, Current Line), Current column)
type Pos = ((Offset, Line), Col)

-- pos - ((LineStartingPosition, NumberOfLine), NumberOf Column)
-- tab - tab size
data Input = Input { offset :: Offset
                   , pos    :: Pos
                   , source :: Text
                   , config :: Config
                   }

instance Eq Input where
  i1 == i2 = pos i1 == pos i2

instance Ord Input where
  i1 <= i2 = pos i1 <= pos i2

instance Show Input where
  show input =
    let ((_, l), c) = pos input
     in ( "Current offset: "
            <> show (offset input)
            <> "\nCurrent postion: ln "
            <> show l
            <> " col "
            <> show c
            <> "\nSource:\n"
            <> (T.unpack . source $ input)
        )

-- Standard config
stdConfig :: Config
stdConfig =
  Config
    { tabWidth = 8,
      doubleSep = "."
    }

-- convert file to Input with user config
mkInputFromFile :: Config -> FilePath -> IO Input
mkInputFromFile cfg filepath = mkInputFromString cfg <$> readFile filepath

-- convert file to Input with standard config
mkInputFromFile_ :: FilePath -> IO Input
mkInputFromFile_ = mkInputFromFile stdConfig

-- convert string to Input structure with user config
mkInputFromString :: Config -> String -> Input
mkInputFromString cfg str =
  Input
    { offset = 0,
      pos = ((0, 1), 1),
      source = T.pack str,
      config = cfg
    }

-- convert string to Input with standard config
mkInputFromString_ :: String -> Input
mkInputFromString_ = mkInputFromString stdConfig

-- update position
updatePos :: Input -> Input
updatePos i =
  let newOffset = offset i + 1
      ch = T.index (source i) (offset i)
   in case ch of
        '\n' -> let ((_, l), _) = pos i in i {offset = newOffset, pos = ((newOffset, l + 1), 1)}
        '\t' ->
          let t = tabWidth . config $ i
              (l, c) = pos i
           in i {offset = newOffset, pos = (l, c - (c - 1) `mod` t + t)}
        _ -> let (l, c) = pos i in i {offset = newOffset, pos = (l, c + 1)}
