module Yap.Error where

import           Data.List (intercalate)

data Error = EndOfFileError { errPos :: (Int, Int)
                            , src    :: String
                            }
           | UnexpectedError { ch     :: Char
                             , errPos :: (Int, Int)
                             , msg    :: String
                             , src    :: String
                             }
           | InternalError { errPos :: (Int, Int)
                           , msg    :: String
                           , src    :: String
                           }

instance Show Error where
  show (EndOfFileError p s) = intercalate "\n" [errHeader p, "unexpected end of input", s]
  show (UnexpectedError c p m s) = intercalate "\n" [errHeader p, m, "unexpected symbol '" <> [c] <> "'", s]
  show (InternalError p m s) = intercalate "\n" [errHeader p, m, s]

errHeader :: (Int, Int) -> String
errHeader (l, c) = "Error at line " <> show l <> " column " <> show c
