module Yap.Error where

import           Data.List (intercalate)

-- ch and msg are char at which error happens and message about error (provided by parser), src - line from input with error
data Error = EndOfFileError { ch     :: Char
                            , errPos :: (Int, Int)
                            , src    :: String
                            }
           | UnexpectedError { msg    :: String
                             , ch     :: Char
                             , errPos :: (Int, Int)
                             , src    :: String
                             }
           | ExpectedError { msg    :: String
                           , errPos :: (Int, Int)
                           , src    :: String
                           }
           | InternalError { msg    :: String
                           , errPos :: (Int, Int)
                           , src    :: String
                           }

instance Eq Error where
  e1 == e2 =
    let (l1, c1) = errPos e1
        (l2, c2) = errPos e2
     in (l1 == l2) && (c1 == c2)

instance Ord Error where
  e1 <= e2 =
    let (l1, c1) = errPos e1
        (l2, c2) = errPos e2
     in if l1 == l2
          then c1 <= c2
          else l1 <= l2

instance Show Error where
  show (EndOfFileError _ p s) = intercalate "\n" [errHeader p, "unexpected end of input", s]
  show (UnexpectedError m c p s) = intercalate "\n" [errHeader p, m, "unexpected symbol '" <> [c] <> "'", s]
  show (ExpectedError m p s) = intercalate "\n" [errHeader p, m, s]
  show (InternalError m p s) = intercalate "\n" [errHeader p, m, s]

instance Semigroup Error where
  (InternalError _ _ _) <> e2 = e2
  e1 <> (InternalError _ _ _) = e1
  e1 <> e2                    = if e1 <= e2 then e2 else e1

instance Monoid Error where
  mempty = InternalError "Internal error for mempty element" (1, 1) "No source code"
  mappend = (<>)

-- HELPERS
-- error header with position of error
errHeader :: (Int, Int) -> String
errHeader (l, c) = "Error at line " <> show l <> " column " <> show c
