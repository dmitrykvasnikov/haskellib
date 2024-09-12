module Yap.Error where

data Error = Unexpected { msg    :: String
                        , errPos :: (Int, Int)
                        , chr    :: Char
                        , src    :: String
                        }
           | Message { msg    :: String
                     , errPos :: (Int, Int)
                     , chr    :: Char
                     , src    :: String
                     }
           | EndOfInput { errPos :: (Int, Int)
                        , chr    :: Char
                        , src    :: String
                        }
           | Internal { errPos :: (Int, Int)
                      , chr    :: Char
                      , src    :: String
                      }

instance Show Error where
  show (Unexpected msg (l, c) ch s) = "\nError at line " <> show l <> " column " <> show c <> "\n" <> msg <> "\nUnexpected char '" <> [ch] <> "'\n" <> s
  show (Message msg (l, c) ch s) = "\nError at line " <> show l <> " column " <> show c <> "\n" <> msg <> "Unexpected char '" <> [ch] <> "'\n" <> s
  show (EndOfInput (l, c) _ s) = "Unexpected end of input at line " <> show l <> " column " <> show c <> "\n" <> s
  show (Internal _ _ _) = "Internal error, something wrong ... just for monoid instance"

instance Semigroup Error where
  err1 <> err2 =
    let (l1, c1) = errPos err1
        (l2, c2) = errPos err2
     in if l1 == l2
          then if c2 > c1 then err2 else err1
          else if l2 > l1 then err2 else err1

instance Monoid Error where
  mempty = Internal (0, 0) '\NUL' ""
