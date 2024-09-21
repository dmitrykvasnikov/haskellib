module Yap.Error where

import           Data.List (intercalate)

type Line = Int

type Col = Int

type Pos = (Line, Col)

type Message = String

type Source = String

data Error = UnexpectedChar { ch     :: Char
                            , errPos :: Pos
                            , msg    :: Message
                            , src    :: Source
                            }
           | EndOfInput { errPos :: Pos
                        , msg    :: Message
                        , src    :: Source
                        }
           | CustomError { errPos :: Pos
                         , msg    :: Message
                         , src    :: Source
                         }
           | InternalError { errPos :: Pos
                           , msg    :: Message
                           , src    :: Source
                           }

instance Eq Error where
  e1 == e2 = errPos e1 == errPos e2

instance Ord Error where
  e1 <= e2 = errPos e1 <= errPos e2

instance Semigroup Error where
  (InternalError _ _ _) <> e2 = e2
  e1 <> (InternalError _ _ _) = e1
  e1 <> e2                    = if e1 > e2 then e1 else e2

instance Monoid Error where
  mempty = InternalError (0, 0) "Internal error for memty instance" ""
  mappend = (<>)

instance Show Error where
  show (UnexpectedChar c p m s) = intercalate "\n" [header p, m, "unexpected symbol '" <> [c] <> "'", s]
  show (EndOfInput p m s) = intercalate "\n" [header p, m, "unexpected end of stream", s]
  show (CustomError p m s) = intercalate "\n" [header p, m, s]
  show (InternalError p m s) = intercalate "\n" [header p, m, s]

-- make Error header
header :: Pos -> String
header (l, c) = "Error at ln " <> show l <> " col " <> show c
