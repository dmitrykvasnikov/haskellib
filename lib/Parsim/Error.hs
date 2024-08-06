module Parsim.Error
  ( ParsimError (..),
    Message (..),
    mkError,
  )
where

import           Parsim.Pos (SourcePos)

data Message = Unexpect String
             | Expect String
             | Message String

instance Enum Message where
  fromEnum (Unexpect _) = 1
  fromEnum (Expect _)   = 2
  fromEnum (Message _)  = 3
  toEnum _ = error "toEnum is undefined for Message datatype"

instance Show Message where
  show (Unexpect s) = s
  show (Expect s)   = s
  show (Message s)  = s

data ParsimError = ParsimError SourcePos [Message]

instance Show ParsimError where
  show (ParsimError pos msgs) =
    "Error(s) happened\n" <> show pos <> printMessages msgs

printMessages :: [Message] -> String
printMessages [] = "Empty error messages list"
printMessages [msg] = show msg
printMessages (msg : msgs) = go 1 msg msgs
  where
    go n msg' [] = show n <> ". " <> show msg'
    go n msg' msgs' = show n <> ". " <> show msg' <> "\n" <> go (n + 1) (head msgs') (tail msgs')

mkError :: SourcePos -> String -> ParsimError
mkError pos msg = ParsimError pos [Message msg]
