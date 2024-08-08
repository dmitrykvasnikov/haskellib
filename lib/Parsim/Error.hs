module Parsim.Error
  ( ParsimError (..),
    Message (..),
    mkError,
    mergeError,
  )
where

import           Parsim.Pos (SourcePos)

data Message = Unexpect String
             | Expect String
             | Message String
             | System String

instance Enum Message where
  fromEnum (Unexpect _) = 1
  fromEnum (Expect _)   = 2
  fromEnum (Message _)  = 3
  fromEnum (System _)   = 4
  toEnum _ = error "toEnum is undefined for Message datatype"

instance Show Message where
  show (Unexpect s) = s
  show (Expect s)   = s
  show (Message s)  = s
  show (System s)   = s

data ParsimError = ParsimError SourcePos [Message]

instance Show ParsimError where
  show (ParsimError pos msgs) =
    "Error(s) happened\n" <> show pos <> printMessages msgs

printMessages :: [Message] -> String
printMessages [] = "Empty error messages list"
printMessages [msg] = show msg
printMessages (msg : msgs) = go 1 msg msgs
  where
    go :: Int -> Message -> [Message] -> String
    go n msg' [] = show n <> ". " <> show msg'
    go n msg' msgs' = show n <> ". " <> show msg' <> "\n" <> go (n + 1) (head msgs') (tail msgs')

mkError :: String -> SourcePos -> String -> ParsimError
mkError errType pos msg
  | errType == "sys" = ParsimError pos [System msg]
  | errType == "msg" = ParsimError pos [Message msg]
  | otherwise = error $ "Unknown error type in mkParsimError method: " <> errType

mergeError :: ParsimError -> ParsimError -> ParsimError
mergeError e1@(ParsimError pos1 msg1) e2@(ParsimError pos2 msg2) =
  case compare pos1 pos2 of
    EQ -> ParsimError pos1 (msg1 <> msg2)
    LT -> e2
    GT -> e1
