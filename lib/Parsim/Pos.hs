module Parsim.Pos
  ( SourcePos (..),
    updatePosChar,
    updatePosString,
  )
where

type SourceName = String

type Column = Int

type Line = Int

data SourcePos = SourcePos SourceName Line Column

instance Show SourcePos where
  show (SourcePos n l c) = "Source: " <> n <> "\n" <> "line " <> show l <> " | position " <> show c <> "\n"

updatePosChar :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos n l c) ch
  | ch == '\n' = SourcePos n (l + 1) 1
  | ch == '\t' = SourcePos n l (c - ((c - 1) `mod` 8) + 8)
  | otherwise = SourcePos n l (c + 1)

updatePosString :: SourcePos -> String -> SourcePos
updatePosString s = foldl updatePosChar s
