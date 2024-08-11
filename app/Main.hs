{-# LANGUAGE RecordWildCards #-}

module Main where

import           Parsim

data I = I { a, b, c :: Int
           }
  deriving (Show)

i :: Parsim String I
i = do
  b <- (read @Int . (\x -> [x])) <$> digit
  sym '-'
  a <- (read @Int . (\x -> [x])) <$> digit
  sym '-'
  c <- (read @Int . (\x -> [x])) <$> digit
  return I {..}

main :: IO ()
main = putStrLn "libs"
