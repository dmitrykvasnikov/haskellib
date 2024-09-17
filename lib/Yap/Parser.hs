{-# LANGUAGE DerivingStrategies #-}

module Yap.Parser where

import           Control.Applicative        (Alternative, asum, empty, many,
                                             some, (<|>))
import           Control.Monad.Except       (ExceptT, MonadError, runExceptT,
                                             throwError)
import           Control.Monad.Reader       (MonadReader, Reader, ask, asks,
                                             runReader)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, modify, put)
import           Data.Char                  (isAlpha, isAlphaNum, isDigit,
                                             isLower, isUpper)
import qualified Data.Text                  as T (index, length, unpack)
import           Yap.Config
import           Yap.Error
import           Yap.Input

-- for conviniece usage in internal functions
type Parser_ = ExceptT Error (StateT Input (Reader Config))

newtype Parser t = Parser { unparse :: Parser_ t }
  deriving newtype
  ( Functor
  , Monad
  , MonadError Error
  , MonadReader Config
  , MonadState Input
  )

instance Applicative Parser

-- works properly and return error with longest consumed input
instance Alternative Parser where
  empty = Parser $ getErrorPos >>= \p -> getErrorSrc >>= throwError . InternalError "Internal error for empty instance of Applicative" p
  (Parser p1) <|> (Parser p2) = Parser $ do
    t1 <- try_ p1
    case t1 of
      Right _ -> p1 >>= return
      Left e1 -> do
        t2 <- try_ p2
        case t2 of
          Right _ -> p2 >>= return
          Left e2 -> if e1 < e2 then throwError e2 else throwError e1

-- basic parser for char - return char if confition holds and input has chars
satisfy :: (Char -> Bool) -> (Char -> (Int, Int) -> String -> Error) -> Parser Char
satisfy cond err = Parser $ do
  ch <- peek
  case cond ch of
    True  -> consumeInput >> return ch
    False -> getErrorPos >>= \p -> getErrorSrc >>= throwError . err ch p

-- parser for specific char
sym :: Char -> Parser Char
sym ch = satisfy (== ch) (UnexpectedError $ "Can not parse symbol '" <> [ch] <> "'")

-- get any symbol and make error in case of end of input
anySym :: Parser Char
anySym = satisfy (const True) EndOfFileError

-- parse char according to condition, including message
psym :: (Char -> Bool) -> String -> Parser Char
psym cond msg = satisfy cond $ (UnexpectedError $ msg)

-- parse char according to condition with standard message
psym_ :: (Char -> Bool) -> Parser Char
psym_ cond = satisfy cond (UnexpectedError "Char doesnot satisfy condition")

-- parser for up / lower / alpha / alphanum char
upSym, lowSym, alSym, alnumSym :: Parser Char
upSym = psym isUpper "Can not parse symbol in upper case"
lowSym = psym isLower "Can not parse symbol in lower case"
alSym = psym isAlpha "Can not parse alphabetic symbol"
alnumSym = psym isAlphaNum "Can not parse alphanumeric symbol"

-- strings from lower / upper symbol
upSyms, lowSyms :: Parser String
upSyms = some $ psym isUpper "Can not parser string of symbols in upper case"
lowSyms = some $ psym isLower "Can not parser string of symbols in lower case"

-- parser char which is presented / not presented in string
oneOf, noneOf :: String -> Parser Char
oneOf str = satisfy (flip elem str) (UnexpectedError $ "Can not parse oneOf '" <> str <> "'")
noneOf str = satisfy (not . flip elem str) (UnexpectedError $ "Can not parse noneOf '" <> str <> "'")

-- parser for string
string :: String -> Parser String
string s = traverse (\ch -> satisfy (== ch) (UnexpectedError $ "Can not parse string '" <> s <> "'")) s

-- numS - helper, read number as STRING
numS :: String -> Parser String
numS str = some $ satisfy isDigit $ UnexpectedError str

-- sp / sps - space and spaces
-- ws / wss - white space (' ', '\n', '\t'), with many, sps1, wss1 - with some
sp, ws :: Parser Char
sp = psym (== ' ') "Can not parse space"
ws = psym (flip elem " \n\t") "Can not parse whitespace (space, newline or tab)"

sps, sps1, wss, wss1 :: Parser String
sps = many $ psym (== ' ') "Can not parse spaces"
wss = many $ psym (flip elem " \n\t") "Can not parse whitespaces (space, newline or tab)"
sps1 = some $ psym (== ' ') "Can not parse spaces"
wss1 = some $ psym (flip elem " \n\t") "Can not parse whitespaces (space, newline or tab)"

-- parser for integer number and signed integer number (signed includes number without sign)
num, sigNum :: Parser Int
num = read <$> numS "Can not parse integer number"
sigNum = num <|> (sym '+' *> num) <|> (sym '-' *> (negate <$> num))

-- parser for double number and signed double number (signed includes number without sign)
-- separator for number parts stored in Config
double, sigDouble :: Parser Double
double =
  Parser $
    asks doubleSep >>= \sep ->
      read . mconcat <$> (sequence $ map unparse [numS "Can not parse double number", string sep, numS "Can not parse double number"])
sigDouble = double <|> (sym '+' *> double) <|> (sym '-' *> (negate <$> double))

-- parser for list elements with separator
-- sepBy return [] in case of fail, sepBy1 needs at least 1 element for success
sepBy, sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy (Parser p) (Parser sep) = Parser $ do
  r <- try_ p
  case r of
    Left _  -> return []
    Right _ -> (:) <$> p <*> many (sep *> p)
sepBy1 (Parser p) (Parser sep) = Parser $ (:) <$> p <*> many (sep *> p)

-- HELPERS
--
-- return current char or throw exceptioin in case of end of input
peek :: Parser_ Char
peek = do
  (Input o _ s) <- get
  case o < T.length s of
    False -> modify (\(Input o' ((lp, l), c) s') -> (Input o' ((lp, l), c - 1) s')) >> getErrorPos >>= \(l, c) -> getErrorSrc >>= throwError . EndOfFileError '!' (l, c)
    True -> return $ T.index s o

-- move input one char further, in case of end of input position do not change
consumeInput :: Parser_ ()
consumeInput = do
  (Input o ((lo, l), c) s) <- get
  let o' = o + 1
      ch = T.index s o
  case ch of
    '\n' -> put (Input o' ((o', l + 1), 1) s) >> return ()
    '\t' -> asks tab >>= \t -> put (Input o' ((lo, l), c - (mod (c - 1) t) + t) s) >> return ()
    _ -> put (Input o' ((lo, l), c + 1) s) >> return ()

-- try parser in isolated monad transformer environment
try_ :: Parser_ t -> Parser_ (Either Error t)
try_ p = do
  c <- ask
  i <- get
  return $ (flip runReader c) ((evalStateT . runExceptT) p i)

-- get position of error for ErrorMessage
getErrorPos :: Parser_ (Int, Int)
getErrorPos = gets pos >>= \((_, l), c) -> return (l, c)

-- get string with error from source
getErrorSrc :: Parser_ String
getErrorSrc = do
  (Input _ ((l, _), c) s) <- get
  let sps = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ T.unpack s)
  return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
