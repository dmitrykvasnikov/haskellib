module Yap.Parser where

import           Control.Applicative              (Alternative, asum, empty,
                                                   some, (<|>))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader       (Reader, ask, asks, runReader)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get,
                                                   gets, modify, put)
import           Data.Char                        (isDigit)
import qualified Data.Text                        as T (index, length, unpack)
import           Yap.Config
import           Yap.Error
import           Yap.Input

type Parser_ = ExceptT Error (StateT Input (Reader Config))

newtype Parser t = Parser { unparse :: Parser_ t }
  deriving (Applicative, Functor, Monad)

-- return error with longest consumed input
instance Alternative Parser where
  empty = Parser $ getErrorPos >>= \p -> getErrorSrc >>= throwE . InternalError "Internal error for empty instance of Applicative" p
  (Parser p1) <|> (Parser p2) = Parser $ do
    t1 <- try_ p1
    case t1 of
      Right _ -> p1 >>= return
      Left e1 -> do
        t2 <- try_ p2
        case t2 of
          Right _ -> p2 >>= return
          Left e2 -> if e1 < e2 then throwE e2 else throwE e1

-- try parser in isolated monad transformer
try_ :: Parser_ t -> Parser_ (Either Error t)
try_ p = do
  c <- lift . lift $ ask
  i <- lift get
  return $ (flip runReader c) ((evalStateT . runExceptT) p i)

satisfy :: (Char -> Bool) -> (Char -> (Int, Int) -> String -> Error) -> Parser Char
satisfy cond err = Parser $ do
  ch <- peek
  case cond ch of
    True  -> consumeInput >> return ch
    False -> getErrorPos >>= \p -> getErrorSrc >>= throwE . err ch p

sym :: Char -> Parser Char
sym ch = satisfy (== ch) (UnexpectedError $ "Can not parse symbol '" <> [ch] <> "'")

anySym :: Parser Char
anySym = satisfy (const True) EndOfFileError

string :: String -> Parser String
string s = traverse (\ch -> satisfy (== ch) (UnexpectedError $ "Can not parse string '" <> s <> "'")) s

-- numS - helper, read number as STRING
numS :: String -> Parser String
numS str = some $ satisfy isDigit $ UnexpectedError str

num, sigNum :: Parser Int
num = read <$> numS "Can not parse integer number"
sigNum = num <|> (sym '+' *> num) <|> (sym '-' *> (negate <$> num))

double, sigDouble :: Parser Double
double =
  Parser $
    (lift . lift . asks $ doubleSep) >>= \sep ->
      read . mconcat <$> (sequence $ map unparse [numS "Can not parse double number", string sep, numS "Can not parse double number"])
sigDouble = double <|> (sym '+' *> double) <|> (sym '-' *> (negate <$> double))

peek :: Parser_ Char
peek = do
  (Input o _ s) <- lift get
  case o < T.length s of
    False -> (lift . modify $ \(Input o' ((lp, l), c) s') -> (Input o' ((lp, l), c - 1) s')) >> getErrorPos >>= \(l, c) -> getErrorSrc >>= throwE . EndOfFileError '!' (l, c)
    True -> return $ T.index s o

consumeInput :: Parser_ ()
consumeInput = do
  (Input o ((lo, l), c) s) <- lift get
  let o' = o + 1
      ch = T.index s o
  case ch of
    '\n' -> (lift . put $ (Input o' ((o', l + 1), 1) s)) >> return ()
    '\t' -> (lift . lift . asks $ tab) >>= \t -> (lift . put $ (Input o' ((lo, l), c - (mod (c - 1) t) + t) s)) >> return ()
    _ -> (lift . put $ (Input o' ((lo, l), c + 1) s)) >> return ()

getErrorPos :: Parser_ (Int, Int)
getErrorPos = lift . gets $ pos >>= \((_, l), c) -> return (l, c)

getErrorSrc :: Parser_ String
getErrorSrc = do
  (Input _ ((l, _), c) s) <- lift get
  let sps = take (c - 1) (repeat ' ')
      src = takeWhile (/= '\n') (drop l $ T.unpack s)
  return $ sps <> "*\n" <> src <> "\n" <> sps <> "*\n"
