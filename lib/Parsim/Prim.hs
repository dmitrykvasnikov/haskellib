{-# LANGUAGE FunctionalDependencies #-}

module Parsim.Prim
  ( Parsim (..),
    Source (..),
    Consumed (..),
    Stream,
    mzero,
    peek,
    eof,
    many,
    many1,
    (<|>),
    sepBy,
    sepBy1,
    between,
    withDefault,
    withDefaultCons,
    followedBy,
    notFollowedBy,
    choice,
    chain,
    token,
    parse,
    count,
  )
where

import           Control.Applicative (Alternative, empty, many, some, (<|>))
import           Control.Monad       (MonadPlus, mplus, mzero)
import           Parsim.Error
import           Parsim.Pos

class Stream i o | i -> o where
  peek :: i -> Maybe (o, i)

instance Stream [tok] tok where
  peek []       = Nothing
  peek (t : ts) = Just (t, ts)

data Source i = Source { sourcePos :: SourcePos
                       , source    :: i
                       }

instance Functor Source where
  fmap f (Source pos src) = Source pos (f src)

instance (Show i) => Show (Source i) where
  show (Source pos src) = show pos <> show src

data Consumed i o = Consumed o (Source i)
  deriving (Show)

type Result i o = Either ParsimError (Consumed i o)

instance {-# OVERLAPPING #-} (Show o, Show i) => Show (Result i o) where
  show (Left e)                 = show e
  show (Right (Consumed o src)) = "Result: " <> show o <> "\n" <> show src

newtype Parsim i o = Parsim { unParsim :: (Source i) -> Result i o }

instance Functor (Parsim i) where
  fmap f p = Parsim $ \src ->
    case unParsim p src of
      Left e                  -> Left e
      Right (Consumed o src') -> Right (Consumed (f o) src')

instance Applicative (Parsim i) where
  pure o = Parsim $ \src -> Right $ Consumed o src
  pab <*> pa = Parsim $ \src -> do
    Consumed ab src' <- unParsim pab src
    Consumed a src'' <- unParsim pa src'
    return $ Consumed (ab a) src''

instance Semigroup (Parsim i o) where
  (<>) p1 p2 = Parsim $ \src ->
    case unParsim p1 src of
      r1@(Right _) -> r1
      Left e1 -> case unParsim p2 src of
        r2@(Right _) -> r2
        Left e2      -> Left $ mergeError e1 e2

instance Monoid (Parsim i o) where
  mappend = (<>)
  mempty = mkParsimError "sys" "Mempty element"

instance Alternative (Parsim i) where
  empty = mempty
  (<|>) = (<>)
  some p = some_p
    where
      some_p = liftA2 (:) p many_p
      many_p = some_p <|> pure []
  many p = many_p
    where
      many_p = some_p <|> pure []
      some_p = liftA2 (:) p many_p

many1 :: Parsim i o -> Parsim i [o]
many1 = some

instance Monad (Parsim i) where
  return = pure
  pa >>= apb = Parsim $ \src -> do
    Consumed a src' <- unParsim pa src
    unParsim (apb a) src'

instance MonadPlus (Parsim i) where
  mzero = Parsim $ \src -> Left $ ParsimError (sourcePos src) [Message $ "Empty parser error\n"]
  mplus = (<|>)

-- returns default value if parser fails

-- * doesn't consume input stream

withDefault :: o -> Parsim i o -> Parsim i o
withDefault def p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed def src
    r      -> r

-- * consume input stream

withDefaultCons :: o -> (Source i -> o -> Source i) -> Parsim i o -> Parsim i o
withDefaultCons def toPos p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed def $ toPos src def
    r      -> r

-- end of file

eof :: (Stream i o) => Parsim i ()
eof = Parsim $ \s@(Source pos src) ->
  case peek src of
    Nothing  -> Right $ Consumed () s
    (Just _) -> Left $ ParsimError pos [Expect $ "Expected end of file\n"]

-- chain with provided function. Raise error in case of empty parser list
-- Execut the only parser if only one parser in the list
chain :: (o -> o -> o) -> [Parsim i o] -> Parsim i o
chain _ []       = undefined
chain _ [p]      = p
chain f (p : ps) = foldl (liftA2 f) p ps

-- choice is alternative to <|>
choice :: [Parsim o i] -> Parsim o i
choice = foldr (<|>) mzero

-- try parser N times
count :: Int -> Parsim i o -> Parsim i [o]
count n p
  | n < 1 = pure []
  | otherwise = sequence $ replicate n p

sepBy :: Parsim i a -> Parsim i b -> Parsim i [a]
sepBy p sep = liftA2 (:) p (many (sep *> p)) <|> pure []

sepBy1 :: Parsim i a -> Parsim i b -> Parsim i [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

between :: Parsim i o1 -> Parsim i o2 -> Parsim i o -> Parsim i o
between p1 p2 p = p1 *> p <* p2

-- check parser without consuming input
followedBy :: (Stream i o) => Parsim i o -> Parsim i ()
followedBy p = Parsim $ \src ->
  case unParsim p src of
    Right _ -> Right $ Consumed () src
    Left e  -> Left e

notFollowedBy :: (Stream i o) => Parsim i o -> Parsim i ()
notFollowedBy p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed () src
    Right _ -> Left $ mkError "msg" (sourcePos src) ("notFollowedBy parser failed")

token ::
  (Stream i t) =>
  (t -> Maybe o) ->
  (SourcePos -> t -> SourcePos) ->
  (Source i -> t -> ParsimError) ->
  Parsim i o
token test toPos err = Parsim $ \s@(Source pos i) ->
  case peek i of
    Nothing -> Left $ ParsimError pos [Unexpect "Unexpected end of input"]
    Just (tok, toks) -> case test tok of
      Nothing -> Left $ err s tok
      Just o  -> Right $ Consumed o (Source (toPos pos tok) toks)

streamFromString :: String -> Source String
streamFromString str = Source (SourcePos "Custom string" 1 1) str

streamFromFile :: FilePath -> IO (Source String)
streamFromFile f = do
  content <- readFile f
  return $ Source (SourcePos ("File :" <> f) 1 1) content

-- Error types: "sys" - System, "msg" - Message
mkParsimError :: String -> String -> Parsim i o
mkParsimError errType str = Parsim $ \src -> Left $ mkError errType (sourcePos src) str

parse :: Parsim String o -> String -> Result String o
parse p str = unParsim p (streamFromString str)
