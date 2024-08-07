{-# LANGUAGE FunctionalDependencies #-}

module Parsim.Prim
  ( Parsim (..),
    Source (..),
    Consumed (..),
    Stream,
    mzero,
    peek,
    many,
    many1,
    (<|>),
    (<!>),
    token,
    parse,
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
  mempty = mkParsimError "Mempty element"

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

instance Monad (Parsim i) where
  return = pure
  pa >>= apb = Parsim $ \src -> do
    Consumed a src' <- unParsim pa src
    unParsim (apb a) src'

instance MonadPlus (Parsim i) where
  mzero = Parsim $ \src -> Left $ ParsimError (sourcePos src) [Message $ "Empty parser error\n"]
  mplus = (<|>)

-- returns default value if parser fails
(<!>) :: o -> Parsim i o -> Parsim i o
(<!>) def p = Parsim $ \src ->
  case unParsim p src of
    Left _ -> Right $ Consumed def src
    r      -> r

many1 :: Parsim i o -> Parsim i [o]
many1 = some

token ::
  (Stream i t) =>
  (SourcePos -> t -> SourcePos) ->
  (t -> Maybe o) ->
  (Source i -> t -> ParsimError) ->
  Parsim i o
token toPos test err = Parsim $ \s@(Source pos i) ->
  case peek i of
    Nothing -> Left $ ParsimError pos [Unexpect "Unexpected end of input"]
    Just (tok, toks) -> case test tok of
      Nothing -> Left $ err s tok
      Just o  -> Right $ Consumed o (Source (toPos pos tok) toks)

streamFromString :: String -> Source String
streamFromString str = Source (SourcePos "Custom string" 1 1) str

mkParsimError :: String -> Parsim i o
mkParsimError str = Parsim $ \src -> Left $ mkError (sourcePos src) str

parse :: Parsim String o -> String -> Result String o
parse p str = unParsim p (streamFromString str)
