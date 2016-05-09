module Deserializer
    ( Deserializer (..)
    , tryConvert
    ) where

import Control.Applicative
import Control.Monad


-- |Deserializers are used in marshaling process of cmd-line parameters.
newtype Deserializer a = Deserializer { deserialize :: [Char] -> Int -> [(a,[Char],Int)] }

instance Functor Deserializer where
  fmap f d = Deserializer $ \st p -> fmap (\(a,st',p') -> (f a,st',p')) (deserialize d st p)

instance Applicative Deserializer where
  pure a = Deserializer $ \st p -> [(a,st,p)]
  f <*> d = Deserializer $ \st p -> deserialize f st p
                         >>= \(g,st',p') -> deserialize d st' p'
                         >>= \(a,st'',p'') -> return (g a,st'',p'')

instance Alternative Deserializer where
  empty = Deserializer (const (const empty))
  d <|> h = Deserializer $ \st p -> deserialize d st p <|> deserialize h st p

instance Monad Deserializer where
  return = pure
  d >>= f = Deserializer $ \st p -> deserialize d st p >>= \(d',st',p') -> deserialize (f d') st' p'

instance MonadPlus Deserializer where
  mzero = empty
  mplus = (<|>)

-- |A pure computation abstraction layer for the Deserializer.
tryConvert :: (String -> [(a,Int)]) -> Deserializer a
tryConvert f = Deserializer $ \st p -> (\(a,i) -> (a,[],p+i)) <$> f st
