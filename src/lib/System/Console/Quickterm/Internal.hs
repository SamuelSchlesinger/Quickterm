module System.Console.Quickterm.Internal
    ( Quickterm (..)
    , param
    ) where

import           System.Console.Quickterm.CanMarshall
import           System.Console.Quickterm.Deserializer
import           System.Console.Quickterm.Help

import           Control.Applicative
import           Control.Monad

-- |Quickterm represents a non-deterministic calculation of a most predictable
-- |command based on a breadth-first parsing strategy. The Quickterm is applied
-- |to a [String] to achieve parsing of command line arguments.
newtype Quickterm a = Quickterm
  { runQuickterm :: Int
                 -> Help
                 -> [String]
                 -> [String]
                 -> [(a, Int, Help, [String], [String])]
  }

instance Functor Quickterm where
  fmap f m = Quickterm $ \i h pi as ->
    fmap (\(a,i',h',pi',as') -> (f a,i',h',pi',as'))
         (runQuickterm m i h pi as)

instance Applicative Quickterm where
  pure a = Quickterm $ \i h pi as ->
    pure (a,i,h,pi,as)
  f <*> m = Quickterm $ \i h pi as -> join $
    (\(g,i',h', pi',as') -> (\(a,i'',h'', pi'',as'') -> (g a,i'',h'',pi'',as''))
      <$> runQuickterm m i' h' pi' as')
      <$> runQuickterm f i h pi as

instance Alternative Quickterm where
  empty = Quickterm (const (const (const (const empty))))
  m <|> n = Quickterm $ \i h pi as -> filter (\(_,i,_,_,_) -> i < 1000) $
    runQuickterm m i h pi as <|> runQuickterm n i h pi as

instance Monad Quickterm where
  return = pure
  m >>= f = Quickterm $ \i h pi as ->
    runQuickterm m i h pi as >>= \(a,i',h',pi',as') ->
      runQuickterm (f a) i' h' pi' as'

instance MonadPlus Quickterm where
  mzero = empty
  mplus = (<|>)

param :: (CanMarshall a) => Quickterm a
param = Quickterm $ \i h pi as -> case as of
      []      -> let d = defaultM in [(d,i+10,h,asInput d:pi,[])]
      (a:as') -> deserialize deserializer a 0 >>= \(a, _, i') ->
        return (a, i + i', h, asInput a:pi, as')
