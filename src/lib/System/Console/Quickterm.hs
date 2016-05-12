module System.Console.Quickterm
    ( Quickterm (..)
    , param
    , exact
    , Description (..)
    , desc
    , section
    , program
    , quickterm
    ) where

import System.Console.Quickterm.CanMarshall
import System.Console.Quickterm.Help
import System.Console.Quickterm.Deserializer

import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Data.Char
import Data.Foldable (asum)
import Data.List (intercalate,sortBy)
import Data.Ord (comparing)
import Text.EditDistance
import Text.Regex.Base hiding (empty)
import Text.Regex.TDFA hiding (empty)
import System.IO (hFlush,stdout)


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
  f <*> m = Quickterm $ \i h pi as ->
    runQuickterm f i h pi as >>= \(g,i' ,h' , pi', as' ) ->
      runQuickterm m i' h' pi' as' >>= \(a,i'',h'', pi'',as'') ->
        return (g a,i'',h'',pi'',as'')

instance Alternative Quickterm where
  empty = Quickterm (const (const (const (const empty))))
  m <|> n = Quickterm $ \i h pi as ->
    runQuickterm m i h pi as <|> runQuickterm n i h pi as

instance Monad Quickterm where
  return = pure
  m >>= f = Quickterm $ \i h pi as ->
    runQuickterm m i h pi as >>= \(a,i',h',pi',as') ->
      runQuickterm (f a) i' h' pi' as'

instance MonadPlus Quickterm where
  mzero = empty
  mplus = (<|>)

-- |Handles the marshalling from cmd-line argument to a Haskell value in
-- |Quickterm-syntax.
param :: (Show a, CanMarshall a) => Quickterm a
param = Quickterm $ \i h pi as -> case as of
  []      -> [(defaultM,i+10,h,pi,[])]
  (a:as') -> deserialize deserializer a 0 >>= \(a, _, i') ->
    return (a, i + i', h, show a:pi, as')

-- TODO: Remove wiping behaviour of 'exact' and implement maximum depth for
--       recursive calls.
-- |Enforces exact string matching.
exact :: String -> Quickterm String
exact s = mfilter (==s) param

-- |A simple description for a section.
data Description = Description
  { -- | The name of a section.
    nameD :: String
  , -- | The description of a section.
    longD :: Help
  }

-- |Creates a description.
desc :: String -> Description
desc n = Description n (const "")

-- |Creates a section Quickterm.
section :: Description -> [Quickterm a] -> Quickterm a
section (Description n h) qs = Quickterm $ \i h pi as ->
  let h' i = h i ++ "\n" ++ indent n i
      leven = levenshteinDistance defaultEditCosts
   in case as of
        []      -> qs >>= \m -> runQuickterm m (i + 10) h (n:pi) []
        (a:as') -> qs >>= \m ->
          runQuickterm m (i + leven n a) h' (n:pi) as'

-- |Creates a program Quickterm.
program :: [Quickterm a] -> Quickterm a
program qs = Quickterm $ \i h pi as -> qs >>= \m -> runQuickterm m i h pi as

-- |Runs a quickterm application.
quickterm :: Quickterm (IO ()) -> [String] -> IO ()
quickterm qt as = f . filter (\(_, i, _, _, rs) -> i == 0 && null rs) $ ts
  where
    snd5 :: (a,b,c,d,e) -> b
    snd5 (a,b,c,d,e) = b
    f rs = case rs of
      []                 -> case sortBy (comparing snd5) ts of
        [] -> error "No match could be found."
        ts  ->
          let f i []                 =
                return ()
              f i ((_,_,_,pi,_):ts') =
                putStrLn ("[" ++ show i ++ "] " ++ getPi pi) >> f (i+1) ts'
              getPi = unwords . reverse
           in putStrLn "Could not match arguments to a command:" >>
              putStrLn (">> " ++ unwords as ++ " <<") >>
              putStrLn "Did you mean one of these?" >>
              f 1 (take 10 ts) >> putStr "[0 to quit]: " >>
              hFlush stdout >> getLine >>= \l ->
                when (l =~ "(1|2|3|4|5|6|7|8|9|10)") $
                  putStrLn (case ts !! read l of (_,_,h,_,_) -> h 0)
      [r@(a,_,_,_,_)]    -> a
      (  (_,_,_,_,_):_ ) -> error "TODO: generate ambiguous call error message"
    ts = runQuickterm qt 0 (const "") [] as
