module Quickterm
    (
    ) where

import Data.Foldable (asum)
import Text.EditDistance
import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Data.List (intercalate)
import Data.Char
import Text.Regex.Base hiding (empty)
import Text.Regex.TDFA hiding (empty)

type Predicate = String -> Int
type TermAction = [String] -> IO ()
type Help = Int -> String

data Quickterm
  = Choice [(Predicate, Quickterm)] Help
  | Action TermAction Help

runQuickterm :: Quickterm -> [String] -> [(Int, [String] -> IO ())]
runQuickterm (Action t  _) []     = pure (0, t)
runQuickterm c@Choice{} []        = pure (0, const . putStrLn $ makeHelp 0 1 c)
runQuickterm (Action t  _) _      = pure (0, t)
runQuickterm (Choice ps _) (a:as) =
  ps >>= \(p,q) ->
  runQuickterm q as >>= \(i, io) ->
  return (p a + i, io)

makeHelp :: Int -> Int -> Quickterm -> String
makeHelp l i (Choice ps u) =
  if   i >= 0
  then u l ++ "\n" ++ intercalate (ws i ++ "\n") (makeHelp (l+1) (i - 1) . snd <$> ps)
  else u l
makeHelp l _ (Action _ u) = u l

ws :: Int -> String
ws l = replicate (l*2) ' '

indent :: String -> Int -> String
indent a i = intercalate "\n" ((ws i ++) <$> splitLn a)

splitLn :: String -> [String]
splitLn = f []
  where
    f rs []        = [reverse rs]
    f rs ('\n':ss) = reverse rs : f [] ss
    f rs (s:ss)    = f (s:rs) ss

execQuickterm :: Quickterm -> [String] -> IO ()
execQuickterm q as = void . sequence $ (\q -> q as) <$> valid
  where
    rs = runQuickterm q as
    valid = snd <$> filter (\(i,_) -> i == 0) rs

regexPenalty :: Bool -> Int
regexPenalty True = 0
regexPenalty _    = 10

regex :: String -> String -> Int
regex r = regexPenalty . (=~ r)

exact :: String -> String -> Int
exact = levenshteinDistance defaultEditCosts

section :: String -> [(Predicate,Quickterm)] -> (Predicate,Quickterm)
section n ds = (exact n, Choice ds (indent $ "== " ++ n ++ " =="))

command :: String -> String -> TermAction -> (Predicate,Quickterm)
command n d t = (exact n, Action t (indent $ n ++ "\n-- " ++ d))

newtype Marshaler f a = Marshaler { marshal :: [String] -> f (a, [String]) }

instance (Functor f) => Functor (Marshaler f) where
  fmap f m = Marshaler $ \as -> first f `fmap` (marshal m as)

instance (Monad f) => Applicative (Marshaler f) where
  pure a = Marshaler $ \as -> pure (a,as)
  f <*> m = Marshaler $ \as -> marshal f as >>= \(g, as') -> marshal m as' >>= \(a, as'') -> return (g a, as'')

instance (MonadPlus f) => Alternative (Marshaler f) where
  empty = Marshaler $ const empty
  m <|> n = Marshaler $ \as -> marshal m as <|> marshal n as

instance (Monad f) => Monad (Marshaler f) where
  return = pure
  m >>= f = Marshaler $ \as -> marshal m as >>= \(a, as') -> marshal (f a) as'

instance (MonadPlus f) => MonadPlus (Marshaler f) where
  mzero = empty
  mplus = (<|>)

aInt :: (Alternative f) => Marshaler f Int
aInt = Marshaler $ \as -> case as of
  []     -> empty
  (a:as) -> if   a =~ "((0|1|2|3|4|5|6|7|8|9)+)"
            then pure (read $ a, as)
            else empty

aString :: (Alternative f) => Marshaler f String
aString = Marshaler $ \as -> case as of
  []     -> empty
  (a:as) -> if   a =~ "([^-]+)"
            then pure (a, as)
            else empty

exampleMarshaling :: (MonadPlus f) => f (Int, [String])
exampleMarshaling = marshal ((\x y z -> x + y + z) <$> (aInt >>= return . (* 2)) <*> aInt <*> aInt) ["10", "20", "30"]

genericPenalty :: (Foldable f) => f a -> Int
genericPenalty = regexPenalty . not . null

m2pred :: (MonadPlus f, Foldable f) => Marshaler f a -> Predicate
m2pred m s = genericPenalty (marshal m [s])

program :: String -> [(Predicate,Quickterm)] -> Quickterm
program d ds = Choice ds (indent d)

example :: Quickterm
example = program "Description of your application"
  [ command "install" "installs a package" . const $ putStrLn "installation process"
  , section "sandbox"
    [ command "init" "initialize a sandbox" . const $ putStrLn "initializing a sandbox"
    , command "clear" "clear the current sandbox" . const $ putStrLn "clearing the current sandbox"
    ]
  ]
