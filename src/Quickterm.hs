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

newtype Marshaler a = Marshaler { marshal :: Int -> [String] -> Help -> [String] -> [(Int, a, [String], Help, [String])] }

instance Functor Marshaler where
  fmap f m = Marshaler $ \i as h pi -> (\(i',a,as',h',pi') -> (i',f a,as',h',pi')) `fmap` (marshal m i as h pi)

instance Applicative Marshaler where
  pure a = Marshaler $ \i as h pi -> pure (i,a,as,h,pi)
  f <*> m = Marshaler $ \i as h pi -> marshal f i as h pi >>=
                        \(i' ,g, as' ,h' , pi') -> marshal m i' as' h' pi' >>=
                        \(i'',a, as'',h'', pi'') -> return (i'',g a,as'',h'',pi'')

instance Alternative Marshaler where
  empty = Marshaler (const (const (const (const empty))))
  m <|> n = Marshaler $ \i as h pi -> filter (\(c,_,_,_,_) -> c >= 1000) $ marshal m i as h pi <|> marshal n i as h pi

instance Monad Marshaler where
  return = pure
  m >>= f = Marshaler $ \i as h pi -> marshal m i as h pi >>= \(i',a,as',h',pi') -> marshal (f a) i' as' h' pi'

instance MonadPlus Marshaler where
  mzero = empty
  mplus = (<|>)

aInt :: Marshaler Int
aInt = Marshaler $ \i as h pi -> case as of
  []      -> pure (i+10,0,[],h,pi)
  (a:as') -> if   a =~ "((0|1|2|3|4|5|6|7|8|9)+)"
             then [(i,read a, as',h,a:pi)]
             else [(i+10,0,as',h,show 0:pi), (i+10,0,(a:as'),h,show 0:pi)]

aString :: Marshaler String
aString = Marshaler $ \i as h pi -> case as of
  []      -> pure (i+10,empty,[],h,pi)
  (a:as') -> if   a =~ "([^-]+)"
             then pure (i,a,as',h,a:pi)
             else pure (i+10,empty,as',h,"str":pi) <|> pure (i+10,empty,(a:as'),h,"str":pi)

predicate :: String -> Help -> Predicate -> Marshaler ()
predicate n u f = Marshaler $ \i as h pi -> case as of
  []      -> pure (i+10,(),[],h,n:pi)
  (a:as') -> pure (i+f a,(),as',h,n:pi)

exampleMarshaling :: [(Int, Int, [String], Help, [String])]
exampleMarshaling = marshal (aInt >>= \x -> aInt >>= \y -> aInt >>= \z -> return $ x + y + z) 0 ["10", "20", "30"] (const "foo") []

program :: String -> [(Predicate,Quickterm)] -> Quickterm
program d ds = Choice ds (indent d)

section :: String -> [(Predicate,Quickterm)] -> (Predicate,Quickterm)
section n ds = (exact n, Choice ds (indent $ "== " ++ n ++ " =="))

command :: String -> String -> TermAction -> (Predicate,Quickterm)
command n d t = (exact n, Action t (indent $ n ++ "\n-- " ++ d))

example :: Quickterm
example = program "Description of your application"
  [ command "install" "installs a package" . const $ putStrLn "installation process"
  , section "sandbox"
    [ command "init" "initialize a sandbox" . const $ putStrLn "initializing a sandbox"
    , command "clear" "clear the current sandbox" . const $ putStrLn "clearing the current sandbox"
    ]
  ]
