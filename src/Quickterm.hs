{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

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
import Text.Regex.Base
import Text.Regex.TDFA

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
