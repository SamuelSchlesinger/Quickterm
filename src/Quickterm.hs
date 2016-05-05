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

defaultWs :: String -> Int -> String
defaultWs a i = intercalate "\n" ((ws i ++) <$> splitLn a)

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

l = levenshteinDistance defaultEditCosts

example :: Quickterm
example = Choice
  [ (l "install", Action (const $ putStrLn "installation process") . defaultWs $ "install -- install a package")
  , (l "sandbox", Choice
    [ (l "init", Action (const $ putStrLn "initializing a sandbox") . defaultWs $ "init\n  -- initialize a sandbox")
    , (l "clear", Action (const $ putStrLn "clearing the current sandbox") . defaultWs $ "clear\n  -- clear the current sandbox")
    ] . defaultWs $ "sandbox -- Sandbox related actions.")
  ] . defaultWs $ "Description of your application"
