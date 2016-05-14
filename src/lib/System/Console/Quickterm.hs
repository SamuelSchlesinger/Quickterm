module System.Console.Quickterm
    ( module Export
    , flag
    , flag_
    , command
    , command_
    , Description (..)
    , section
    , program
    , quickterm
    , qtMain
    ) where

import           Control.Applicative
import           Control.Monad

import           Data.Char
import           Data.Foldable                         (asum)
import           Data.List                             (intercalate, sortBy)
import           Data.Ord                              (comparing)

import           System.Environment                    (getArgs)
import           System.IO                             (hFlush, stdout)

import           Text.EditDistance
import           Text.Regex.Base                       hiding (empty)
import           Text.Regex.TDFA                       hiding (empty)

import           System.Console.Quickterm.CanMarshall  as Export
import           System.Console.Quickterm.Description  as Export
import           System.Console.Quickterm.Deserializer as Export
import           System.Console.Quickterm.Help         as Export
import           System.Console.Quickterm.Internal     as Export


flag :: (IsDescription d, CanMarshall a) => d -> Quickterm a
flag d = param >>= \n ->
  let d' = toDescription d
   in if   nameD d' == n
      then param
      else empty

flag_ :: (IsDescription d) => d -> Quickterm ()
flag_ d = param >>= \n' ->
  let d' = toDescription d
   in if nameD d' == n'
      then pure ()
      else empty

command :: (IsDescription d) => d -> Quickterm a -> Quickterm a
command n c = section n [c]

command_ :: (IsDescription d) => d -> a -> Quickterm a
command_ n c = command n (pure c)

-- |Creates a section Quickterm.
section :: (IsDescription d) => d -> [Quickterm a] -> Quickterm a
section d qs = Quickterm $ \i h pi as ->
  let n     = nameD $ toDescription d
      h' i  = h i ++ "\n" ++ indent n i
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

qtMain :: Quickterm (IO ()) -> IO ()
qtMain qt = quickterm qt =<< getArgs
