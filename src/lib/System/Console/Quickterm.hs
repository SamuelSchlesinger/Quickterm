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

fst5 :: (a,b,c,d,e) -> a
fst5 (a,_,_,_,_) = a

snd5 :: (a,b,c,d,e) -> b
snd5 (_,b,_,_,_) = b

verboseHelp :: [(IO (), Int, Help, [String], [String])] -> IO ()
verboseHelp ts = do
  let trav i ts = case ts of
        []                 -> return ()
        ((_,_,_,pi,_):ts') -> do
          putStrLn ("[" ++ show i ++ "] " ++ (unwords . reverse) pi)
          trav (i+1) ts'
  when (not (null ts)) $ do
    putStrLn "Did you mean one of these?"
    trav 1 (take 10 ts)
    putStr "[0 to quit]: "
    hFlush stdout
    l <- getLine
    when (l =~ "(1|2|3|4|5|6|7|8|9)") $ do
      let i = read l
      when (i > 0 && i < 10) $
        case ts !! (i - 1) of
          (a,i,h,_,_) ->
            if   i /= 0
            then putStrLn (h 0)
            else putStrLn "" >> a

-- |Runs a quickterm application.
quickterm :: Quickterm (IO ()) -> [String] -> IO ()
quickterm qt as = case as of
    "-v":as -> verbose True  as
    as      -> verbose False as
  where
    ts = runQuickterm qt 0 (const "") []
    verbose v as =
      let ts' = ts as
       in f v ts' . filter (\(_,i,_,_,rs) -> i == 0 && null rs) $ ts'
    f v ts rs = case rs of
      []  -> do
        putStrLn "Could not match arguments to a command:"
        putStrLn (">> " ++ unwords (if v then tail as else as) ++ " <<")
        when v (verboseHelp ts)
      [r] -> fst5 r
      _   -> error "ambiguous call"

qtMain :: Quickterm (IO ()) -> IO ()
qtMain qt = quickterm qt =<< getArgs
