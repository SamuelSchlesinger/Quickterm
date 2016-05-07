{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Quickterm
    (
    ) where

import Data.Foldable (asum)
import Text.EditDistance
import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Data.Ord (comparing)
import Data.List (intercalate,sortBy)
import Data.Char
import Text.Regex.Base hiding (empty)
import Text.Regex.TDFA hiding (empty)

type Predicate = String -> Int
type TermAction = [String] -> IO ()
type Help = Int -> String

-- |A simple whitespace generator.
ws :: Int -> String
ws l = replicate (l*2) ' '

-- |A whitespace manipulation function indenting the whole text block.
indent :: String -> Int -> String
indent a i = intercalate "\n" ((ws i ++) <$> splitLn a)

-- |Splits a String to a [String] based on \n.
splitLn :: String -> [String]
splitLn = f []
  where
    f rs []        = [reverse rs]
    f rs ('\n':ss) = reverse rs : f [] ss
    f rs (s:ss)    = f (s:rs) ss

-- |Quickterm represents a non-deterministic calculation of a most predictable command based on a breadth-first parsing
-- |strategy. The Quickterm is applied to a [String] to achieve parsing of command line arguments.
newtype Quickterm a = Quickterm { runQuickterm :: Int -> Help -> [String] -> [String] -> [(a, Int, Help, [String], [String])] }

instance Functor Quickterm where
  fmap f m = Quickterm $ \i h pi as -> (\(a,i',h',pi',as') -> (f a,i',h',pi',as')) `fmap` (runQuickterm m i h pi as)

instance Applicative Quickterm where
  pure a = Quickterm $ \i h pi as -> pure (a,i,h,pi,as)
  f <*> m = Quickterm $ \i h pi as -> runQuickterm f i h pi as
                      >>= \(g,i' ,h' , pi', as' ) -> runQuickterm m i' h' pi' as'
                      >>= \(a,i'',h'', pi'',as'') -> return (g a,i'',h'',pi'',as'')

instance Alternative Quickterm where
  empty = Quickterm (const (const (const (const empty))))
  m <|> n = Quickterm $ \i h pi as -> runQuickterm m i h pi as <|> runQuickterm n i h pi as

instance Monad Quickterm where
  return = pure
  m >>= f = Quickterm $ \i h pi as -> runQuickterm m i h pi as >>= \(a,i',h',pi',as') -> runQuickterm (f a) i' h' pi' as'

instance MonadPlus Quickterm where
  mzero = empty
  mplus = (<|>)

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

-- |Handles marshaling from a cmd-line argument to a Haskell data type.
class CanMarshal a where
  -- |A default value for the generic atomic operation 'param'.
  defaultM :: a
  -- |A help description for the generic atomic operation 'param'.
  helpU :: a -> Int -> String
  -- |A deserializer declaration for the generic atomic operation 'param'.
  deserializer :: Deserializer a

instance CanMarshal Int where
  defaultM = 0
  helpU _ = indent "<Integer>"
  deserializer = tryConvert $ \st ->
    if   st =~ "((0|1|2|3|4|5|6|7|8|9)+)"
    then [(read $ st,0)]
    else [(0,length st * 2)]

instance CanMarshal String where
  defaultM = "str"
  helpU _ = indent "<String>"
  deserializer = tryConvert $ \st ->
    if   st =~ "([^-]+)"
    then [(st,0)]
    else [("str",length st * 2)]

-- |Handles the marshaling from cmd-line argument to a Haskell value in Quickterm-syntax.
param :: (Show a, CanMarshal a) => Quickterm a
param = Quickterm $ \i h pi as -> case as of
  []      -> [(defaultM,i+10,h,pi,[])]
  (a:as') -> deserialize deserializer a 0 >>= \(a, _, i') -> return (a, i + i', h, show a:pi, as')

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
  let h' = \i -> h i ++ "\n" ++ indent n i
   in case as of
        []      -> qs >>= \m -> runQuickterm m (i + 10) h (n:pi) []
        (a:as') -> qs >>= \m -> runQuickterm m (i + levenshteinDistance defaultEditCosts n a) h' (n:pi) as'

-- |Creates a program Quickterm.
program :: [Quickterm a] -> Quickterm a
program qs = Quickterm $ \i h pi as -> qs >>= \m -> runQuickterm m i h pi as

-- |Runs a quickterm application.
quickterm :: Quickterm (IO ()) -> [String] -> IO ()
quickterm qt as = f . filter (\(_, i, _, _, rs) -> i == 0 && rs == []) $ ts
  where
    snd5 :: (a,b,c,d,e) -> b
    snd5 (a,b,c,d,e) = b
    f rs = case rs of
      []                 -> case sortBy (comparing snd5) ts of
        [] -> error "No match could be found."
        ts  ->
          let f i []                = return ()
              f i ((_,_,_,pi,_):ts') = putStrLn ("[" ++ show i ++ "] " ++ getPi pi) >> f (i+1) ts'
              getPi = intercalate " " . reverse
           in putStrLn "Could not match arguments to a command:" >>
              putStrLn (">> " ++ intercalate " " as ++ " <<") >>
              putStrLn "Did you mean one of these?" >> f 1 (take 10 ts) >> putStr "[0 to quit]: " >> getLine >>= \l ->
                if   l =~ "(1|2|3|4|5|6|7|8|9|10)"
                then putStrLn (case ts !! read l of (_,_,h,_,_) -> h 0)
                else return ()
      (r@(a,_,_,_,_):[]) -> a
      (  (_,_,_,_,_):_ ) -> error "TODO: generate ambiguous call error message"
    ts = runQuickterm qt 0 (const "") [] as

-- |Is a command line argument set for installation.
data InstallConfig
  = InstallConfig
    { bindir :: String
    , docdir :: String
    , datadir :: String
    , builddir :: String
    } deriving (Show, Eq)

-- |Default values for installation configuration.
defaultInstallConfig :: InstallConfig
defaultInstallConfig = InstallConfig
    { bindir = "/default/bindir"
    , docdir = "/default/docdir"
    , datadir = "/default/datadir"
    , builddir = "/default/builddir"
    }

-- |Defines the parsing process of command line arguments in relation to InstallConfig.
installConfig :: InstallConfig -> Quickterm InstallConfig
installConfig config =   pure config
                     <|> (exact "--bindir" >> param >>= \p -> installConfig (config { bindir = p }))
                     <|> (exact "--docdir" >> param >>= \p -> installConfig (config { docdir = p }))
                     <|> (exact "--datadir" >> param >>= \p -> installConfig (config { datadir = p }))
                     <|> (exact "--builddir" >> param >>= \p -> installConfig (config { builddir = p }))

-- |Example program for parsing command line arguments.
foo :: Quickterm (IO ())
foo = program
  [ section (desc "install")
    [ cmdInstall <$> installConfig defaultInstallConfig -- default values could be loaded from a config file
    ]
  , section (desc "sandbox")
    [ section (desc "init")
      [ pure cmdSandboxInit
      ]
    , (const cmdSandboxHelp) <$> exact "--help"
    , (const cmdSandboxSnapshot) <$> exact "--snapshot"
    ]
  ]

qt as = do
  putStrLn $ "=== " ++ show as ++ " ==="
  quickterm foo as
  putStrLn ""

demo :: IO ()
demo = do
  qt ["install"]
  qt ["install", "--bindir", "./my/local/bindir"]
  qt ["install", "--datadir", "./mylocal/datadir"]
  qt ["install", "--builddir", "./my/local/builddir", "--bindir", "./my/local/bindir", "--datadir", "./my/local/datadir"]
  qt ["sandbox", "init"]
  qt ["sandbox", "--help"]
  qt ["sandbox", "--snapshot"]

-- |Simple application module.
cmdSandboxSnapshot :: IO ()
cmdSandboxSnapshot = do
  putStrLn "Creating a snapshot..."
  putStrLn "Done!"
  putStrLn ""

-- |Simple application module.
cmdSandboxHelp :: IO ()
cmdSandboxHelp = do
  putStrLn "Help description for sandbox commands"
  putStrLn ""

-- |Simple application module.
cmdSandboxInit :: IO ()
cmdSandboxInit = do
  putStrLn "Initializing a sandbox..."
  putStrLn "Done!"
  putStrLn ""

-- |Application module with complex cmd-line parameters.
cmdInstall :: InstallConfig -> IO ()
cmdInstall config = do
  putStrLn "Starting installation with"
  putStrLn $ "builddir: " ++ builddir config
  putStrLn $ "datadir: " ++ datadir config
  putStrLn $ "docdir: " ++ docdir config
  putStrLn $ "bindir: " ++ bindir config
  putStrLn "Installation done!"
  putStrLn ""
