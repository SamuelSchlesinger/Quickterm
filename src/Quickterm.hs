{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

tryConvert :: (String -> [(a,Int)]) -> Deserializer a
tryConvert f = Deserializer $ \st p -> (\(a,i) -> (a,[],p+i)) <$> f st


data Description = Description
  { nameD :: String
  , longD :: Int -> String
  }

class CanMarshal a where
  defaultM :: a
  helpU :: a -> Int -> String
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

bar :: String -> IO ()
bar m = print $ deserialize deserializer m 0 >>= \(a, i, _) -> return (a::String,i)

param :: (Show a, CanMarshal a) => Quickterm a
param = Quickterm $ \i h pi as -> case as of
  []      -> [(defaultM,i+10,h,pi,[])]
  (a:as') -> deserialize deserializer a 0 >>= \(a, _, i') -> return (a, i + i', h, show a:pi, as')

exact :: String -> Quickterm String
exact s = mfilter (==s) param

desc :: String -> Description
desc n = Description n (const "")

section :: Description -> [Quickterm a] -> Quickterm a
section (Description n h) qs = Quickterm $ \i h pi as -> case as of
  []      -> empty
  (a:as') -> qs >>= \m -> runQuickterm m (i + levenshteinDistance defaultEditCosts n a) h (n:pi) as'

program :: [Quickterm a] -> Quickterm a
program qs = Quickterm $ \i h pi as -> qs >>= \m -> runQuickterm m i h pi as

results qt as = quickterm qt as >>= \(a,i,_,pi,_) -> return (a,i,reverse pi)

quickterm :: Quickterm a -> [String] -> a
quickterm qt as = f . filter (\(_, i, _, _, rs) -> i == 0 && rs == []) $ ts
  where
    f rs = case rs of
      []                 -> error . show $ (\(_,_,_,_,rs) -> rs) <$> ts
      (r@(a,_,_,_,_):[]) -> a
      (  (_,_,_,_,_):_ ) -> error "TODO: ambiguous call"
    ts = runQuickterm qt 0 (const "foo") [] as

data InstallConfig
  = InstallConfig
    { bindir :: String
    , docdir :: String
    , datadir :: String
    , builddir :: String
    } deriving (Show, Eq)

defaultInstallConfig :: InstallConfig
defaultInstallConfig = InstallConfig
    { bindir = "/default/bindir"
    , docdir = "/default/docdir"
    , datadir = "/default/datadir"
    , builddir = "/default/builddir"
    }

installConfig :: InstallConfig -> Quickterm InstallConfig
installConfig config =   pure config
                     <|> (exact "--bindir" >> param >>= \p -> installConfig (config { bindir = p }))
                     <|> (exact "--docdir" >> param >>= \p -> installConfig (config { docdir = p }))
                     <|> (exact "--datadir" >> param >>= \p -> installConfig (config { datadir = p }))
                     <|> (exact "--builddir" >> param >>= \p -> installConfig (config { builddir = p }))

foo = program
  [ section (desc "install")
    [ cmdInstall <$> param <*> param
    , cmdInstall <$> param <*> param
    , const cmdInstallBindir <$> exact "--bindir" <*> param <*> param <*> param
    ]
  , section (desc "sandbox")
    [ section (desc "init")
      [ pure cmdSandboxInit
      ]
    , (const cmdSandboxHelp) <$> exact "--help"
    , (const cmdSandboxSnapshot) <$> exact "--snapshot"
    ]
  ]

cmdInstallBindir :: String -> String -> String -> IO ()
cmdInstallBindir bindir app v = do
  putStrLn $ "installing " ++ app
  putStrLn $ "with version " ++ v
  putStrLn $ "with bindir " ++ bindir
  putStrLn "done"
  putStrLn ""

cmdSandboxSnapshot :: IO ()
cmdSandboxSnapshot = do
  putStrLn "Creating a snapshot..."
  putStrLn "Done!"
  putStrLn ""

cmdSandboxHelp :: IO ()
cmdSandboxHelp = do
  putStrLn "Help description for sandbox commands"
  putStrLn ""

cmdSandboxInit :: IO ()
cmdSandboxInit = do
  putStrLn "Initializing a sandbox..."
  putStrLn "Done!"
  putStrLn ""

cmdInstall :: String -> String -> IO ()
cmdInstall = cmdInstallBindir "/default/bindir"


--program :: String -> [(Predicate,Quickterm)] -> Quickterm
--program d ds = Choice ds (indent d)

--section :: String -> [(Predicate,Quickterm)] -> (Predicate,Quickterm)
--section n ds = (exact n, Choice ds (indent $ "== " ++ n ++ " =="))

--command :: String -> String -> TermAction -> (Predicate,Quickterm)
--command n d t = (exact n, Action t (indent $ n ++ "\n-- " ++ d))

--example :: Quickterm
--example = program "Description of your application"
--  [ command "install" "installs a package" . const $ putStrLn "installation process"
--  , section "sandbox"
--    [ command "init" "initialize a sandbox" . const $ putStrLn "initializing a sandbox"
--    , command "clear" "clear the current sandbox" . const $ putStrLn "clearing the current sandbox"
--    ]
--  ]
