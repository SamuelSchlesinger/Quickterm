module Main where

import Control.Applicative
import Control.Monad
import System.Environment

import System.Console.Quickterm
import System.Console.Quickterm.Help
import System.Console.Quickterm.CanMarshall
import System.Console.Quickterm.Deserializer


main = getArgs >>= quickterm myQtProgram

myQtProgram = program
  [ section (desc "install")
    [ cmdInstall <$> installConfig defaultInstallConfig -- default values could be loaded from a config file
    ]
  , section (desc "sandbox")
    [ section (desc "init")
      [ pure cmdSandboxInit
      ]
    , const cmdSandboxHelp <$> exact "--help"
    , const cmdSandboxSnapshot <$> exact "--snapshot"
    ]
  ]

qt as = do
  putStrLn $ "=== " ++ show as ++ " ==="
  quickterm myQtProgram as
  putStrLn ""

-- |InstallConfig contains all flags for installation command.
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
installConfig :: Quickterm InstallConfig
installConfig = recursion 1000 installConfig $
  \f -> section (desc "--bindir")
        [ param >>= \p -> f >>= \c -> return $ c { bindir = p } ]
  <|>   section (desc "--datadir")
        [ param >>= \p -> f >>= \c -> return $ c { datadir = p } ]
  <|>   section (desc "--docdir")
        [ param >>= \p -> f >>= \c -> return $ c { docdir = p } ]
  <|>   section (desc "--builddir")
        [ param >>= \p -> f >>= \c -> return $ c { builddir = p } ]
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
