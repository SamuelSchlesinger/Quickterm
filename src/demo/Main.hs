module Main where

import           Control.Applicative
import           Control.Monad
import           System.Environment

import           System.Console.Quickterm
import           System.Console.Quickterm.CanMarshall
import           System.Console.Quickterm.Deserializer
import           System.Console.Quickterm.Help


main = quickterm myQtProgram =<< getArgs

myQtProgram = program
  [ command "install" (cmdInstall <$> installConfig defaultInstallConfig)
  , section "sandbox"
    [ command_ "init" cmdSandboxInit
    , command_ "--help" cmdSandboxHelp
    , command_ "--snapshot" cmdSandboxSnapshot
    ]
  ]

-- |InstallConfig contains all flags for installation command.
data InstallConfig
  = InstallConfig
    { bindir   :: String
    , docdir   :: String
    , datadir  :: String
    , builddir :: String
    } deriving (Show, Eq)

-- |Default values for installation configuration.
defaultInstallConfig :: InstallConfig
defaultInstallConfig = InstallConfig
    { bindir   = "/default/bindir"
    , docdir   = "/default/docdir"
    , datadir  = "/default/datadir"
    , builddir = "/default/builddir"
    }

-- |Defines the parsing process of command line arguments in relation to InstallConfig.
installConfig :: InstallConfig -> Quickterm InstallConfig
installConfig c = pure c
  <|> (flag "--bindir"   >>= \p -> installConfig (c { bindir   = p }))
  <|> (flag "--docdir"   >>= \p -> installConfig (c { docdir   = p }))
  <|> (flag "--datadir"  >>= \p -> installConfig (c { datadir  = p }))
  <|> (flag "--builddir" >>= \p -> installConfig (c { builddir = p }))

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
cmdInstall c = do
  putStrLn "Starting installation with"
  putStrLn $ "builddir: " ++ builddir c
  putStrLn $ "datadir: "  ++ datadir  c
  putStrLn $ "docdir: "   ++ docdir   c
  putStrLn $ "bindir: "   ++ bindir   c
  putStrLn "Installation done!"
  putStrLn ""
