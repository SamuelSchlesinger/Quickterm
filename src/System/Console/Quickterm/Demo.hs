module System.Console.Quickterm.Demo
    (
    ) where

import Control.Applicative
import Control.Monad

import System.Console.Quickterm
import System.Console.Quickterm.Help
import System.Console.Quickterm.CanMarshall
import System.Console.Quickterm.Deserializer


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