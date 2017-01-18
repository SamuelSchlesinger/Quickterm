module Main where

import           Control.Applicative
import           Control.Monad

import Data.List (lookup)

import           System.Console.Quickterm


main = qtMain myQtProgram

myQtProgram = program
  [ command "install" $ cmdInstall <$> flags
    [ ("--bindir"  , Just "/default/bindir"  )
    , ("--docdir"  , Just "/default/docdir"  )
    , ("--datadir" , Just "/default/datadir" )
    , ("--builddir", Just "/default/builddir")
    , ("--foo"     , Nothing                 )
    ]
  , section "sandbox"
    [ command_ "init"       cmdSandboxInit
    , command_ "--help"     cmdSandboxHelp -- TODO: should be a built-in command
    , command_ "--snapshot" cmdSandboxSnapshot
    ]
  ]

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
cmdInstall :: [(String,String)] -> IO ()
cmdInstall c = do
  putStrLn "Starting installation with"
  print c
  putStrLn  "Installation done!"
  putStrLn ""
