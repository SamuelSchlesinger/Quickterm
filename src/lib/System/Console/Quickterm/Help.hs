module System.Console.Quickterm.Help
    ( Help
    , indent
    ) where

import Data.List (intercalate)


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
