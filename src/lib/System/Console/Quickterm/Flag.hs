{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.Console.Quickterm.Flag
    ( Flag (..)
    , IsFlag (..)
    ) where

import           System.Console.Quickterm.CanMarshall
import           System.Console.Quickterm.Help        (indent)

newtype Flag = Flag
  { getFlag :: String
  } deriving (Eq)

instance CanMarshall Flag where
  defaultM = Flag ""
  helpU _ = indent ""
  deserializer = undefined
  asInput = getFlag

class IsFlag f where
  toFlag :: f -> Flag

instance IsFlag String where
  toFlag = Flag
