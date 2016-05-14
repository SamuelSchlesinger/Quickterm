{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.Console.Quickterm.Description
    ( Description   (..)
    , IsDescription (..)
    ) where

import           System.Console.Quickterm.CanMarshall
import           System.Console.Quickterm.Help        (Help, indent)

-- |A simple description for a section.
data Description = Description
  { -- |The name of a section.
    nameD :: String
  , -- |The description of a section.
    longD :: Help
  }

class IsDescription f where
  toDescription :: f -> Description

instance IsDescription String where
  toDescription s = Description s (indent s)

instance IsDescription Description where
  toDescription = id
