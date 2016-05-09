{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module System.Console.Quickterm.CanMarshall
    ( CanMarshall (..)
    ) where

import Text.Regex.Base hiding (empty)
import Text.Regex.TDFA hiding (empty)

import System.Console.Quickterm.Help
import System.Console.Quickterm.Deserializer


-- |Handles marshaling from a cmd-line argument to a Haskell data type.
class CanMarshall a where
  -- |A default value for the generic atomic operation 'param'.
  defaultM :: a
  -- |A help description for the generic atomic operation 'param'.
  helpU :: a -> Int -> String
  -- |A deserializer declaration for the generic atomic operation 'param'.
  deserializer :: Deserializer a

instance CanMarshall Int where
  defaultM = 0
  helpU _ = indent "<Integer>"
  deserializer = tryConvert $ \st ->
    if   st =~ "((0|1|2|3|4|5|6|7|8|9)+)"
    then [(read $ st,0)]
    else [(0,length st * 2)]

instance CanMarshall String where
  defaultM = "str"
  helpU _ = indent "<String>"
  deserializer = tryConvert $ \st ->
    if   st =~ "([^-]+)"
    then [(st,0)]
    else [("str",length st * 2)]
