{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.Console.Quickterm.CanMarshall
    ( CanMarshall (..)
    ) where

import           Text.Regex.Base                       hiding (empty)
import           Text.Regex.TDFA                       hiding (empty)

import           System.Console.Quickterm.Deserializer
import           System.Console.Quickterm.Help


-- |Handles marshaling from a cmd-line argument to a Haskell data type.
class CanMarshall a where
  -- |A default value for the generic atomic operation 'param'.
  defaultM     :: a
  -- |A help description for the generic atomic operation 'param'.
  helpU        :: a -> Int -> String
  -- |A deserializer declaration for the generic atomic operation 'param'.
  deserializer :: Deserializer a
  -- |A conversion of a value to the predicted input.
  asInput      :: a -> String

instance CanMarshall Int where
  defaultM = 0
  helpU _ = indent "<Integer>"
  deserializer = tryConvert $ \st ->
    if   st =~ "((0|1|2|3|4|5|6|7|8|9)+)"
    then [(read st,0)]
    else [(0,length st * 2)]
  asInput = show

instance CanMarshall String where
  defaultM = "<value>"
  helpU _ = indent "<String>"
  deserializer = tryConvert $ \st ->
    if   st =~ "([^-]+)"
    then [(st,0)]
    else [("str",length st * 2)]
  asInput = id
