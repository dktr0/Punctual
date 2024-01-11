module Value where

-- Value represents Signals and functions over Signals (ie. Signal -> Signal, Signal -> Signal -> Signal, etc)
-- The List of Strings are the names of any function arguments, which appear also in the Signal constructor Reference

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List)

import Signal

data Value = Value (List String) Signal

derive instance Eq Value
derive instance Generic Value _

instance Show Value where
  show = genericShow
