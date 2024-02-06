module MultiMode where

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data MultiMode = Combinatorial | Pairwise

derive instance Eq MultiMode
derive instance Generic MultiMode _
instance Show MultiMode where
  show = genericShow

