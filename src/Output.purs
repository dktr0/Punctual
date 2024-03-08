module Output where

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Output =
  Audio |
  Blend |
  RGBA |
  Add |
  Mul |
  RGB

derive instance Eq Output
derive instance Generic Output _
instance Show Output where
  show = genericShow

