module Duration where

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Rational (Rational)

data Duration = Seconds Rational | Cycles Rational

derive instance Eq Duration 
derive instance Generic Duration _

instance Show Duration where
  show = genericShow

