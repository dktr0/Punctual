module Duration where

import Prelude (class Eq,class Show,($),(/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Rational (Rational,toNumber)
import Data.Newtype (wrap)
import Data.Tempo (Tempo)
import Data.Time.Duration (Seconds)

data Duration = InSeconds Rational | InCycles Rational

derive instance Eq Duration
derive instance Generic Duration _

instance Show Duration where
  show = genericShow

toSeconds :: Tempo -> Duration -> Seconds
toSeconds _ (InSeconds x) = wrap $ toNumber x
toSeconds tempo (InCycles x) = wrap $ toNumber $ x / tempo.freq
