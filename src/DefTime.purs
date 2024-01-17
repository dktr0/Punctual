module DefTime where

import Prelude (class Eq,class Show,($),identity,(*))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Rational (Rational,toNumber,toRational)
import Data.Tempo (Tempo,nextBeatExclusive,timeToCount,countToTime)
import Data.DateTime (DateTime, adjust)
import Data.Maybe (maybe)
import Data.Int (floor)

import Duration

data DefTime = After Duration | Quant Rational Duration

derive instance Eq DefTime
derive instance Generic DefTime _

instance Show DefTime where
  show = genericShow

calculateT1 :: Tempo -> DateTime -> DefTime -> DateTime
calculateT1 tempo evalTime (After d) = maybe evalTime identity $ adjust x evalTime
  where x = toSeconds tempo d
calculateT1 tempo evalTime (Quant n (InSeconds t)) = countToTime tempo x
  where
    t' = toRational (floor $ t * 1000.0) 1000 -- converting Number to Rational with millisecond precision
    x = nextBeatExclusive n (t' * tempo.freq) $ timeToCount tempo evalTime
calculateT1 tempo evalTime (Quant n (InCycles t)) = countToTime tempo x
  where x = nextBeatExclusive n t $ timeToCount tempo evalTime

-- TODO: consider reworking so that Quant options have to be a certain minimum time in the future
