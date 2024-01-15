module DefTime where

import Prelude (class Eq,class Show,($),identity,(/),(*))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Rational (Rational,toNumber)
import Data.Tempo (Tempo,nextBeatExclusive,timeToCount,countToTime)
import Data.DateTime (DateTime, adjust)
import Data.Time.Duration (Seconds)
import Data.Newtype (wrap)
import Data.Maybe (maybe)

import Duration

data DefTime = After Duration | Quant Rational Duration

derive instance Eq DefTime
derive instance Generic DefTime _

instance Show DefTime where
  show = genericShow

calculateT1 :: Tempo -> DateTime -> DefTime -> DateTime
calculateT1 _ evalTime (After (Seconds t)) = maybe evalTime identity $ adjust x evalTime
  where x = wrap (toNumber t) :: Seconds
calculateT1 tempo evalTime (After (Cycles t)) = maybe evalTime identity $ adjust x evalTime
  where x = wrap (toNumber $ t / tempo.freq) :: Seconds
calculateT1 tempo evalTime (Quant n (Seconds t)) = countToTime tempo x
  where x = nextBeatExclusive n (t * tempo.freq) $ timeToCount tempo evalTime
calculateT1 tempo evalTime (Quant n (Cycles t)) = countToTime tempo x
  where x = nextBeatExclusive n t $ timeToCount tempo evalTime
  
-- TODO: consider reworking so that Quant options have to be a certain minimum time in the future

