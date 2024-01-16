module Transition where

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tempo (Tempo)
import Data.Time.Duration (Seconds)
import Data.Newtype (wrap)

import Duration

data Transition =
  DefaultCrossFade |
  CrossFade Duration |
  HoldPhase

derive instance Eq Transition
derive instance Generic Transition _
instance Show Transition where
  show = genericShow

-- note: returned value represents half of total xfade duration
transitionToXfade :: Tempo -> Transition -> Seconds
transitionToXfade _ DefaultCrossFade = wrap 0.25
transitionToXfade tempo (CrossFade d) = toSeconds tempo d
transitionToXfade _ HoldPhase = wrap 0.005
