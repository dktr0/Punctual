{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Transition where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Tempo
import Data.Time

import Sound.Punctual.Duration

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq, Generic, NFData)

-- note: returned value represents half of total xfade duration
transitionToXfade :: Tempo -> Transition -> NominalDiffTime
transitionToXfade _ DefaultCrossFade = 0.25
transitionToXfade _ (CrossFade (Seconds x)) = realToFrac x
transitionToXfade tempo (CrossFade (Cycles x)) = realToFrac (x/freq tempo)
transitionToXfade _ HoldPhase = 0.005
