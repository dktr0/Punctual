{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Definition where

import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.AudioTime
import Sound.Punctual.DefTime
import Sound.Punctual.Transition
import Sound.Punctual.Graph
import Sound.Punctual.Target

data Definition = Definition deriving (Show,Eq,Generic,NFData)

{-
data Definition = Definition {
  defTime :: DefTime,
  transition :: Transition,
  graph :: Graph,
  targets :: [Target]
  } deriving (Show, Eq)

definitionToTimes :: (AudioTime,Double) -> AudioTime -> Definition -> (AudioTime,AudioTime)
definitionToTimes tempo@(anchor,cps) evalTime x = (t1,t2)
  where
    t1 = calculateT1 tempo evalTime (defTime x)
    t2 = (transitionToXfade cps $ transition x) + t1
-}
