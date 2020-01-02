{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Action where

import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.Graph
import Sound.Punctual.DefTime
import Sound.Punctual.Transition
import Sound.Punctual.Output

data Action = Action {
  graph :: Graph,
  defTime :: DefTime,
  transition :: Transition,
  output :: Output
  } deriving (Show, Eq, Generic, NFData)

actionFromGraph :: Graph -> Action
actionFromGraph g = Action {
  graph = g,
  defTime = Quant 1.0 (Seconds 0.0),
  transition = DefaultCrossFade,
  output = NoOutput
  }

(<>) :: Action -> Duration -> Action
a <> d = a { transition = CrossFade d }

(@@) :: Action -> DefTime -> Action
a @@ d = a { defTime = d }

(>>) :: Action -> Output -> Action
a >> o = a { output = o }

actionToTimes :: (AudioTime,Double) -> AudioTime -> Action -> (AudioTime,AudioTime)
actionToTimes tempo evalTime x = (t1,t2)
  where
    t1 = calculateT1 tempo evalTime (defTime x)
    t2 = (transitionToXfade cps $ transition x) + t1
