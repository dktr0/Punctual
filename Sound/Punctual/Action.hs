{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Action where

import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph
import Sound.Punctual.DefTime
import Sound.Punctual.Transition
import Sound.Punctual.Duration
import Sound.Punctual.Output

data Action = Action {
  graph :: Graph,
  defTime :: DefTime,
  transition :: Transition,
  outputs :: [Output]
  } deriving (Show, Eq, Generic, NFData)

emptyAction :: Action
emptyAction = actionFromGraph EmptyGraph

actionFromGraph :: Graph -> Action
actionFromGraph g = Action {
  graph = g,
  defTime = Quant 1.0 (Seconds 0.0),
  transition = DefaultCrossFade,
  outputs = []
  }

(<>) :: Action -> Duration -> Action
a <> d = a { transition = CrossFade d }

(@@) :: Action -> DefTime -> Action
a @@ d = a { defTime = d }

(>>) :: Action -> [Output] -> Action
a >> o = a { outputs = o ++ outputs a }

actionToTimes :: (AudioTime,Double) -> AudioTime -> Action -> (AudioTime,AudioTime)
actionToTimes tempo@(tempoT,cps) eTime x = (t1,t2)
  where
    t1 = calculateT1 tempo eTime (defTime x)
    t2 = (transitionToXfade cps $ transition x) + t1

actionOutputsAudio :: Action -> Bool
actionOutputsAudio = outputsAudio . outputs

actionOutputsWebGL :: Action -> Bool
actionOutputsWebGL = outputsWebGL . outputs
