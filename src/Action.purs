module Action where

import Prelude (identity,($),one,zero)
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust)
import Data.Maybe (maybe)
import Data.List (List(..),(:))

import Signal (Signal)
import DefTime (DefTime(..), calculateT1)
import Transition (Transition(..), transitionToXfade)
import Duration (Duration(..))
import Output (Output)

type Action = {
  signal :: Signal,
  defTime :: DefTime,
  transition :: Transition,
  output :: List Output
  }
  
signalToAction :: Signal -> Action
signalToAction x = { signal: x, defTime: Quant one (InSeconds zero), transition: DefaultCrossFade, output: Nil }

setOutput :: Action -> Output -> Action
setOutput x o = x { output = o : x.output }

setCrossFade :: Action -> Number -> Action
setCrossFade x t = x { transition = CrossFade (InSeconds t) }

actionToTimes :: Tempo -> DateTime -> Action -> Tuple DateTime DateTime
actionToTimes tempo eTime x = Tuple t1 t2
  where
    t1 = calculateT1 tempo eTime x.defTime
    t2 = maybe eTime identity $ adjust (transitionToXfade tempo x.transition) t1
