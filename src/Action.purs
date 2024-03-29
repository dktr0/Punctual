module Action where

import Prelude (identity,($),one,zero,(==),(||))
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust, diff)
import Data.Time.Duration (Seconds)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)

import Signal (Signal)
import DefTime (DefTime(..), calculateT1)
import Transition (Transition(..), transitionToXfade)
import Duration (Duration(..))
import Output (Output(..))

type Action = {
  signal :: Signal,
  defTime :: DefTime,
  transition :: Transition,
  output :: Output
  }
  
signalToAction :: Signal -> Action
signalToAction x = { signal: x, defTime: Quant one (InSeconds zero), transition: DefaultCrossFade, output: Audio }

setOutput :: Action -> Output -> Action
setOutput x o = x { output = o }

setCrossFade :: Action -> Number -> Action
setCrossFade x t = x { transition = CrossFade (InSeconds t) }

actionToTimes :: Tempo -> DateTime -> Action -> Tuple Number Number
actionToTimes tempo eTime x = Tuple t1' t2'
  where
    t1 = calculateT1 tempo eTime x.defTime
    t2 = maybe eTime identity $ adjust (transitionToXfade tempo x.transition) t1
    t1' = unwrap (diff t1 eTime :: Seconds)
    t2' = unwrap (diff t2 eTime :: Seconds)

actionHasVisualOutput :: Action -> Boolean
actionHasVisualOutput a = a.output == Blend || a.output == RGBA || a.output == Add || a.output == Mul || a.output == RGB

