module Action where

import Prelude (identity,($))
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust)
import Data.Maybe (maybe)

import Signal (Signal)
import DefTime (DefTime, calculateT1)
import Transition (Transition, transitionToXfade)
import Output (Output)

type Action = {
  signal :: Signal,
  defTime :: DefTime,
  transition :: Transition,
  output :: Output
  }

actionToTimes :: Tempo -> DateTime -> Action -> Tuple DateTime DateTime
actionToTimes tempo eTime x = Tuple t1 t2
  where
    t1 = calculateT1 tempo eTime x.defTime
    t2 = maybe eTime identity $ adjust (transitionToXfade tempo x.transition) t1
