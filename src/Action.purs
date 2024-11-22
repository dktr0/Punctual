module Action where

import Prelude (identity,($),one,zero,(==),(||),(/),(-))
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust, diff)
import Data.DateTime.Instant (unInstant,fromDateTime)
import Data.Time.Duration (Seconds)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)

import Signal (Signal,signalInfo)
import DefTime (DefTime(..), calculateT1)
import Transition (Transition(..), transitionToXfade)
import Duration (Duration(..))
import Output (Output(..),isVisualOutput,isAudioOutput)

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

actionTimesAsDateTime :: Tempo -> DateTime -> Action -> Tuple DateTime DateTime
actionTimesAsDateTime tempo eTime x = Tuple t1 t2
  where
    t1 = calculateT1 tempo eTime x.defTime
    t2 = maybe eTime identity $ adjust (transitionToXfade tempo x.transition) t1

-- this is used for crossfades on the WebGL side
actionTimesAsSecondsSinceEval :: Tempo -> DateTime -> Action -> Tuple Number Number
actionTimesAsSecondsSinceEval tempo eTime x = Tuple t1' t2'
  where
    Tuple t1 t2 = actionTimesAsDateTime tempo eTime x
    t1' = unwrap (diff t1 eTime :: Seconds)
    t2' = unwrap (diff t2 eTime :: Seconds)

-- this is used for crossfades on the Web Audio side
actionTimesAsAudioTime :: Tempo -> DateTime -> Number -> Action -> Tuple Number Number
actionTimesAsAudioTime tempo eTime clockDiff x = Tuple t1Audio t2Audio
  where
    Tuple t1 t2 = actionTimesAsDateTime tempo eTime x
    t1Posix = (unwrap $ unInstant $ fromDateTime t1) / 1000.0
    t2Posix = (unwrap $ unInstant $ fromDateTime t2) / 1000.0
    t1Audio = t1Posix - clockDiff
    t2Audio = t2Posix - clockDiff
        
actionHasVisualOutput :: Action -> Boolean
actionHasVisualOutput a = isVisualOutput a.output

actionHasAudioOutput :: Action -> Boolean
actionHasAudioOutput a = isAudioOutput a.output

actionHasAudioInput :: Action -> Boolean
actionHasAudioInput a = unwrap (signalInfo a.signal).ain
