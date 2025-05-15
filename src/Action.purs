module Action where

import Prelude (identity,($),one,zero,(==),(||),(/),(-),(<>),show,(*))
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust, diff)
import Data.DateTime.Instant (unInstant,fromDateTime)
import Data.Time.Duration (Seconds)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)

import Signal (Signal,signalInfo,dimensions,showIndented)
import DefTime (DefTime(..), calculateT1)
import Transition (Transition(..), transitionToXfade)
import Duration (Duration(..))
import Output (Output(..),isVisualOutput,isAudioOutput)

newtype Action = Action {
  signal :: Signal,
  defTime :: DefTime,
  transition :: Transition,
  output :: Output
  }

signal :: Action -> Signal
signal (Action x) = x.signal

defTime :: Action -> DefTime
defTime (Action x) = x.defTime

transition :: Action -> Transition
transition (Action x) = x.transition

output :: Action -> Output
output (Action x) = x.output
  
signalToAction :: Signal -> Action
signalToAction x = Action { signal: x, defTime: Quant one (InSeconds zero), transition: DefaultCrossFade, output: Audio }

setOutput :: Action -> Output -> Action
setOutput (Action x) o = Action $ x { output = o }

setCrossFade :: Action -> Number -> Action
setCrossFade (Action x) t = Action $ x { transition = CrossFade (InSeconds t) }

actionTimesAsDateTime :: Tempo -> DateTime -> Action -> Tuple DateTime DateTime
actionTimesAsDateTime tempo eTime (Action x) = Tuple t1 t2
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
actionHasVisualOutput (Action a) = isVisualOutput a.output

actionHasAudioOutput :: Action -> Boolean
actionHasAudioOutput (Action a) = isAudioOutput a.output

actionHasAudioInput :: Action -> Boolean
actionHasAudioInput (Action a) = unwrap (signalInfo a.signal).ain

showAction :: Action -> String
showAction (Action x) = "  " <> show x.output <> ": " <> showDimensions <> showIndented 4 x.signal
  where
    ds = dimensions x.signal
    showDimensions = show (ds.rows * ds.columns) <> " channels (" <> show ds.columns <> " cols x " <> show ds.rows <> " rows)\n"
