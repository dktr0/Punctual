module Action where

import Prelude (identity, map, one, zero, ($), (-), (/), (<>), class Show, show)
import Data.Tuple (Tuple(..))
import Data.Tempo (Tempo)
import Data.DateTime (DateTime, adjust, diff)
import Data.DateTime.Instant (unInstant,fromDateTime)
import Data.Time.Duration (Seconds)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Foldable (foldMap)

import Signal (Signal, SignalInfo)
import Channels (channels)
import Matrix (Matrix(..),rows,columns)
import DefTime (DefTime(..), calculateT1)
import Transition (Transition(..), transitionToXfade)
import Duration (Duration(..))
import Output (Output(..),isVisualOutput,isAudioOutput)

newtype Action = Action {
  matrix :: Matrix Signal,
  defTime :: DefTime,
  transition :: Transition,
  output :: Output
  }

instance Show Action where
  show (Action x) = " " <> show x.output <> " " <> show x.defTime <> " " <> showDimensions -- <> ": " <> show x.matrix
    where
      showDimensions = show (channels x.matrix) <> " channels (" <> show (columns x.matrix) <> " cols x " <> show (rows x.matrix) <> " rows)"

matrix :: Action -> Matrix Signal
matrix (Action x) = x.matrix

defTime :: Action -> DefTime
defTime (Action x) = x.defTime

transition :: Action -> Transition
transition (Action x) = x.transition

output :: Action -> Output
output (Action x) = x.output
  
matrixSignalToAction :: Matrix Signal -> Action
matrixSignalToAction x = Action { matrix: x, defTime: Quant one (InSeconds zero), transition: DefaultCrossFade, output: Audio }

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
-- actionHasAudioInput (Action a) = unwrap (signalInfo a.signal).ain
-- actionHasAudioInput x = any identity $ map (\m -> unwrap $ (matrixSignalInfo m).ain) $ matrix x
actionHasAudioInput x = unwrap (matrixSignalInfo $ matrix x).ain

matrixSignalInfo :: Matrix Signal -> SignalInfo
matrixSignalInfo (Matrix xss) = foldMap (foldMap _.info) xss
