module Action where

{- import Signal
import DefTime
import Transition
import Output

type Action = {
  signal :: Signal,
  defTime :: DefTime,
  transition :: Transition,
  outputs :: [Output]
  } deriving (Show, Eq, Generic, NFData)

emptyAction :: Action
emptyAction = actionFromGraph $ Constant 0

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

actionToTimes :: Tempo -> UTCTime -> Action -> (UTCTime,UTCTime)
actionToTimes tempo eTime x = (t1,t2)
  where
    t1 = calculateT1 tempo eTime (defTime x)
    t2 = addUTCTime (transitionToXfade tempo $ transition x) t1

actionOutputsAudio :: Action -> Bool
actionOutputsAudio = outputsAudio . outputs

actionOutputsWebGL :: Action -> Bool
actionOutputsWebGL = outputsWebGL . outputs
-}
