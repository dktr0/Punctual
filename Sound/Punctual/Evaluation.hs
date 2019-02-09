module Sound.Punctual.Evaluation where

import Data.Time

import Sound.Punctual.Types
import Sound.Punctual.Graph
import Sound.Punctual.Phasor

data PunctualState = PunctualState {
  expressions :: [Expression],
  startTime :: UTCTime
  }

emptyPunctualState :: UTCTime -> PunctualState
emptyPunctualState t = PunctualState {
  expressions = [],
  startTime = t
  }

type Evaluation = ([Expression],UTCTime)

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  expressions = fst e,
  startTime = snd e
  }

findGraphsForOutput :: String -> Evaluation -> [Graph]
findGraphsForOutput outputName (xs,t) = fmap (graph . definition) $ filter ((==NamedOutput outputName) . output) xs
