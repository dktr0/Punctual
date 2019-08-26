module Sound.Punctual.PunctualState where

import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Evaluation
import Sound.Punctual.Graph
import Sound.Punctual.Phasor

data ProgramState = ProgramState {
   :: [Expression],
  startTime :: AudioTime
  }

emptyPunctualState :: AudioTime -> PunctualState
emptyPunctualState t = PunctualState {
  expressions = [],
  startTime = t
  }

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  expressions = fst e,
  startTime = snd e
  }
