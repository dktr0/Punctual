module AudioZone where 

import SharedResources (SharedResources)
import Program (Program)

type AudioZone = {
  sharedResources :: SharedResources,
  program :: Ref Program,
  signals :: Ref (Map Int Signal),
  worklets :: Ref (Map Int AudioWorklet)
  }

newAudioZone :: SharedResources -> Program -> Effect AudioZone
-- launch audio worklets for every signal in the program with an audio output
-- cache the signals for comparison during redefinition

redefineAudioZone :: AudioZone -> Program -> Effect Unit
-- compare signals with audio outputs to cached signals
-- any that are the same, do nothing
-- any that are new, add a new worklet
-- any that are removed, delete the worklet

renderAudioZone :: AudioZone -> Effect Unit
-- possibly nothing to do now, but soon it will be responsible for sync updates

deleteAudioZone :: AudioZone -> Effect Unit
-- delete all of the worklets


-- runWorklet :: WebAudioContext -> WebAudioNode -> String -> Signal -> Number -> Number -> Effect AudioWorklet
-- stopWorklet :: AudioWorklet -> Number -> Number -> Effect Unit

