module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Sound.Punctual.Evaluation
import Sound.MusicW
import Sound.MusicW.Synthstance

data PunctualW = PunctualW {
  history :: History,
  prevSynthstance :: Maybe Synthstance
  }

emptyPunctualW :: PunctualW
emptyPunctualW = PunctualW {
  history = emptyHistory,
  prevSynthstance = Nothing
  }

updatePunctualW :: PunctualW -> Evaluation -> IO PunctualW
updatePunctualW s e = return emptyPunctualW -- placeholder
{-
  maybe ... -- fade out previous synthstance if there was one
  let newSynth = -- calculate new synth (which includes fade of previous and future graphs)
  newSynthstance <- runSynth? ...  -- launch the new synth and hold on to the synthstance
  let newHistory = updateHistory (history s) e
  return $ s { history = newHistory, prevSynthStance = Just newSynthstance }

newSynth :: History -> Evaluation -> SynthBuilder ()
newSynth h e = pastSynth h e >> futureSynth h e

pastSynth :: History -> Evaluation -> SynthBuilder ()

futureSynth :: History -> Evaluation -> SynthBuilder ()
-}
