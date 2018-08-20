module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Sound.Punctual.Evaluation
import qualified Sound.MusicW as W

data PunctualW = PunctualW {
  history :: History,
  prevSynthstance :: Maybe W.Synthstance
  }

emptyPunctualW :: PunctualW
emptyPunctualW = PunctualW {
  history = emptyHistory,
  prevSynthstance = Nothing
  }

updatePunctualW :: PunctualW -> Evaluation -> IO PunctualW
updatePunctualW s e = do
  maybe (return ()) stopSynthNow $ prevSynthstance s -- placeholder: should be a specifically timed fade
  let newSynth = futureSynth (history s) e -- placeholder: should manage replacement of previous synth
  newSynthstance <- instantiateSynth newSynth
  newSynthstance' <- startSynthNow newSynthstance -- placeholder: time management needed
  let newHistory = updateHistory (history s) e
  return $ s { history = newHistory, prevSynthStance = Just newSynthstance' }

futureSynth :: History -> Evaluation -> Synth ()
futureSynth h (xs,t) = W.buildSynth $ mapM_ definitionToMusicW xs

definitionToMusicW :: Definition -> SynthBuilder ()
definitionToMusicW (Definition _ _ _ g) = graphToMusicW g >> W.destination

graphToMusicW :: Graph -> W.SynthBuilder W.Graph

graphToMusicW (Constant x) = W.constant

graphToMusicW Noise = error "graphToMusicW Noise not supported yet"

graphToMusicW Pink = error "graphToMusicW Pink not supported yet"

graphToMusicW (Sine x) = do
  s <- W.oscillator W.Sine (W.Hz 0)
  graphToMusicW x >>= W.audioParamSink "freq" s

graphToMusicW (Tri x) = do
  s <- W.oscillator W.Sine (W.Hz 0)
  graphToMusicW x >>= W.audioParamSink "freq" s

graphToMusicW (Saw x) = do
  s <- W.oscillator W.Sawtooth (W.Hz 0)
  graphToMusicW x >>= W.audioParamSink "freq" s

graphToMusicW (Square x) = do
  s <- W.oscillator W.Square (W.Hz 0)
  graphToMusicW x >>= W.audioParamSink "freq" s

graphToMusicW (LPF i f q) = do
  graphToMusicW i
  x <- W.biquadFilter (W.LowPass (W.Hz 20000) 1)
  graphToMusicW f >>= W.audioParamSink "freq" x
  graphToMusicW q >>= W.audioParamSink "q" x

graphToMusicW (HPF i f q) = do
  graphToMusicW i
  x <- W.biquadFilter (W.HighPass (W.Hz 20000) 1)
  graphToMusicW f >>= W.audioParamSink "freq" x
  graphToMusicW q >>= W.audioParamSink "q" x

graphToMusicW (ADSR a b c d) = error "graphToMusicW ADSR _ _ _ not yet supported"

graphToMusicW (Mix xs) = error "graphToMusicW Mix _ not yet supported"

graphToMusicW EmptyGraph = W.constant 0

graphToMusicW (FromTarget x) = error "graphToMusicW (FromTarget _) not yet supported"

graphToMusicW (Sum x y) = error "graphToMusicW Sum _ _ not yet supported"

graphToMusicW (Product x y) = error "graphToMusicW Product _ _ not yet supported"
