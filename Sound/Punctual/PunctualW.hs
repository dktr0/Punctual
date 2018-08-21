module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Sound.Punctual.Graph
import Sound.Punctual.Types
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
  putStrLn "updatePunctualW"
  maybe (return ()) W.stopSynthNow $ prevSynthstance s -- placeholder: should be a specifically timed fade
  putStrLn "previous synthstance stopped"
  let newSynth = futureSynth (history s) e -- placeholder: should manage replacement of previous synth
  putStrLn $ show newSynth
  newSynthstance <- W.instantiateSynth newSynth
  putStrLn "new synthstance instantiated"
  newSynthstance' <- W.startSynthNow newSynthstance -- placeholder: time management needed
  putStrLn "new synthstance started - updatePunctualW completed"
  let newHistory = updateHistory (history s) e
  return $ s { history = newHistory, prevSynthstance = Just newSynthstance' }

futureSynth :: History -> Evaluation -> W.Synth ()
futureSynth h (xs,t) = W.buildSynth $ mapM_ (definitionToMusicW . definition) xs

definitionToMusicW :: Definition -> W.SynthBuilder W.Graph
definitionToMusicW (Definition _ _ _ g) = graphToMusicW g >> W.destination

graphToMusicW :: Graph -> W.SynthBuilder W.Graph

graphToMusicW (Constant x) = W.constant x

graphToMusicW Noise = error "graphToMusicW Noise not supported yet"

graphToMusicW Pink = error "graphToMusicW Pink not supported yet"

graphToMusicW (Sine (Constant x)) = W.oscillator W.Sine (W.Hz x)
graphToMusicW (Sine x) = do
  s <- W.oscillator W.Sine (W.Hz 0)
  graphToMusicW x >> W.audioParamSink "frequency" s

graphToMusicW (Tri x) = do
  s <- W.oscillator W.Triangle (W.Hz 0)
  graphToMusicW x >> W.audioParamSink "frequency" s

graphToMusicW (Saw x) = do
  s <- W.oscillator W.Sawtooth (W.Hz 0)
  graphToMusicW x >> W.audioParamSink "frequency" s

graphToMusicW (Square x) = do
  s <- W.oscillator W.Square (W.Hz 0)
  graphToMusicW x >> W.audioParamSink "frequency" s

graphToMusicW (LPF i f q) = do
  graphToMusicW i
  x <- W.biquadFilter (W.LowPass (W.Hz 20000) 1)
  graphToMusicW f >> W.audioParamSink "frequency" x
  graphToMusicW q >> W.audioParamSink "Q" x

graphToMusicW (HPF i f q) = do
  graphToMusicW i
  x <- W.biquadFilter (W.HighPass (W.Hz 20000) 1)
  graphToMusicW f >> W.audioParamSink "frequency" x
  graphToMusicW q >> W.audioParamSink "Q" x

graphToMusicW (ADSR a b c d) = error "graphToMusicW ADSR _ _ _ not yet supported"

graphToMusicW (Mix xs) = error "graphToMusicW Mix _ not yet supported"

graphToMusicW EmptyGraph = W.constant 0

graphToMusicW (FromTarget x) = error "graphToMusicW (FromTarget _) not yet supported"

graphToMusicW (Sum x y) = error "graphToMusicW Sum _ _ not yet supported"

graphToMusicW (Product x y) = error "graphToMusicW Product _ _ not yet supported"
