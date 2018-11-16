module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (forM_)

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
  maybe (return ()) W.stopSynthNow $ prevSynthstance s -- placeholder: should be a specifically timed fade
  let newSynth = futureSynth (history s) e -- placeholder: should manage replacement of previous synth
  putStrLn $ show e
  putStrLn $ show newSynth
  newSynthstance <- W.instantiateSynth newSynth
  newSynthstance' <- W.startSynthNow newSynthstance -- placeholder: time management needed
  let newHistory = updateHistory (history s) e
  return $ s { history = newHistory, prevSynthstance = Just newSynthstance' }

futureSynth :: History -> Evaluation -> W.Synth ()
-- futureSynth _ _ = test6
futureSynth h (xs,t) = W.buildSynth $ mapM_ (definitionToMusicW . definition) xs

definitionToMusicW :: Definition -> W.SynthBuilder W.Graph
definitionToMusicW (Definition _ _ _ g) = graphToMusicW g >> W.destination

graphToMusicW :: Graph -> W.SynthBuilder W.Graph

graphToMusicW (Constant x) = W.constant x

graphToMusicW Noise = W.constant 0 -- placeholder

graphToMusicW Pink = W.constant 0 -- placeholder

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
  x <- W.biquadFilter (W.LowPass (W.Hz 0) 0)
  graphToMusicW f >> W.audioParamSink "frequency" x
  graphToMusicW q >> W.audioParamSink "Q" x

graphToMusicW (HPF i f q) = do
  graphToMusicW i
  x <- W.biquadFilter (W.HighPass (W.Hz 0) 0)
  graphToMusicW f >> W.audioParamSink "frequency" x
  graphToMusicW q >> W.audioParamSink "Q" x

graphToMusicW (Mix []) = graphToMusicW EmptyGraph
graphToMusicW (Mix (x:xs)) = do
  graphToMusicW x
  m <- W.gain (W.Amp 1.0)
  forM_ xs $ \y -> do
    graphToMusicW y
    W.audioReSink m
  return m

graphToMusicW EmptyGraph = W.constant 0

graphToMusicW (FromTarget x) = W.constant 0 -- placeholder

graphToMusicW (Sum x y) = do
  graphToMusicW x
  m <- W.gain (W.Amp 1.0)
  graphToMusicW y
  W.audioReSink m

graphToMusicW (Product x y) = do
  graphToMusicW x
  m <- W.gain (W.Amp 0.0)
  graphToMusicW y
  W.audioParamSink "gain" m
