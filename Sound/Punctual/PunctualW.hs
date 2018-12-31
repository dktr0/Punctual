module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (forM_)
import Data.Time.Clock

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import qualified Sound.MusicW as W

data PunctualW = PunctualW {
  punctualAudioContext :: W.AudioContext,
  punctualState :: PunctualState,
  prevSynthstance :: Maybe W.Synthstance
  }

emptyPunctualW :: W.AudioContext -> UTCTime -> PunctualW
emptyPunctualW ac t = PunctualW {
  punctualAudioContext = ac,
  punctualState = emptyPunctualState t,
  prevSynthstance = Nothing
  }

updatePunctualW :: PunctualW -> Evaluation -> IO PunctualW
updatePunctualW s e = do
  let ac = punctualAudioContext s
  maybe (return ()) W.stopSynthNow $ prevSynthstance s -- placeholder: should be a specifically timed fade
  let newSynth = futureSynth (punctualState s) e -- placeholder: should manage replacement of previous synth
  putStrLn $ show e
  putStrLn $ show newSynth
  newSynthstance <- W.instantiateSynth (punctualAudioContext s) newSynth
  newSynthstance' <- W.startSynthNow newSynthstance -- placeholder: time management needed
  let newState = updatePunctualState (punctualState s) e
  return $ s { punctualState = newState, prevSynthstance = Just newSynthstance' }

futureSynth :: PunctualState -> Evaluation -> W.Synth ()
futureSynth s (xs,t) = W.buildSynth $ mapM_ (definitionToMusicW . definition) xs

definitionToMusicW :: Definition -> W.SynthBuilder W.Graph
definitionToMusicW (Definition _ _ _ g) = graphToMusicW g >> W.destination

graphToMusicW :: Graph -> W.SynthBuilder W.Graph

graphToMusicW EmptyGraph = W.constant 0

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
