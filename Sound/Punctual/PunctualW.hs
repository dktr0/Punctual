module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data PunctualW m = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualDestination :: Node,
  punctualState :: PunctualState,
  prevSynth :: Maybe (Synth m)
  }

emptyPunctualW :: AudioIO m => AudioContext -> Node -> UTCTime -> PunctualW m
emptyPunctualW ac dest t = PunctualW {
  punctualAudioContext = ac,
  punctualDestination = dest,
  punctualState = emptyPunctualState t,
  prevSynth = Nothing
  }

updatePunctualW :: AudioIO m => PunctualW m -> Evaluation -> m (PunctualW m)
updatePunctualW s e = do
  let ac = punctualAudioContext s
  maybe (return ()) W.stopSynthNow $ prevSynth s -- placeholder: should be a specifically timed fade
  newSynthDef <- futureSynth (punctualState s) e -- placeholder: should manage replacement of previous synth
  newSynth <- W.playSynth (punctualDestination s) newSynthDef
  W.startSynthNow newSynth -- placeholder: time management needed
  let newState = updatePunctualState (punctualState s) e
  return $ s { punctualState = newState, prevSynth = Just newSynth }

futureSynth :: AudioIO m => PunctualState -> Evaluation -> m (SynthDef m ())
futureSynth s (xs,t) = do
  let x = fmap (definitionToMusicW . definition) xs -- [SynthDef m NodeRef]
  return $ W.mixSynthDefs x >>= W.out -- SynthDef m ()

definitionToMusicW :: AudioIO m => Definition -> SynthDef m NodeRef
definitionToMusicW (Definition _ _ _ g) = graphToMusicW g

graphToMusicW :: AudioIO m => Graph -> SynthDef m NodeRef
graphToMusicW EmptyGraph = W.constantSource 0
graphToMusicW (Constant x) = W.constantSource x
graphToMusicW Noise = W.constantSource 0 -- placeholder
graphToMusicW Pink = W.constantSource 0 -- placeholder
graphToMusicW (Sine x) = do
  s <- W.oscillator W.Sine 0
  graphToMusicW x >>= W.param W.Frequency s
  return s
graphToMusicW (Tri x) = do
  s <- W.oscillator W.Triangle 0
  graphToMusicW x >>= W.param W.Frequency s
  return s
graphToMusicW (Saw x) = do
  s <- W.oscillator W.Sawtooth 0
  graphToMusicW x >>= W.param W.Frequency s
  return s
graphToMusicW (Square x) = do
  s <- W.oscillator W.Square 0
  graphToMusicW x >>= W.param W.Frequency s
  return s
graphToMusicW (LPF i f q) = do
  x <- graphToMusicW i >>= W.biquadFilter (W.LowPass 0 0)
  graphToMusicW f >>= W.param W.Frequency x
  graphToMusicW q >>= W.param W.Q x
  return x
graphToMusicW (HPF i f q) = do
  x <- graphToMusicW i >>= W.biquadFilter (W.HighPass 0 0)
  graphToMusicW f >>= W.param W.Frequency x
  graphToMusicW q >>= W.param W.Q x
  return x
graphToMusicW (FromTarget x) = W.constantSource 0 -- placeholder
graphToMusicW (Sum x y) = W.mixSynthDefs $ fmap graphToMusicW [x,y]
graphToMusicW (Product x y) = do
  m <- graphToMusicW x >>= W.gain 0.0
  graphToMusicW y >>= W.param W.Gain m
  return m
