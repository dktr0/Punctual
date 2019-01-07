module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Maybe

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data PunctualW m = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualDestination :: Node,
  punctualState :: PunctualState,
  prevSynth :: Maybe (Synth m),
  prevNode :: Maybe Node
  }

emptyPunctualW :: AudioIO m => AudioContext -> Node -> UTCTime -> PunctualW m
emptyPunctualW ac dest t = PunctualW {
  punctualAudioContext = ac,
  punctualDestination = dest,
  punctualState = emptyPunctualState t,
  prevSynth = Nothing,
  prevNode = Nothing
  }

updatePunctualW :: AudioIO m => PunctualW m -> Evaluation -> m (PunctualW m)
updatePunctualW s e@(xs,t) = do
  let t' = W.utcTimeToDouble t
  let synthStartTime = t' + 0.1
  when (isJust $ prevNode s) $ do
    W.linearRampToValueAtTime (fromJust $ prevNode s) W.Gain 0.0 (synthStartTime+0.005)
    W.stopSynth (synthStartTime + 0.05) (fromJust $ prevSynth s)
  let newSynthDef = futureSynth (punctualState s) e
  (newNodeRef,newSynth) <- W.playSynth (punctualDestination s) synthStartTime newSynthDef
  newNode <- W.nodeRefToNode newNodeRef newSynth
  let newState = updatePunctualState (punctualState s) e
  t2 <- W.audioUTCTime
  liftIO$ putStrLn $ "t2=" ++ show t2
  return $ s { punctualState = newState, prevSynth = Just newSynth, prevNode = Just newNode }

futureSynth :: AudioIO m => PunctualState -> Evaluation -> SynthDef m NodeRef
futureSynth s (xs,t) = do
  let t' = W.utcTimeToDouble t
  xs <- mapM (expressionToMusicW t') xs -- [SynthDef m NodeRef]
  masterGain <- W.mix xs >>= W.gain 0
  W.audioOut masterGain
  W.setParam W.Gain 0.0 0.000 masterGain
  W.linearRampOnParam W.Gain 1.0 0.005 masterGain -- a 5 msec fade in at synth start time

expressionToMusicW :: AudioIO m => Double -> Expression -> SynthDef m NodeRef
expressionToMusicW t (Expression _ NoOutput) = W.constantSource 0
expressionToMusicW t (Expression d (PannedOutput p)) = definitionToMusicW d >>= W.equalPowerPan p

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
