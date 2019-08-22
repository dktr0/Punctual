{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Time
import Data.Maybe
import Data.Map.Strict
import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Graph hiding (difference)
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.PunctualState
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data PunctualW m = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualDestination :: Node,
  punctualState :: PunctualState,
  prevSynthsNodes :: Map Target' (Synth m,Node),
  punctualChannels :: Int
  }

emptyPunctualW :: AudioIO m => AudioContext -> Node -> Int -> AudioTime -> PunctualW m
emptyPunctualW ac dest nchnls t = PunctualW {
  punctualAudioContext = ac,
  punctualDestination = dest,
  punctualState = emptyPunctualState t,
  prevSynthsNodes = empty,
  punctualChannels = nchnls
  }

updatePunctualW :: AudioIO m => PunctualW m -> (AudioTime,Double) -> Evaluation -> m (PunctualW m)
updatePunctualW s tempo e@(xs,t) = do
  let evalTime = t + 0.2
  let dest = punctualDestination s
  let exprs = listOfExpressionsToMap xs -- Map Target' Expression
  mapM_ (deleteSynth evalTime evalTime (0.050 + evalTime)) $ difference (prevSynthsNodes s) exprs -- delete synths no longer present
  addedSynthsNodes <- mapM (addNewSynth dest tempo evalTime) $ difference exprs (prevSynthsNodes s) -- add synths newly present
  let continuingSynthsNodes = intersection (prevSynthsNodes s) exprs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth dest tempo evalTime) continuingSynthsNodes exprs
  let newSynthsNodes = union addedSynthsNodes updatedSynthsNodes
  let newState = updatePunctualState (punctualState s) e
  return $ s { punctualState = newState, prevSynthsNodes = newSynthsNodes }

addNewSynth :: AudioIO m => W.Node -> (AudioTime,Double) -> AudioTime -> Expression -> m (Synth m, W.Node)
addNewSynth dest tempo evalTime expr = do
  let (xfadeStart,xfadeEnd) = expressionToTimes tempo evalTime expr
  addSynth dest xfadeStart xfadeStart xfadeEnd expr

updateSynth :: AudioIO m => W.Node -> (AudioTime,Double) -> AudioTime -> (Synth m, W.Node) -> Expression -> m (Synth m, W.Node)
updateSynth dest tempo evalTime prevSynthNode expr = do
  let (xfadeStart,xfadeEnd) = expressionToTimes tempo evalTime expr
  deleteSynth evalTime xfadeStart xfadeEnd prevSynthNode
  addSynth dest xfadeStart xfadeStart xfadeEnd expr

addSynth :: AudioIO m => W.Node -> AudioTime -> AudioTime -> AudioTime -> Expression -> m (Synth m, W.Node)
addSynth dest startTime xfadeStart xfadeEnd expr = do
  let xfadeStart' = xfadeStart - startTime
  let xfadeEnd' = xfadeEnd - startTime
  (newNodeRef,newSynth) <- W.playSynth dest startTime $ do
    gainNode <- expressionToSynthDef expr
    W.setParam W.Gain 0.0 0.0 gainNode
    W.setParam W.Gain 0.0 xfadeStart' gainNode
    W.linearRampOnParam W.Gain 1.0 xfadeEnd' gainNode
    W.audioOut gainNode
    return gainNode
  newNode <- W.nodeRefToNode newNodeRef newSynth
  return (newSynth,newNode)

deleteSynth :: MonadIO m => AudioTime -> AudioTime -> AudioTime -> (Synth m, W.Node) -> m ()
deleteSynth evalTime xfadeStart xfadeEnd (prevSynth,prevGainNode) = do
  W.setValueAtTime prevGainNode W.Gain 1.0 xfadeStart
  W.linearRampToValueAtTime prevGainNode W.Gain 0.0 xfadeEnd
  W.stopSynth xfadeEnd prevSynth
  let microseconds = ceiling $ (xfadeEnd - evalTime + 0.3) * 1000000
  --  ^ = kill synth 100ms after fade out, assuming evalTime is 200ms in future
  liftIO $ forkIO $ do
    threadDelay microseconds
    W.disconnectSynth prevSynth
  return ()


-- every expression, when converted to a SynthDef, has a Gain node as its final node,
-- to which a reference will be returned as the wrapped NodeRef supplement. The reference
-- to this final Gain node is used to manage cross-fading between new and old instances
-- of things.

expressionToSynthDef:: AudioIO m => Expression -> SynthDef m NodeRef
expressionToSynthDef (Expression d (NamedOutput "splay")) = definitionToSynthDef d >>= W.splay 2 >>= W.gain 0
expressionToSynthDef (Expression d (PannedOutput p)) = definitionToMonoSynthDef d >>= W.equalPowerPan p >>= W.gain 0
expressionToSynthDef _ = W.constantSource 0 >>= W.gain 0

definitionToSynthDef:: AudioIO m => Definition -> SynthDef m [NodeRef]
definitionToSynthDef (Definition _ _ _ g) = mapM graphToSynthDef $ expandMultis g

definitionToMonoSynthDef:: AudioIO m => Definition -> SynthDef m NodeRef
definitionToMonoSynthDef (Definition _ _ _ g) = graphToMonoSynthDef g

graphToMonoSynthDef :: AudioIO m => Graph -> SynthDef m NodeRef
graphToMonoSynthDef = graphToSynthDef . mixGraphs . expandMultis


graphToSynthDef :: AudioIO m => Graph -> SynthDef m NodeRef
graphToSynthDef (Multi _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Multi)"
graphToSynthDef (Mix _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Mix)"

graphToSynthDef EmptyGraph = W.constantSource 0

graphToSynthDef (Constant x) = W.constantSource x

graphToSynthDef Noise = W.constantSource 0 -- placeholder

graphToSynthDef Pink = W.constantSource 0 -- placeholder

graphToSynthDef Fx = W.constantSource 1

graphToSynthDef Fy = W.constantSource 1

graphToSynthDef Px = W.constantSource 0

graphToSynthDef Py = W.constantSource 0

graphToSynthDef (Sine (Constant x)) = W.oscillator W.Sine x
graphToSynthDef (Sine x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Tri (Constant x)) = W.oscillator W.Triangle x
graphToSynthDef (Tri x) = do
  s <- W.oscillator W.Triangle 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Saw (Constant x)) = W.oscillator W.Sawtooth x
graphToSynthDef (Saw x) = do
  s <- W.oscillator W.Sawtooth 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Square (Constant x)) = W.oscillator W.Square x
graphToSynthDef (Square x) = do
  s <- W.oscillator W.Square 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (LPF i (Constant f) (Constant q)) = graphToSynthDef i >>= W.biquadFilter (W.LowPass f q)
graphToSynthDef (LPF i (Constant f) q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.LowPass f 0)
  graphToSynthDef q >>= W.param W.Q x
  return x
graphToSynthDef (LPF i f (Constant q)) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.LowPass 0 q)
  graphToSynthDef f >>= W.param W.Frequency x
  return x
graphToSynthDef (LPF i f q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.LowPass 0 0)
  graphToSynthDef f >>= W.param W.Frequency x
  graphToSynthDef q >>= W.param W.Q x
  return x

graphToSynthDef (HPF i (Constant f) (Constant q)) = graphToSynthDef i >>= W.biquadFilter (W.HighPass f q)
graphToSynthDef (HPF i (Constant f) q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.HighPass f 0)
  graphToSynthDef q >>= W.param W.Q x
  return x
graphToSynthDef (HPF i f (Constant q)) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.HighPass 0 q)
  graphToSynthDef f >>= W.param W.Frequency x
  return x
graphToSynthDef (HPF i f q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.HighPass 0 0)
  graphToSynthDef f >>= W.param W.Frequency x
  graphToSynthDef q >>= W.param W.Q x
  return x

graphToSynthDef (FromTarget x) = W.constantSource 0 -- placeholder

graphToSynthDef (Sum (Constant x) (Constant y)) = graphToSynthDef (Constant $ x+y)
graphToSynthDef (Sum x y) = W.mixSynthDefs $ fmap graphToSynthDef [x,y]

graphToSynthDef (Product (Constant x) (Constant y)) = graphToSynthDef (Constant $ x*y)
graphToSynthDef (Product x (Constant y)) = graphToSynthDef x >>= W.gain y
graphToSynthDef (Product (Constant x) y) = graphToSynthDef y >>= W.gain x
graphToSynthDef (Product x y) = do
  m <- graphToSynthDef x >>= W.gain 0.0
  graphToSynthDef y >>= W.param W.Gain m
  return m

graphToSynthDef (Division x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.safeDivideWorklet x' y'

graphToSynthDef (GreaterThan x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.greaterThanWorklet x' y'

graphToSynthDef (GreaterThanOrEqual x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.greaterThanOrEqualWorklet x' y'

graphToSynthDef (LessThan x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.lessThanWorklet x' y'

graphToSynthDef (LessThanOrEqual x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.lessThanOrEqualWorklet x' y'

graphToSynthDef (Equal x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.equalWorklet x' y'

graphToSynthDef (NotEqual x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.notEqualWorklet x' y'

graphToSynthDef (MidiCps x) = graphToSynthDef x >>= W.midiCpsWorklet

graphToSynthDef (CpsMidi x) = graphToSynthDef x >>= W.cpsMidiWorklet

graphToSynthDef (DbAmp x) = graphToSynthDef x >>= W.dbAmpWorklet

graphToSynthDef (AmpDb x) = graphToSynthDef x >>= W.ampDbWorklet

graphToSynthDef (Abs x) = graphToSynthDef x >>= W.absWorklet

graphToSynthDef (Sqrt x) = graphToSynthDef x >>= W.sqrtWorklet
