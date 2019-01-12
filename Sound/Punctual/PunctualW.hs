module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.Time
import Data.Maybe
import Data.Map.Strict

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data Target' = Named String | Anon Int deriving (Show,Eq,Ord)

data PunctualW m = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualDestination :: Node,
  punctualState :: PunctualState,
  prevSynthsNodes :: Map Target' (Synth m,Node)
  }

emptyPunctualW :: AudioIO m => AudioContext -> Node -> UTCTime -> PunctualW m
emptyPunctualW ac dest t = PunctualW {
  punctualAudioContext = ac,
  punctualDestination = dest,
  punctualState = emptyPunctualState t,
  prevSynthsNodes = empty
  }

updatePunctualW :: AudioIO m => PunctualW m -> (Double,Double) -> Evaluation -> m (PunctualW m)
updatePunctualW s tempo e@(xs,t) = do
  let evalTime = W.utcTimeToDouble t + 0.2
  let dest = punctualDestination s
  let exprs = listOfExpressionsToMap xs -- Map Target' Expression
  mapM_ (deleteSynth evalTime evalTime (evalTime+0.050)) $ difference (prevSynthsNodes s) exprs -- delete synths no longer present
  addedSynthsNodes <- mapM (addNewSynth dest tempo evalTime) $ difference exprs (prevSynthsNodes s) -- add synths newly present
  let continuingSynthsNodes = intersection (prevSynthsNodes s) exprs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth dest tempo evalTime) continuingSynthsNodes exprs
  let newSynthsNodes = union addedSynthsNodes updatedSynthsNodes
  let newState = updatePunctualState (punctualState s) e
  return $ s { punctualState = newState, prevSynthsNodes = newSynthsNodes }

addNewSynth :: AudioIO m => W.Node -> (Double,Double) -> Double -> Expression -> m (Synth m, W.Node)
addNewSynth dest tempo evalTime expr = do
  let (xfadeStart,xfadeEnd) = expressionToTimes tempo evalTime expr
  liftIO $ putStrLn $ "addNewSynth evalTime=" ++ show evalTime ++ " xfadeStart=" ++ show xfadeStart ++ " xfadeEnd=" ++ show xfadeEnd
  addSynth dest evalTime xfadeStart xfadeEnd expr

updateSynth :: AudioIO m => W.Node -> (Double,Double) -> Double -> (Synth m, W.Node) -> Expression -> m (Synth m, W.Node)
updateSynth dest tempo evalTime prevSynthNode expr = do
  let (xfadeStart,xfadeEnd) = expressionToTimes tempo evalTime expr
  liftIO $ putStrLn $ "updateSynth evalTime=" ++ show evalTime ++ " xfadeStart=" ++ show xfadeStart ++ " xfadeEnd=" ++ show xfadeEnd
  deleteSynth evalTime xfadeStart xfadeEnd prevSynthNode
  addSynth dest evalTime xfadeStart xfadeEnd expr

addSynth :: AudioIO m => W.Node -> Double -> Double -> Double -> Expression -> m (Synth m, W.Node)
addSynth dest startTime xfadeStart xfadeEnd expr = do
  (newNodeRef,newSynth) <- W.playSynth dest startTime $ do
    gainNode <- expressionToSynthDef expr
    W.setParam W.Gain 0.0 0.0 gainNode
    W.setParam W.Gain 0.0 (xfadeStart - startTime) gainNode
    W.linearRampOnParam W.Gain 1.0 (xfadeEnd - startTime) gainNode
    W.audioOut gainNode
    return gainNode
  newNode <- W.nodeRefToNode newNodeRef newSynth
  return (newSynth,newNode)

deleteSynth :: AudioIO m => Double -> Double -> Double -> (Synth m, W.Node) -> m ()
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

listOfExpressionsToMap :: [Expression] -> Map Target' Expression
listOfExpressionsToMap xs = fromList $ namedExprs ++ anonymousExprs
  where
    namedExprs = fmap (\e -> (Named $ explicitTargetOfDefinition $ definition e,e)) $ Prelude.filter (definitionIsExplicitlyNamed . definition) xs
    anonymousExprs = zipWith (\e n -> (Anon n,e)) (Prelude.filter ((not . definitionIsExplicitlyNamed) . definition) xs) [0..]

expressionToTimes :: (Double,Double) -> Double -> Expression -> (Double,Double)
expressionToTimes tempo evalTime x = definitionToTimes tempo evalTime (definition x)

definitionToTimes :: (Double,Double) -> Double -> Definition -> (Double,Double)
definitionToTimes tempo evalTime x = defTimeAndTransitionToTimes tempo evalTime (defTime x) (transition x)

defTimeAndTransitionToTimes :: (Double,Double) -> Double -> DefTime -> Transition -> (Double,Double)
defTimeAndTransitionToTimes tempo@(_,cps) evalTime dt tr = (t0,t2)
  where
    halfXfade = transitionToXfade cps tr
    t1 = calculateT1 tempo evalTime halfXfade dt
    t0 = t1 - halfXfade
    t2 = t1 + halfXfade

calculateT1 :: (Double,Double) -> Double -> Double -> DefTime -> Double
calculateT1 _ evalTime halfXfade (After (Seconds t)) = evalTime + halfXfade + t
calculateT1 (_,cps) evalTime halfXfade (After (Cycles t)) = evalTime + halfXfade + (t * cps)
calculateT1 (beat0time,cps) evalTime halfXfade (Quant n (Seconds t)) = nextBoundary + t
  where
    minimumT1inQuants = (evalTime + halfXfade - beat0time) * cps / n
    nextBoundary = fromIntegral (floor minimumT1inQuants + 1) * n / cps
calculateT1 (beat0time,cps) evalTime halfXfade (Quant n (Cycles t)) = nextBoundary + (t * cps)
  where
    minimumT1inQuants = (evalTime + halfXfade - beat0time) * cps / n
    nextBoundary = fromIntegral (floor minimumT1inQuants + 1) * n / cps

-- note: returned value represents half of total xfade duration
transitionToXfade :: Double -> Transition -> Double
transitionToXfade _ DefaultCrossFade = 0.25
transitionToXfade _ (CrossFade (Seconds x)) = x / 2
transitionToXfade cps (CrossFade (Cycles x)) = x / cps / 2
transitionToXfade _ HoldPhase = 0.005

-- every expression, when converted to a SynthDef, has a Gain node as its final node,
-- to which a reference will be returned as the wrapped NodeRef supplement. The reference
-- to this final Gain node is used to manage cross-fading between new and old instances
-- of things.

expressionToSynthDef:: AudioIO m => Expression -> SynthDef m NodeRef
expressionToSynthDef (Expression _ NoOutput) = W.constantSource 0 >>= W.gain 0
expressionToSynthDef (Expression d (PannedOutput p)) = definitionToSynthDef d >>= W.equalPowerPan p >>= W.gain 0

definitionToSynthDef:: AudioIO m => Definition -> SynthDef m NodeRef
definitionToSynthDef (Definition _ _ _ g) = graphToSynthDef g

graphToSynthDef :: AudioIO m => Graph -> SynthDef m NodeRef
graphToSynthDef EmptyGraph = W.constantSource 0
graphToSynthDef (Constant x) = W.constantSource x
graphToSynthDef Noise = W.constantSource 0 -- placeholder
graphToSynthDef Pink = W.constantSource 0 -- placeholder
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
