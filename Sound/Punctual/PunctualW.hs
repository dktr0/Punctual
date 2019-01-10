module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
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

updatePunctualW :: AudioIO m => PunctualW m -> Evaluation -> m (PunctualW m)
updatePunctualW s e@(xs,t) = do
  let evalTime = W.utcTimeToDouble t + 0.2
  let dest = punctualDestination s
  let exprs = listOfExpressionsToMap xs -- Map Target' Expression
  mapM_ (deleteSynth evalTime (evalTime+0.005)) $ difference (prevSynthsNodes s) exprs -- delete synths no longer present in the expressions
  addedSynthsNodes <- mapM (addSynth dest evalTime) $ difference exprs (prevSynthsNodes s) -- add synths newly added to the expressions
  let continuingSynthsNodes = intersection (prevSynthsNodes s) exprs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth dest evalTime) continuingSynthsNodes exprs
  let newSynthsNodes = union addedSynthsNodes updatedSynthsNodes
  let newState = updatePunctualState (punctualState s) e
  return $ s { punctualState = newState, prevSynthsNodes = newSynthsNodes }

addSynth :: AudioIO m => W.Node -> Double -> Expression -> m (Synth m, W.Node)
addSynth dest evalTime expr = do
  (newNodeRef,newSynth) <- W.playSynth dest evalTime $ do
    gainNode <- expressionToSynthDef expr
    W.setParam W.Gain 1.0 0.0 gainNode
    W.audioOut gainNode
    return gainNode
  newNode <- W.nodeRefToNode newNodeRef newSynth
  return (newSynth,newNode)

updateSynth :: AudioIO m => W.Node -> Double -> (Synth m, W.Node) -> Expression -> m (Synth m, W.Node)
updateSynth dest evalTime prevSynthNode expr = do
  let (xfadeStart,xfadeEnd) = expressionToTimes evalTime expr
  (newNodeRef,newSynth) <- W.playSynth dest xfadeStart $ do
    gainNode <- expressionToSynthDef expr
    W.setParam W.Gain 0.0 0.0 gainNode
    W.linearRampOnParam W.Gain 1.0 (xfadeEnd - xfadeStart) gainNode
    W.audioOut gainNode
    return gainNode
  newGainNode <- W.nodeRefToNode newNodeRef newSynth
  deleteSynth xfadeStart xfadeEnd prevSynthNode
  return (newSynth,newGainNode)

deleteSynth :: AudioIO m => Double -> Double -> (Synth m, W.Node) -> m ()
deleteSynth xfadeStart xfadeEnd (prevSynth,prevGainNode) = do
  W.setValueAtTime prevGainNode W.Gain 1.0 xfadeStart
  W.linearRampToValueAtTime prevGainNode W.Gain 0.0 xfadeEnd
  -- *** TODO: need to schedule a disconnect all and stop!!!
  return ()

listOfExpressionsToMap :: [Expression] -> Map Target' Expression
listOfExpressionsToMap xs = fromList $ namedExprs ++ anonymousExprs
  where
    namedExprs = fmap (\e -> (Named $ explicitTargetOfDefinition $ definition e,e)) $ Prelude.filter (definitionIsExplicitlyNamed . definition) xs
    anonymousExprs = zipWith (\e n -> (Anon n,e)) (Prelude.filter ((not . definitionIsExplicitlyNamed) . definition) xs) [0..]

expressionToTimes :: Double -> Expression -> (Double,Double)
expressionToTimes evalTime x = definitionToTimes evalTime (definition x)

definitionToTimes :: Double -> Definition -> (Double,Double)
definitionToTimes evalTime x = defTimeAndTransitionToTimes evalTime (defTime x) (transition x)

defTimeAndTransitionToTimes :: Double -> DefTime -> Transition -> (Double,Double)
defTimeAndTransitionToTimes evalTime d tr = (t0,t2)
  where
    t1 = concreteDefTime evalTime d
    t0 = t1 - transitionToXfadeDelta tr
    t2 = t1 + transitionToXfadeDelta tr

concreteDefTime :: Double -> DefTime -> Double
concreteDefTime evalTime (After (Seconds t)) = evalTime + t
concreteDefTime evalTime (After (Cycles t)) = evalTime + t -- placeholder: not right...
concreteDefTime evalTime (Quant n (Seconds t)) = evalTime + t -- placeholder: not right...
concreteDefTime evalTime (Quant n (Cycles t)) = evalTime + t -- placeholder: not right...

transitionToXfadeDelta :: Transition -> Double
transitionToXfadeDelta DefaultCrossFade = 0.5
transitionToXfadeDelta (CrossFade (Seconds x)) = x/2
transitionToXfadeDelta (CrossFade (Cycles x)) = x/2 -- placeholder: not right...
transitionToXfadeDelta HoldPhase = 0.5 -- placeholder: actually the phase hold issue doesn't even belong with the xfade issue...

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
graphToSynthDef (Sine x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s
graphToSynthDef (Tri x) = do
  s <- W.oscillator W.Triangle 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s
graphToSynthDef (Saw x) = do
  s <- W.oscillator W.Sawtooth 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s
graphToSynthDef (Square x) = do
  s <- W.oscillator W.Square 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s
graphToSynthDef (LPF i f q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.LowPass 0 0)
  graphToSynthDef f >>= W.param W.Frequency x
  graphToSynthDef q >>= W.param W.Q x
  return x
graphToSynthDef (HPF i f q) = do
  x <- graphToSynthDef i >>= W.biquadFilter (W.HighPass 0 0)
  graphToSynthDef f >>= W.param W.Frequency x
  graphToSynthDef q >>= W.param W.Q x
  return x
graphToSynthDef (FromTarget x) = W.constantSource 0 -- placeholder
graphToSynthDef (Sum x y) = W.mixSynthDefs $ fmap graphToSynthDef [x,y]
graphToSynthDef (Product x y) = do
  m <- graphToSynthDef x >>= W.gain 0.0
  graphToSynthDef y >>= W.param W.Gain m
  return m
