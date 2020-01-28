{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IntMap.Strict as IntMap
import Data.List

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action
import Sound.Punctual.Program
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data PunctualW = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualDestination :: Node,
  punctualChannels :: Int,
  silentSynthLaunched :: Bool,
  prevSynthsNodes :: IntMap (Synth W.AudioContextIO, Node),
  prevProgramW :: Program
  }

emptyPunctualW :: AudioContext -> Node -> Int -> PunctualW
emptyPunctualW ac dest nchnls = PunctualW {
  punctualAudioContext = ac,
  punctualDestination = dest,
  punctualChannels = nchnls,
  silentSynthLaunched = False,
  prevSynthsNodes = empty,
  prevProgramW = emptyProgram
  }

updatePunctualW :: PunctualW -> (AudioTime,Double) -> Program -> W.AudioContextIO PunctualW
updatePunctualW s tempo p = do
  let evalTime' = evalTime p + 0.2
  let xs = IntMap.filter actionOutputsAudio $ actions p
  let dest = punctualDestination s
  mapM_ (deleteSynth evalTime' evalTime' (0.050 + evalTime')) $ difference (prevSynthsNodes s) xs -- delete synths no longer present
  addedSynthsNodes <- mapM (addNewSynth dest tempo evalTime') $ difference xs (prevSynthsNodes s) -- add synths newly present
  let continuingSynthsNodes = intersection (prevSynthsNodes s) xs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth dest tempo evalTime') continuingSynthsNodes xs
  newSynthsNodes <- return $! IntMap.union addedSynthsNodes updatedSynthsNodes
  when (not $ silentSynthLaunched s) $ do
    W.playSynth dest (evalTime p) $ W.constantSource 0 >>= W.audioOut
    return ()
  return $ s {
    prevSynthsNodes = newSynthsNodes,
    prevProgramW = p,
    silentSynthLaunched = True
    }

addNewSynth :: AudioIO m => W.Node -> (AudioTime,Double) -> AudioTime -> Action -> m (Synth m, W.Node)
addNewSynth dest tempo eTime a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  addSynth dest xfadeStart xfadeStart xfadeEnd a

updateSynth :: AudioIO m => W.Node -> (AudioTime,Double) -> AudioTime -> (Synth m, W.Node) -> Action -> m (Synth m, W.Node)
updateSynth dest tempo eTime prevSynthNode a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  deleteSynth eTime xfadeStart xfadeEnd prevSynthNode
  addSynth dest xfadeStart xfadeStart xfadeEnd a

addSynth :: AudioIO m => W.Node -> AudioTime -> AudioTime -> AudioTime -> Action -> m (Synth m, W.Node)
addSynth dest startTime xfadeStart xfadeEnd a = do
  let xfadeStart' = xfadeStart - startTime
  let xfadeEnd' = xfadeEnd - startTime
  (newNodeRef,newSynth) <- W.playSynth dest startTime $ do
    gainNode <- graphToSynthDef' $ graph a
    W.setParam W.Gain 0.0 0.0 gainNode
    W.setParam W.Gain 0.0 xfadeStart' gainNode
    W.linearRampOnParam W.Gain 1.0 xfadeEnd' gainNode
    mapM_ (connectSynthToOutput gainNode) $ outputs a
    return gainNode
  newNode <- W.nodeRefToNode newNodeRef newSynth
  return (newSynth,newNode)

connectSynthToOutput :: AudioIO m => NodeRef -> Output -> SynthDef m ()
connectSynthToOutput nRef (Panned p) = do
  xs <- W.channelSplitter nRef
  y <- W.mix xs
  z <- W.equalPowerPan p y
  W.audioOut z
connectSynthToOutput nRef Splay = do
  xs <- W.channelSplitter nRef
  y <- W.splay 2 xs
  W.audioOut y
connectSynthToOutput _ _ = return ()

deleteSynth :: MonadIO m => AudioTime -> AudioTime -> AudioTime -> (Synth m, W.Node) -> m ()
deleteSynth eTime xfadeStart xfadeEnd (prevSynth,prevGainNode) = do
  W.setValueAtTime prevGainNode W.Gain 1.0 xfadeStart
  W.linearRampToValueAtTime prevGainNode W.Gain 0.0 xfadeEnd
  W.stopSynth xfadeEnd prevSynth
  let microseconds = ceiling $ (xfadeEnd - eTime + 0.3) * 1000000
  --  ^ = kill synth 100ms after fade out
  liftIO $ forkIO $ do
    threadDelay microseconds
    W.disconnectSynth prevSynth
  return ()


graphToSynthDef' :: AudioIO m => Graph -> SynthDef m NodeRef
graphToSynthDef' g = do
  sd <- mapM graphToSynthDef $ expandMultis g
  cm <- W.channelMerger sd
  W.gain 0 cm

graphToSynthDef :: AudioIO m => Graph -> SynthDef m NodeRef

graphToSynthDef (Multi _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Multi)"

graphToSynthDef (Mono _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Mono)"

graphToSynthDef (Constant x) = W.constantSource x

graphToSynthDef (Bipolar x) = graphToSynthDef $ x * 2 - 1
graphToSynthDef (Unipolar x) = graphToSynthDef $ x * 0.5 + 0.5

graphToSynthDef (Sin (MidiCps (Constant x))) = W.oscillator W.Sine $ W.midicps x
graphToSynthDef (Sin (Constant x)) = W.oscillator W.Sine x
graphToSynthDef (Sin x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Tri (MidiCps (Constant x))) = W.oscillator W.Triangle $ W.midicps x
graphToSynthDef (Tri (Constant x)) = W.oscillator W.Triangle x
graphToSynthDef (Tri x) = do
  s <- W.oscillator W.Triangle 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Saw (MidiCps (Constant x))) = W.oscillator W.Sawtooth $ W.midicps x
graphToSynthDef (Saw (Constant x)) = W.oscillator W.Sawtooth x
graphToSynthDef (Saw x) = do
  s <- W.oscillator W.Sawtooth 0
  graphToSynthDef x >>= W.param W.Frequency s
  return s

graphToSynthDef (Sqr (MidiCps (Constant x))) = W.oscillator W.Square $ W.midicps x
graphToSynthDef (Sqr (Constant x)) = W.oscillator W.Square x
graphToSynthDef (Sqr x) = do
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

graphToSynthDef (Sum (Constant x) (Constant y)) = graphToSynthDef (Constant $ x+y)
graphToSynthDef (Sum x y) = W.mixSynthDefs $ fmap graphToSynthDef [x,y]

graphToSynthDef (Product (Constant x) (Constant y)) = graphToSynthDef (Constant $ x*y)
graphToSynthDef (Product x (DbAmp (Constant y))) = graphToSynthDef x >>= W.gain (W.dbamp y)
graphToSynthDef (Product (DbAmp (Constant x)) y) = graphToSynthDef y >>= W.gain (W.dbamp x)
graphToSynthDef (Product x (Constant y)) = graphToSynthDef x >>= W.gain y
graphToSynthDef (Product (Constant x) y) = graphToSynthDef y >>= W.gain x
graphToSynthDef (Product x y) = do
  m <- graphToSynthDef x >>= W.gain 0.0
  graphToSynthDef y >>= W.param W.Gain m
  return m

graphToSynthDef (Max x y) = graphToSynthDef $ (GreaterThanOrEqual x y * x) + (LessThan x y * y)
graphToSynthDef (Min x y) = graphToSynthDef $ (GreaterThanOrEqual x y * y) + (LessThan x y * x)

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

graphToSynthDef (MidiCps (Constant x)) = W.constantSource $ W.midicps x
graphToSynthDef (MidiCps x) = graphToSynthDef x >>= W.midiCpsWorklet

graphToSynthDef (CpsMidi x) = graphToSynthDef x >>= W.cpsMidiWorklet

graphToSynthDef (DbAmp (Constant x)) = W.constantSource $ W.dbamp x
graphToSynthDef (DbAmp x) = graphToSynthDef x >>= W.dbAmpWorklet

graphToSynthDef (AmpDb x) = graphToSynthDef x >>= W.ampDbWorklet

graphToSynthDef (Abs x) = graphToSynthDef x >>= W.absWorklet

graphToSynthDef (Sqrt x) = graphToSynthDef x >>= W.sqrtWorklet

graphToSynthDef (Pow x y) = do
  x' <- graphToSynthDef x
  y' <- graphToSynthDef y
  W.powWorklet x' y'

graphToSynthDef (Floor x) = graphToSynthDef x >>= W.floorWorklet

graphToSynthDef (Fract x) = graphToSynthDef x >>= W.fractWorklet

graphToSynthDef (Clip (Multi [r1,r2]) x) = do -- *** THIS IS PRETTY HACKY
  r1' <- graphToSynthDef r1
  r2' <- graphToSynthDef r2
  x' <- graphToSynthDef x
  W.clipWorklet r1' r2' x'

graphToSynthDef (Between (Multi [r1,r2]) x) = graphToSynthDef g -- ***** THIS IS ALSO PRETTY HACKY
  where g = (GreaterThan r2 r1) * (GreaterThan x r1) * (LessThan x r2) +
            (GreaterThan r1 r2) * (GreaterThan x r2) * (LessThan x r1)

graphToSynthDef (LinLin (Multi [min1,max1]) (Multi [min2,max2]) x) = graphToSynthDef $ min2 + outputRange * proportion -- *** THIS IS ALSO VERY HACKY
  where
    inputRange = max1 - min1
    outputRange = max2 - min2
    proportion = Division (x - min1) inputRange

-- Graph constructors that have no meaning in the audio domain all produce a constant signal of 0
graphToSynthDef _ = W.constantSource 0


-- Multi-channel expansion (and removal of some subgraphs that don't affect audio)

expandMultis :: Graph -> [Graph]
-- multi, mono, constants
expandMultis (Multi []) = []
expandMultis (Multi xs) = fmap graphsToMono $ fmap expandMultis xs
expandMultis (Mono x) = [graphsToMono $ expandMultis x]
expandMultis (Constant x) = [Constant x]
-- unary functions
expandMultis (Bipolar x) = fmap Bipolar $ expandMultis x
expandMultis (Unipolar x) = fmap Unipolar $ expandMultis x
expandMultis (Sin x) = fmap Sin (expandMultis x)
expandMultis (Tri x) = fmap Tri (expandMultis x)
expandMultis (Saw x) = fmap Saw (expandMultis x)
expandMultis (Sqr x) = fmap Sqr (expandMultis x)
expandMultis (MidiCps x) = fmap MidiCps (expandMultis x)
expandMultis (CpsMidi x) = fmap CpsMidi (expandMultis x)
expandMultis (DbAmp x) = fmap DbAmp (expandMultis x)
expandMultis (AmpDb x) = fmap AmpDb (expandMultis x)
expandMultis (Abs x) = fmap Abs (expandMultis x)
expandMultis (Sqrt x) = fmap Sqrt (expandMultis x)
expandMultis (Floor x) = fmap Floor (expandMultis x)
expandMultis (Fract x) = fmap Fract (expandMultis x)
-- binary functions
expandMultis (Product x y) = expandWith' Product x y
expandMultis (Sum x y) = expandWith' Sum x y
expandMultis (Max x y) = expandWith' Max x y
expandMultis (Min x y) = expandWith' Min x y
expandMultis (Division x y) = expandWith' Division x y
expandMultis (GreaterThan x y) = expandWith' GreaterThan x y
expandMultis (GreaterThanOrEqual x y) = expandWith' GreaterThanOrEqual x y
expandMultis (LessThan x y) = expandWith' LessThan x y
expandMultis (LessThanOrEqual x y) = expandWith' LessThanOrEqual x y
expandMultis (Equal x y) = expandWith' Equal x y
expandMultis (NotEqual x y) = expandWith' NotEqual x y
expandMultis (Pow x y) = expandWith' Pow x y
expandMultis (Clip r x) = zipWith Clip r' x' -- *** VERY HACKY
  where
    x' = expandMultis x
    n = length x' * 2
    r' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r
expandMultis (Between r x) = zipWith Between r' x' -- *** VERY HACKY
  where
    x' = expandMultis x
    n = length x' * 2
    r' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r
-- ternary functions
expandMultis (LinLin r1 r2 x) = zipWith3 LinLin r1' r2' x' -- *** VERY HACKY
  where
    x' = expandMultis x
    n = length x' * 2
    r1' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r1
    r2' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r2
expandMultis (LPF i f q) = expandWith3' LPF i f q
expandMultis (HPF i f q) = expandWith3' HPF i f q
expandMultis _ = []

listIntoTuples :: [a] -> [(a,a)]
listIntoTuples (x:y:xs) = (x,y):listIntoTuples xs
listIntoTuples _ = []

graphsToMono :: [Graph] -> Graph
graphsToMono [] = Constant 0
graphsToMono xs = foldl1 Sum xs

expandWith' :: (Graph -> Graph -> Graph) -> Graph -> Graph -> [Graph]
expandWith' f x y = zipWith f x'' y''
  where
    x' = expandMultis x
    y' = expandMultis y
    n = maximum [length x',length y']
    x'' = take n (cycle x')
    y'' = take n (cycle y')

expandWith3' :: (Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> [Graph]
expandWith3' f x y z = zipWith3 f x'' y'' z''
  where
    x' = expandMultis x
    y' = expandMultis y
    z' = expandMultis z
    n = maximum [length x',length y',length z']
    x'' = take n (cycle x')
    y'' = take n (cycle y')
    z'' = take n (cycle z')

expandWith4 :: (Graph -> Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> Graph -> [Graph]
expandWith4 f a b c d = zipWith4 f a'' b'' c'' d''
  where
    a' = expandMultis a
    b' = expandMultis b
    c' = expandMultis c
    d' = expandMultis d
    n = maximum [length a',length b',length c',length d']
    a'' = take n (cycle a')
    b'' = take n (cycle b')
    c'' = take n (cycle c')
    d'' = take n (cycle d')

expandWith5 :: (Graph -> Graph -> Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> Graph -> Graph -> [Graph]
expandWith5 f a b c d e = zipWith5 f a'' b'' c'' d'' e''
  where
    a' = expandMultis a
    b' = expandMultis b
    c' = expandMultis c
    d' = expandMultis d
    e' = expandMultis e
    n = maximum [length a',length b',length c',length d',length e']
    a'' = take n (cycle a')
    b'' = take n (cycle b')
    c'' = take n (cycle c')
    d'' = take n (cycle d')
    e'' = take n (cycle e')
