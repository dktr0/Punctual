{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when,forM)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Split
import Data.Time
import Data.Tempo

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph hiding (when)
import Sound.Punctual.Output
import Sound.Punctual.Action
import Sound.Punctual.Program
import Sound.MusicW (AudioIO,SynthDef,Synth,AudioContext,Node,NodeRef)
import qualified Sound.MusicW as W

data PunctualW = PunctualW {
  punctualAudioContext :: AudioContext,
  punctualIONodes :: (Node,Node),
  punctualChannels :: Int,
  silentSynthLaunched :: Bool,
  prevSynthsNodes :: IntMap (Synth W.AudioContextIO, Node),
  prevProgramW :: Program
  }

emptyPunctualW :: AudioContext -> Node -> Node -> Int -> UTCTime -> PunctualW
emptyPunctualW ac audioIn audioOut nchnls _t0 = PunctualW {
  punctualAudioContext = ac,
  punctualIONodes = (audioIn,audioOut),
  punctualChannels = nchnls,
  silentSynthLaunched = False,
  prevSynthsNodes = empty,
  prevProgramW = emptyProgram _t0
  }


-- probably this belongs in MusicW instead
type TimePair = (UTCTime,AudioTime)

utcAudioTimePair :: W.AudioIO m => m TimePair
utcAudioTimePair = do
  utcNow <- liftIO $ getCurrentTime
  audioNow <- W.audioTime
  return (utcNow,audioNow)

utcToAudioTime :: TimePair -> UTCTime -> AudioTime
utcToAudioTime (utcT,audioT) x = audioT + (realToFrac $ diffUTCTime x utcT)

utcToSafeAudioTime :: TimePair -> UTCTime -> AudioTime
utcToSafeAudioTime (utcT,audioT) x = max (audioT + (realToFrac $ diffUTCTime x utcT)) 0

deletePunctualW :: PunctualW -> W.AudioContextIO ()
deletePunctualW st = do
  timePair@(utcNow,_) <- utcAudioTimePair
  let xfadeStart = addUTCTime 0.050 utcNow
  let xfadeEnd = addUTCTime 0.010 utcNow
  forM (prevSynthsNodes st) $ deleteSynth timePair xfadeStart xfadeEnd
  return ()

updatePunctualW :: PunctualW -> Tempo -> Program -> W.AudioContextIO PunctualW
updatePunctualW s tempo p = do
  timePair <- utcAudioTimePair
  let evalTime' = evalTime p
  let xs = IntMap.filter actionOutputsAudio $ actions p
  let ioNodes = punctualIONodes s
  let aLittleLater = addUTCTime 0.2 evalTime'
  let laterStill = addUTCTime 0.05 aLittleLater
  mapM_ (deleteSynth timePair aLittleLater laterStill) $ difference (prevSynthsNodes s) xs -- delete synths no longer present
  addedSynthsNodes <- mapM (addNewSynth timePair ioNodes tempo evalTime') $ difference xs (prevSynthsNodes s) -- add synths newly present
  let continuingSynthsNodes = intersection (prevSynthsNodes s) xs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth timePair ioNodes tempo evalTime') continuingSynthsNodes xs
  newSynthsNodes <- return $! IntMap.union addedSynthsNodes updatedSynthsNodes
  when (not $ silentSynthLaunched s) $ do
    let (_,dest) = ioNodes
    -- *** could we use a variant of playSynth here that just uses the default destination since it's silent anyway?
    W.playSynth dest (utcToSafeAudioTime timePair evalTime') $ W.constantSource 0 >>= W.audioOut
    return ()
  return $ s {
    prevSynthsNodes = newSynthsNodes,
    prevProgramW = p,
    silentSynthLaunched = True
    }

addNewSynth :: AudioIO m => TimePair -> (W.Node,W.Node) -> Tempo -> UTCTime -> Action -> m (Synth m, W.Node)
addNewSynth timePair ioNodes tempo eTime a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  addSynth timePair ioNodes xfadeStart xfadeStart xfadeEnd a

updateSynth :: AudioIO m => TimePair -> (W.Node,W.Node) -> Tempo -> UTCTime -> (Synth m, W.Node) -> Action -> m (Synth m, W.Node)
updateSynth timePair ioNodes tempo eTime prevSynthNode a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  deleteSynth timePair xfadeStart xfadeEnd prevSynthNode
  addSynth timePair ioNodes xfadeStart xfadeStart xfadeEnd a

addSynth :: AudioIO m => TimePair -> (W.Node,W.Node) -> UTCTime -> UTCTime -> UTCTime -> Action -> m (Synth m, W.Node)
addSynth timePair (audioIn,audioOut) startTime xfadeStart xfadeEnd a = do
  let startTimeUnsafe = utcToAudioTime timePair startTime
  let xfadeStart' = max 0 $ (utcToAudioTime timePair xfadeStart) - startTimeUnsafe
  let xfadeEnd' = max 0.001 $ (utcToAudioTime timePair xfadeEnd) - startTimeUnsafe
  now <- W.audioTime
  let nearFuture = now + 0.005
  let startTimeSafe = max nearFuture startTimeUnsafe
  (newNodeRef,newSynth) <- W.playSynth audioOut startTimeSafe $ do
    gainNode <- graphToSynthDef' audioIn $ graph a
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

deleteSynth :: MonadIO m => TimePair -> UTCTime -> UTCTime -> (Synth m, W.Node) -> m ()
deleteSynth timePair xfadeStart xfadeEnd (prevSynth,prevGainNode) = do
  W.setValueAtTime prevGainNode W.Gain 1.0 $ utcToSafeAudioTime timePair xfadeStart
  let xfadeEnd' = utcToSafeAudioTime timePair xfadeEnd
  W.linearRampToValueAtTime prevGainNode W.Gain 0.0 xfadeEnd'
  W.stopSynth (xfadeEnd' + 0.01) prevSynth
  -- now schedule disconnect for 310 ms after end of fade out, or 500 ms from now, whichever comes later
  let timeOfDisconnect = addUTCTime 0.31 xfadeEnd
  tNow <- liftIO getCurrentTime
  let timeTillDisconnect = max (realToFrac (diffUTCTime timeOfDisconnect tNow) :: Double) 0.5
  let microseconds = ceiling $ timeTillDisconnect * 1000000
  liftIO $ forkIO $ do
    threadDelay microseconds
    W.disconnectSynth prevSynth
  return ()

-- non-recursive: specific optimizations go here, don't use this directly (use optimize instead)
optimize' :: Graph -> Graph
optimize' (Sum (Constant x) (Constant y)) = Constant $ x+y
optimize' (Product (Constant x) (Constant y)) = Constant $ x*y
optimize' (Division _ (Constant 0)) = Constant 0
optimize' (Division (Constant x) (Constant y)) = Constant $ x/y
optimize' (Division x (Constant y)) = Product x (Constant $ 1/y)
optimize' (MidiCps (Constant x)) = Constant $ W.midicps x
optimize' (CpsMidi (Constant x)) = Constant $ W.cpsmidi x
optimize' (DbAmp (Constant x)) = Constant $ W.dbamp x
optimize' (AmpDb (Constant x)) = Constant $ W.ampdb x
optimize' x = x

-- recursive: use this when optimizing, calls optimize' for specific optimizations but recursively walks the tree to make sure the whole tree is optimized
optimize :: Graph -> Graph
optimize (Multi xs) = Multi $ fmap optimize xs
optimize (Mono x) = Mono $ optimize x
optimize (Rep n x) = Rep n $ optimize x
optimize (UnRep n x) = UnRep n $ optimize x
optimize (Bipolar x) = Bipolar $ optimize x
optimize (Unipolar x) = Bipolar $ optimize x
optimize (Sin x) = Sin $ optimize x
optimize (Tri x) = Tri $ optimize x
optimize (Saw x) = Saw $ optimize x
optimize (Sqr x) = Sqr $ optimize x
optimize (MidiCps x) = optimize' $ MidiCps $ optimize x
optimize (CpsMidi x) = optimize' $ CpsMidi $ optimize x
optimize (DbAmp x) = optimize' $ DbAmp $ optimize x
optimize (AmpDb x) = optimize' $ AmpDb $ optimize x
optimize (Abs x) = Abs $ optimize x
optimize (Sqrt x) = Sqrt $ optimize x
optimize (Floor x) = Floor $ optimize x
optimize (Ceil x) = Ceil $ optimize x
optimize (Fract x) = Fract $ optimize x
optimize (Sum x y) = optimize' $ Sum (optimize x) (optimize y)
optimize (Product x y) = optimize' $ Product (optimize x) (optimize y)
optimize (Division x y) = optimize' $ Division (optimize x) (optimize y)
optimize (Max x y) = Max (optimize x) (optimize y)
optimize (Min x y) = Min (optimize x) (optimize y)
optimize (GreaterThan x y) = GreaterThan (optimize x) (optimize y)
optimize (GreaterThanOrEqual x y) = GreaterThanOrEqual (optimize x) (optimize y)
optimize (LessThan x y) = LessThan (optimize x) (optimize y)
optimize (LessThanOrEqual x y) = LessThanOrEqual (optimize x) (optimize y)
optimize (Equal x y) = Equal (optimize x) (optimize y)
optimize (NotEqual x y) = NotEqual (optimize x) (optimize y)
optimize (Gate x y) = Gate (optimize x) (optimize y)
optimize (Pow x y) = Pow (optimize x) (optimize y)
optimize (Clip x y) = Clip (optimize x) (optimize y)
optimize (Between x y) = Between (optimize x) (optimize y)
optimize (Step xs y) = Step xs $ optimize y
optimize (IfThenElse x y z) = IfThenElse (optimize x) (optimize y) (optimize z)
optimize (LinLin x y z) = LinLin (optimize x) (optimize y) (optimize z)
optimize (LPF x y z) = LPF (optimize x) (optimize y) (optimize z)
optimize (HPF x y z) = HPF (optimize x) (optimize y) (optimize z)
optimize (BPF x y z) = BPF (optimize x) (optimize y) (optimize z)
optimize (Delay maxT t i) = Delay maxT (optimize t) (optimize i)
optimize x = x

graphToSynthDef' :: AudioIO m => W.Node -> Graph -> SynthDef m NodeRef
graphToSynthDef' i g = do
  sd <- mapM (graphToSynthDef i . optimize) $ expandMultis g
  case sd of
    [] -> W.constantSource 0 >>= W.gain 0
    _ -> W.channelMerger sd >>= W.gain 0

graphToSynthDef :: AudioIO m => W.Node -> Graph -> SynthDef m NodeRef
graphToSynthDef _ (Multi _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Multi)"
graphToSynthDef _ (Mono _) = error "internal error: graphToSynthDef should only be used post multi-channel expansion (can't handle Mono)"
graphToSynthDef _ (Constant x) = W.constantSource x

graphToSynthDef i (Bipolar x) = graphToSynthDef i $ optimize $ x * 2 - 1
graphToSynthDef i (Unipolar x) = graphToSynthDef i $ optimize $ x * 0.5 + 0.5

graphToSynthDef i AudioIn = W.externalNode i

graphToSynthDef _ (Sin (Constant x)) = W.oscillator W.Sine x
graphToSynthDef i (Sin x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef i x >>= W.param W.Frequency s
  return s

graphToSynthDef _ (Tri (Constant x)) = W.oscillator W.Triangle x
graphToSynthDef i (Tri x) = do
  s <- W.oscillator W.Triangle 0
  graphToSynthDef i x >>= W.param W.Frequency s
  return s

graphToSynthDef _ (Saw (Constant x)) = W.oscillator W.Sawtooth x
graphToSynthDef i (Saw x) = do
  s <- W.oscillator W.Sawtooth 0
  graphToSynthDef i x >>= W.param W.Frequency s
  return s

graphToSynthDef _ (LFTri (Constant x)) = W.oscillator W.Sine (x/2) >>= W.sinToTriWorklet
graphToSynthDef i (LFTri x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef i x >>= W.param W.Frequency s
  W.sinToTriWorklet s

graphToSynthDef _ (LFSaw (Constant x)) = do
  y <- W.oscillator W.Sine (x*0.25)
  z <- W.oscillator W.Sine (x*0.5)
  W.sinToSawWorklet y z
graphToSynthDef i (LFSaw x) = do
  y <- W.oscillator W.Sine 0
  graphToSynthDef i (x*0.25) >>= W.param W.Frequency y
  z <- W.oscillator W.Sine 0
  graphToSynthDef i (x*0.5) >>= W.param W.Frequency z
  W.sinToSawWorklet y z

graphToSynthDef _ (LFSqr (Constant x)) = W.oscillator W.Sine x >>= W.sinToSqrWorklet
graphToSynthDef i (LFSqr x) = do
  s <- W.oscillator W.Sine 0
  graphToSynthDef i x >>= W.param W.Frequency s
  W.sinToSqrWorklet s

graphToSynthDef _ Rnd = W.whiteNoiseWorklet

graphToSynthDef _ (Sqr (Constant x)) = W.oscillator W.Square x
graphToSynthDef i (Sqr x) = do
  s <- W.oscillator W.Square 0
  graphToSynthDef i x >>= W.param W.Frequency s
  return s

graphToSynthDef i (LPF filterIn (Constant f) (Constant q)) = graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass f q)
graphToSynthDef i (LPF filterIn (Constant f) q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (LPF filterIn f (Constant q)) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (LPF filterIn f q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (HPF filterIn (Constant f) (Constant q)) = graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass f q)
graphToSynthDef i (HPF filterIn (Constant f) q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (HPF filterIn f (Constant q)) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (HPF filterIn f q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (BPF filterIn (Constant f) (Constant q)) = graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass f q)
graphToSynthDef i (BPF filterIn (Constant f) q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (BPF filterIn f (Constant q)) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (BPF filterIn f q) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (Sum x y) = W.mixSynthDefs $ fmap (graphToSynthDef i) [x,y]

graphToSynthDef i (Product x (Constant y)) = graphToSynthDef i x >>= W.gain y
graphToSynthDef i (Product (Constant x) y) = graphToSynthDef i y >>= W.gain x
graphToSynthDef i (Product x y) = do
  m <- graphToSynthDef i x >>= W.gain 0.0
  graphToSynthDef i y >>= W.param W.Gain m
  return m

graphToSynthDef i (Max x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.maxWorklet x' y'

graphToSynthDef i (Min x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.minWorklet x' y'

graphToSynthDef i (Division x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.safeDivideWorklet x' y'

graphToSynthDef i (GreaterThan x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.greaterThanWorklet x' y'

graphToSynthDef i (GreaterThanOrEqual x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.greaterThanOrEqualWorklet x' y'

graphToSynthDef i (LessThan x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.lessThanWorklet x' y'

graphToSynthDef i (LessThanOrEqual x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.lessThanOrEqualWorklet x' y'

graphToSynthDef i (Equal x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.equalWorklet x' y'

graphToSynthDef i (NotEqual x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.notEqualWorklet x' y'

graphToSynthDef i (Pow x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.powWorklet x' y'

graphToSynthDef i (Gate x y) = graphToSynthDef i $ optimize $ (LessThan (Abs x) (Abs y)) * y

graphToSynthDef i (Delay maxT (Constant t) (Constant x)) = graphToSynthDef i (Constant x)
graphToSynthDef i (Delay maxT (Constant t) x) = do
  x' <- graphToSynthDef i x
  W.delay maxT x'
graphToSynthDef i (Delay maxT t x) = do
  t' <- graphToSynthDef i t
  x' <- graphToSynthDef i x
  theDelay <- W.delay maxT x'
  W.param W.DelayTime theDelay t' -- *** TODO: this isn't working, delay time isn't changing (it's always maxTime)
  return theDelay

graphToSynthDef i (MidiCps x) = graphToSynthDef i x >>= W.midiCpsWorklet
graphToSynthDef i (CpsMidi x) = graphToSynthDef i x >>= W.cpsMidiWorklet
graphToSynthDef i (DbAmp x) = graphToSynthDef i x >>= W.dbAmpWorklet
graphToSynthDef i (AmpDb x) = graphToSynthDef i x >>= W.ampDbWorklet
graphToSynthDef i (Abs x) = graphToSynthDef i x >>= W.absWorklet
graphToSynthDef i (Sqrt x) = graphToSynthDef i x >>= W.sqrtWorklet
graphToSynthDef i (Floor x) = graphToSynthDef i x >>= W.floorWorklet
graphToSynthDef i (Ceil x) = graphToSynthDef i x >>= W.ceilWorklet
graphToSynthDef i (Fract x) = graphToSynthDef i x >>= W.fractWorklet

graphToSynthDef i (Clip (Multi [r1,r2]) x) = do -- *** THIS IS PRETTY HACKY
  r1' <- graphToSynthDef i r1
  r2' <- graphToSynthDef i r2
  x' <- graphToSynthDef i x
  W.clipWorklet r1' r2' x'

graphToSynthDef i (Between (Multi [r1,r2]) x) = graphToSynthDef i g -- ***** THIS IS ALSO PRETTY HACKY
  where g = (GreaterThan r2 r1) * (GreaterThan x r1) * (LessThan x r2) +
            (GreaterThan r1 r2) * (GreaterThan x r2) * (LessThan x r1)

graphToSynthDef _ (Step [] _) = W.constantSource 0
graphToSynthDef i (Step (x:[]) _) = graphToSynthDef i x
graphToSynthDef i (Step xs (Constant y)) = do
  let y' = max (min y 0.99999999) 0
  let y'' = floor (y' * fromIntegral (length xs))
  graphToSynthDef i (xs!!y'')
graphToSynthDef i (Step xs y) = do
  xs' <- mapM (graphToSynthDef i) xs
  y' <- graphToSynthDef i y
  W.stepWorklet xs' y'

graphToSynthDef i (LinLin (Multi [Constant min1,Constant max1]) (Multi [Constant min2,Constant max2]) x) = graphToSynthDef i $ optimize $ (x - Constant min1) * c + Constant min2
  where c | (max1 - min1) /= 0 = Constant $ (max2 - min2) / (max1 - min1)
          | otherwise = Constant 0
graphToSynthDef i (LinLin (Multi [Constant min1,Constant max1]) (Multi [min2,max2]) x) = graphToSynthDef i $ optimize $ (x - Constant min1) * (max2 - min2) * c + min2
  where c | (max1 - min1) /= 0 = Constant $ 1 / (max1 - min1)
          | otherwise = Constant 0
graphToSynthDef i (LinLin (Multi [min1,max1]) (Multi [min2,max2]) x) = graphToSynthDef i $ optimize $ min2 + outputRange * proportion -- *** THIS IS ALSO VERY HACKY
  where
    inputRange = max1 - min1
    outputRange = max2 - min2
    proportion = Division (x - min1) inputRange

graphToSynthDef i (IfThenElse x y z) = graphToSynthDef i $ ((GreaterThan x 0)*y)+((LessThanOrEqual x 0)*z)

-- Graph constructors that have no meaning in the audio domain all produce a constant signal of 0
graphToSynthDef _ _ = W.constantSource 0


-- Multi-channel expansion (and removal of some subgraphs that don't affect audio)

expandMultis :: Graph -> [Graph]
-- multi, mono, constants
expandMultis (Multi []) = []
expandMultis (Multi xs) = fmap graphsToMono $ fmap expandMultis xs
expandMultis (Mono x) = [graphsToMono $ expandMultis x]
expandMultis (Constant x) = [Constant x]
expandMultis (Rep n x) = concat $ fmap (replicate n) $ expandMultis x
expandMultis (UnRep _ 0) = []
expandMultis (UnRep n x) = fmap ((/ fromIntegral n) . graphsToMono) $ chunksOf n $ expandMultis x
-- unary functions
expandMultis (Bipolar x) = fmap Bipolar $ expandMultis x
expandMultis (Unipolar x) = fmap Unipolar $ expandMultis x
expandMultis Rnd = [Rnd]
expandMultis AudioIn = [AudioIn]
expandMultis (Sin x) = fmap Sin (expandMultis x)
expandMultis (Tri x) = fmap Tri (expandMultis x)
expandMultis (Saw x) = fmap Saw (expandMultis x)
expandMultis (Sqr x) = fmap Sqr (expandMultis x)
expandMultis (LFTri x) = fmap LFTri (expandMultis x)
expandMultis (LFSaw x) = fmap LFSaw (expandMultis x)
expandMultis (LFSqr x) = fmap LFSqr (expandMultis x)
expandMultis (MidiCps x) = fmap MidiCps (expandMultis x)
expandMultis (CpsMidi x) = fmap CpsMidi (expandMultis x)
expandMultis (DbAmp x) = fmap DbAmp (expandMultis x)
expandMultis (AmpDb x) = fmap AmpDb (expandMultis x)
expandMultis (Abs x) = fmap Abs (expandMultis x)
expandMultis (Sqrt x) = fmap Sqrt (expandMultis x)
expandMultis (Floor x) = fmap Floor (expandMultis x)
expandMultis (Ceil x) = fmap Ceil (expandMultis x)
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
expandMultis (Gate x y) = expandWith' Gate x y
expandMultis (Pow x y) = expandWith' Pow x y
expandMultis (Delay maxT t i) = expandWith' (Delay maxT) t i
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
expandMultis (Step xs y) = fmap (Step xs) $ expandMultis y

-- ternary functions
expandMultis (LinLin r1 r2 x) = zipWith3 LinLin r1' r2' x' -- *** VERY HACKY
  where
    x' = expandMultis x
    n = length x' * 2
    r1' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r1
    r2' = fmap (\(a,b) -> Multi [a,b] ) $ listIntoTuples $ take n $ cycle $ expandMultis r2
expandMultis (LPF i f q) = expandWith3' LPF i f q
expandMultis (HPF i f q) = expandWith3' HPF i f q
expandMultis (BPF i f q) = expandWith3' BPF i f q
expandMultis (IfThenElse x y z) = expandWith3' IfThenElse x y z
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
