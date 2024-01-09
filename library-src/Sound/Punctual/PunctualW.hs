{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.PunctualW where

-- This module provides an implementation of Punctual using MusicW as an underlying synthesis library

import Control.Monad (when,forM)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IntMap.Strict as IntMap
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

setPunctualWChannels :: Int -> PunctualW -> PunctualW
setPunctualWChannels n x = x { punctualChannels = n }

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
  let (audioIn,audioOut) = punctualIONodes s
  let nchnls = punctualChannels s
  let aLittleLater = addUTCTime 0.2 evalTime'
  let laterStill = addUTCTime 0.05 aLittleLater
  mapM_ (deleteSynth timePair aLittleLater laterStill) $ difference (prevSynthsNodes s) xs -- delete synths no longer present
  addedSynthsNodes <- mapM (addNewSynth timePair audioIn audioOut nchnls tempo evalTime') $ difference xs (prevSynthsNodes s) -- add synths newly present
  let continuingSynthsNodes = intersection (prevSynthsNodes s) xs
  updatedSynthsNodes <- sequence $ intersectionWith (updateSynth timePair audioIn audioOut nchnls tempo evalTime') continuingSynthsNodes xs
  newSynthsNodes <- return $! IntMap.union addedSynthsNodes updatedSynthsNodes
  when (not $ silentSynthLaunched s) $ do
    W.playSynth audioOut (utcToSafeAudioTime timePair evalTime') $ W.constantSource 0 >>= W.audioOut
    return ()
  return $ s {
    prevSynthsNodes = newSynthsNodes,
    prevProgramW = p,
    silentSynthLaunched = True
    }

addNewSynth :: AudioIO m => TimePair -> W.Node -> W.Node -> Int -> Tempo -> UTCTime -> Action -> m (Synth m, W.Node)
addNewSynth timePair audioIn audioOut nchnls tempo eTime a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  addSynth timePair audioIn audioOut nchnls xfadeStart xfadeStart xfadeEnd a

updateSynth :: AudioIO m => TimePair -> W.Node -> W.Node -> Int -> Tempo -> UTCTime -> (Synth m, W.Node) -> Action -> m (Synth m, W.Node)
updateSynth timePair audioIn audioOut nchnls tempo eTime prevSynthNode a = do
  let (xfadeStart,xfadeEnd) = actionToTimes tempo eTime a
  deleteSynth timePair xfadeStart xfadeEnd prevSynthNode
  addSynth timePair audioIn audioOut nchnls xfadeStart xfadeStart xfadeEnd a

addSynth :: AudioIO m => TimePair -> W.Node -> W.Node -> Int -> UTCTime -> UTCTime -> UTCTime -> Action -> m (Synth m, W.Node)
addSynth timePair audioIn audioOut nchnls startTime xfadeStart xfadeEnd a = do
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
    mapM_ (connectSynthToOutput gainNode nchnls) $ outputs a
    return gainNode
  newNode <- W.nodeRefToNode newNodeRef newSynth
  return (newSynth,newNode)

connectSynthToOutput :: AudioIO m => NodeRef -> Int -> Output -> SynthDef m ()
connectSynthToOutput _ 1 _ = error "mono outputs not supported by Punctual web audio"
connectSynthToOutput nRef 2 (Panned p) = do
  xs <- W.channelSplitter nRef
  y <- W.mix xs
  z <- W.equalPowerPan p y
  W.audioOut z
connectSynthToOutput nRef nchnls (Panned p) = do
  xs <- W.channelSplitter nRef
  y <- W.mix xs
  z <- W.circlePan nchnls p y
  W.audioOut z
connectSynthToOutput nRef nchnls Splay = do
  xs <- W.channelSplitter nRef
  y <- W.splay nchnls xs
  W.audioOut y
connectSynthToOutput _ _ _ = return ()

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

-- non-recursive: specific optimizations go here, don't call this directly (use optimize instead)
optimize' :: Graph -> Graph
optimize' (Sum _ (Constant x) (Constant y)) = Constant $ x+y
optimize' (Product _ (Constant x) (Constant y)) = Constant $ x*y
optimize' (Division _ _ (Constant 0)) = Constant 0
optimize' (Division _ (Constant x) (Constant y)) = Constant $ x/y
optimize' (Division mm x (Constant y)) = Product mm x (Constant $ 1/y)
optimize' (MidiCps (Constant x)) = Constant $ W.midicps x
optimize' (CpsMidi (Constant x)) = Constant $ W.cpsmidi x
optimize' (DbAmp (Constant x)) = Constant $ W.dbamp x
optimize' (AmpDb (Constant x)) = Constant $ W.ampdb x
optimize' x = x

-- recursive: call this when optimizing, calls optimize' for specific optimizations but recursively walks the tree to make sure the whole tree is optimized
optimize :: Graph -> Graph
optimize x = x
optimize (Multi xs) = optimize' $ Multi $ fmap optimize xs
optimize (Append xs ys) = optimize' $ Append (optimize xs) (optimize ys)
optimize (Zip xs ys) = optimize' $ Zip (optimize xs) (optimize ys)
optimize (Mono x) = optimize' $ Mono $ optimize x
optimize (Rep n x) = optimize' $ Rep n $ optimize x
optimize (UnRep n x) = optimize' $ UnRep n $ optimize x
optimize (Bipolar x) = optimize' $ Bipolar $ optimize x
optimize (Unipolar x) = optimize' $ Bipolar $ optimize x
-- oscillators
optimize (Osc x) = optimize' $ Osc $ optimize x
optimize (Tri x) = optimize' $ Tri $ optimize x
optimize (Saw x) = optimize' $ Saw $ optimize x
optimize (Sqr x) = optimize' $ Sqr $ optimize x
optimize (LFTri x) = optimize' $ LFTri $ optimize x
optimize (LFSaw x) = optimize' $ LFSaw $ optimize x
optimize (LFSqr x) = optimize' $ LFSqr $ optimize x
-- unary functions from Javascript Math library
optimize (Abs x) = optimize' $ Abs $ optimize x
optimize (Acos x) = optimize' $ Acos $ optimize x
optimize (Acosh x) = optimize' $ Acosh $ optimize x
optimize (Asin x) = optimize' $ Asin $ optimize x
optimize (Asinh x) = optimize' $ Asinh $ optimize x
optimize (Atan x) = optimize' $ Atan $ optimize x
optimize (Atanh x) = optimize' $ Atanh $ optimize x
optimize (Cbrt x) = optimize' $ Cbrt $ optimize x
optimize (Ceil x) = optimize' $ Ceil $ optimize x
optimize (Cos x) = optimize' $ Cos $ optimize x
optimize (Cosh x) = optimize' $ Cosh $ optimize x
optimize (Exp x) = optimize' $ Exp $ optimize x
optimize (Floor x) = optimize' $ Floor $ optimize x
optimize (Log x) = optimize' $ Log $ optimize x
optimize (Log2 x) = optimize' $ Log2 $ optimize x
optimize (Log10 x) = optimize' $ Log10 $ optimize x
optimize (Round x) = optimize' $ Round $ optimize x
optimize (Sign x) = optimize' $ Sign $ optimize x
optimize (Sin x) = optimize' $ Sin $ optimize x
optimize (Sinh x) = optimize' $ Sinh $ optimize x
optimize (Sqrt x) = optimize' $ Sqrt $ optimize x
optimize (Tan x) = optimize' $ Tan $ optimize x
optimize (Tanh x) = optimize' $ Tanh $ optimize x
optimize (Trunc x) = optimize' $ Trunc $ optimize x
-- other unary functions
optimize (MidiCps x) = optimize' $ MidiCps $ optimize x
optimize (CpsMidi x) = optimize' $ CpsMidi $ optimize x
optimize (DbAmp x) = optimize' $ DbAmp $ optimize x
optimize (AmpDb x) = optimize' $ AmpDb $ optimize x
optimize (Fract x) = optimize' $ Fract $ optimize x
-- other functions
optimize (Sum mm x y) = optimize' $ Sum mm (optimize x) (optimize y)
optimize (Product mm x y) = optimize' $ Product mm (optimize x) (optimize y)
optimize (Division mm x y) = optimize' $ Division mm (optimize x) (optimize y)
optimize (Mod mm x y) = optimize' $ Mod mm (optimize x) (optimize y)
optimize (Pow mm x y) = optimize' $ Pow mm (optimize x) (optimize y)
optimize (Equal mm x y) = optimize' $ Equal mm (optimize x) (optimize y)
optimize (NotEqual mm x y) = optimize' $ NotEqual mm (optimize x) (optimize y)
optimize (GreaterThan mm x y) = optimize' $ GreaterThan mm (optimize x) (optimize y)
optimize (GreaterThanOrEqual mm x y) = optimize' $ GreaterThanOrEqual mm (optimize x) (optimize y)
optimize (LessThan mm x y) = optimize' $ LessThan mm (optimize x) (optimize y)
optimize (LessThanOrEqual mm x y) = optimize' $ LessThanOrEqual mm (optimize x) (optimize y)
optimize (Max mm x y) = optimize' $ Max mm (optimize x) (optimize y)
optimize (Min mm x y) = optimize' $ Min mm (optimize x) (optimize y)
optimize (Gate mm x y) = optimize' $ Gate mm (optimize x) (optimize y)
optimize (Clip x y) = optimize' $ Clip (optimize x) (optimize y)
optimize (Between x y) = optimize' $ Between (optimize x) (optimize y)
optimize (Step xs y) = optimize' $ Step xs $ optimize y
optimize (IfThenElse x y z) = optimize' $ IfThenElse (optimize x) (optimize y) (optimize z)
optimize (LinLin x y z) = optimize' $ LinLin (optimize x) (optimize y) (optimize z)
optimize (LPF f q i) = optimize' $ LPF (optimize f) (optimize q) (optimize i)
optimize (HPF f q i) = optimize' $ HPF (optimize f) (optimize q) (optimize i)
optimize (BPF f q i) = optimize' $ BPF (optimize f) (optimize q) (optimize i)
optimize (Delay maxT t i) = optimize' $ Delay maxT (optimize t) (optimize i)
optimize x = optimize' x

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

graphToSynthDef i Pi = graphToSynthDef i 3.1415926535897932384626433832795

graphToSynthDef _ (Osc (Constant x)) = W.oscillator W.Sine x
graphToSynthDef i (Osc x) = do
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

graphToSynthDef i (LPF (Constant f) (Constant q) filterIn) = graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass f q)
graphToSynthDef i (LPF (Constant f) q filterIn)  = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (LPF f (Constant q) filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (LPF f q filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.LowPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (HPF (Constant f) (Constant q) filterIn) = graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass f q)
graphToSynthDef i (HPF (Constant f) q filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (HPF f (Constant q) filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (HPF f q filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.HighPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (BPF (Constant f) (Constant q) filterIn) = graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass f q)
graphToSynthDef i (BPF (Constant f) q filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass f 0)
  graphToSynthDef i q >>= W.param W.Q x
  return x
graphToSynthDef i (BPF f (Constant q) filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass 0 q)
  graphToSynthDef i f >>= W.param W.Frequency x
  return x
graphToSynthDef i (BPF f q filterIn) = do
  x <- graphToSynthDef i filterIn >>= W.biquadFilter (W.BandPass 0 0)
  graphToSynthDef i f >>= W.param W.Frequency x
  graphToSynthDef i q >>= W.param W.Q x
  return x

graphToSynthDef i (Sum _ x y) = W.mixSynthDefs $ fmap (graphToSynthDef i) [x,y]

graphToSynthDef i (Product _ x (Constant y)) = graphToSynthDef i x >>= W.gain y
graphToSynthDef i (Product _ (Constant x) y) = graphToSynthDef i y >>= W.gain x
graphToSynthDef i (Product _ x y) = do
  m <- graphToSynthDef i x >>= W.gain 0.0
  graphToSynthDef i y >>= W.param W.Gain m
  return m

graphToSynthDef i (Max _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.maxWorklet x' y'

graphToSynthDef i (Min _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.minWorklet x' y'

graphToSynthDef i (Division _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.safeDivideWorklet x' y'
  
graphToSynthDef i (Mod _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.modWorklet x' y'
  
graphToSynthDef i (GreaterThan _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.greaterThanWorklet x' y'

graphToSynthDef i (GreaterThanOrEqual _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.greaterThanOrEqualWorklet x' y'

graphToSynthDef i (LessThan _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.lessThanWorklet x' y'

graphToSynthDef i (LessThanOrEqual _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.lessThanOrEqualWorklet x' y'

graphToSynthDef i (Equal _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.equalWorklet x' y'

graphToSynthDef i (NotEqual _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.notEqualWorklet x' y'

graphToSynthDef i (Pow _ x y) = do
  x' <- graphToSynthDef i x
  y' <- graphToSynthDef i y
  W.powWorklet x' y'

graphToSynthDef i (Gate _ x y) = graphToSynthDef i $ optimize $ (GreaterThan PairWise (Abs y) (Abs x)) * y

graphToSynthDef i (Delay maxT (Constant t) x) = do
  x' <- graphToSynthDef i x
  W.delay maxT x' >>= W.setParam W.DelayTime t 0
graphToSynthDef i (Delay maxT t x) = do
  t' <- graphToSynthDef i t
  x' <- graphToSynthDef i x
  theDelay <- W.delay maxT x'
  W.param W.DelayTime theDelay t'
  return theDelay

-- unary functions from JavaScript Math
graphToSynthDef i (Abs x) = graphToSynthDef i x >>= W.absWorklet
graphToSynthDef i (Acos x) = graphToSynthDef i x >>= W.acosWorklet
graphToSynthDef i (Acosh x) = graphToSynthDef i x >>= W.acoshWorklet
graphToSynthDef i (Asin x) = graphToSynthDef i x >>= W.asinWorklet
graphToSynthDef i (Asinh x) = graphToSynthDef i x >>= W.asinhWorklet
graphToSynthDef i (Atan x) = graphToSynthDef i x >>= W.atanWorklet
graphToSynthDef i (Atanh x) = graphToSynthDef i x >>= W.atanhWorklet
graphToSynthDef i (Cbrt x) = graphToSynthDef i x >>= W.cbrtWorklet
graphToSynthDef i (Ceil x) = graphToSynthDef i x >>= W.ceilWorklet
graphToSynthDef i (Cos x) = graphToSynthDef i x >>= W.cosWorklet
graphToSynthDef i (Cosh x) = graphToSynthDef i x >>= W.coshWorklet
graphToSynthDef i (Exp x) = graphToSynthDef i x >>= W.expWorklet
graphToSynthDef i (Floor x) = graphToSynthDef i x >>= W.floorWorklet
graphToSynthDef i (Log x) = graphToSynthDef i x >>= W.logWorklet
graphToSynthDef i (Log2 x) = graphToSynthDef i x >>= W.log2Worklet
graphToSynthDef i (Log10 x) = graphToSynthDef i x >>= W.log10Worklet
graphToSynthDef i (Round x) = graphToSynthDef i x >>= W.roundWorklet
graphToSynthDef i (Sign x) = graphToSynthDef i x >>= W.signWorklet
graphToSynthDef i (Sin x) = graphToSynthDef i x >>= W.sinWorklet
graphToSynthDef i (Sinh x) = graphToSynthDef i x >>= W.sinhWorklet
graphToSynthDef i (Sqrt x) = graphToSynthDef i x >>= W.sqrtWorklet
graphToSynthDef i (Tan x) = graphToSynthDef i x >>= W.tanWorklet
graphToSynthDef i (Tanh x) = graphToSynthDef i x >>= W.tanhWorklet
graphToSynthDef i (Trunc x) = graphToSynthDef i x >>= W.truncWorklet

-- other unary functions
graphToSynthDef i (MidiCps x) = graphToSynthDef i x >>= W.midiCpsWorklet
graphToSynthDef i (CpsMidi x) = graphToSynthDef i x >>= W.cpsMidiWorklet
graphToSynthDef i (DbAmp x) = graphToSynthDef i x >>= W.dbAmpWorklet
graphToSynthDef i (AmpDb x) = graphToSynthDef i x >>= W.ampDbWorklet
graphToSynthDef i (Fract x) = graphToSynthDef i x >>= W.fractWorklet

graphToSynthDef i (Clip (Multi [r1,r2]) x) = do -- *** THIS IS PRETTY HACKY
  r1' <- graphToSynthDef i r1
  r2' <- graphToSynthDef i r2
  x' <- graphToSynthDef i x
  W.clipWorklet r1' r2' x'

graphToSynthDef i (Between (Multi [r1,r2]) x) = graphToSynthDef i g -- ***** THIS IS ALSO PRETTY HACKY
  where g = (GreaterThan Combinatorial r2 r1) * (GreaterThan Combinatorial x r1) * (LessThan Combinatorial x r2) +
            (GreaterThan Combinatorial r1 r2) * (GreaterThan Combinatorial x r2) * (LessThan Combinatorial x r1)

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
    proportion = Division Combinatorial (x - min1) inputRange

graphToSynthDef i (IfThenElse x y z) = graphToSynthDef i $ ((GreaterThan Combinatorial x 0)*y)+((LessThanOrEqual Combinatorial x 0)*z)

-- Graph constructors that have no meaning in the audio domain all produce a constant signal of 0
graphToSynthDef _ _ = W.constantSource 0


-- Multi-channel expansion (and removal of some subgraphs that don't affect audio)

expandMultis :: Graph -> [Graph]
-- multi, mono, constants
expandMultis (Multi []) = []
expandMultis (Multi xs) = concat $ multi $ fmap expandMultis xs
expandMultis (Append xs ys) = expandMultis xs ++ expandMultis ys
expandMultis (Zip xs ys) = concat $ zipWith (\a b -> [a,b]) (expandMultis xs) (expandMultis ys)
expandMultis (Mono x) = [graphsToMono $ expandMultis x]
expandMultis (Constant x) = [Constant x]
expandMultis (Rep n x) = concat $ fmap (replicate n) $ expandMultis x
expandMultis (UnRep _ 0) = []
expandMultis (UnRep n x) = fmap ((/ fromIntegral n) . graphsToMono) $ chunksOf n $ expandMultis x
-- unary functions
expandMultis (Bipolar x) = fmap Bipolar $ expandMultis x
expandMultis (Unipolar x) = fmap Unipolar $ expandMultis x
expandMultis Pi = [Pi]
expandMultis Rnd = [Rnd]
expandMultis AudioIn = [AudioIn]
-- oscillators
expandMultis (Osc x) = fmap Osc (expandMultis x)
expandMultis (Tri x) = fmap Tri (expandMultis x)
expandMultis (Saw x) = fmap Saw (expandMultis x)
expandMultis (Sqr x) = fmap Sqr (expandMultis x)
expandMultis (LFTri x) = fmap LFTri (expandMultis x)
expandMultis (LFSaw x) = fmap LFSaw (expandMultis x)
expandMultis (LFSqr x) = fmap LFSqr (expandMultis x)
-- unary functions from Javascript Math
expandMultis (Abs x) = fmap Abs (expandMultis x)
expandMultis (Acos x) = fmap Acos (expandMultis x)
expandMultis (Acosh x) = fmap Acosh (expandMultis x)
expandMultis (Asin x) = fmap Asin (expandMultis x)
expandMultis (Asinh x) = fmap Asinh (expandMultis x)
expandMultis (Atan x) = fmap Atan (expandMultis x)
expandMultis (Atanh x) = fmap Atanh (expandMultis x)
expandMultis (Cbrt x) = fmap Cbrt (expandMultis x)
expandMultis (Ceil x) = fmap Ceil (expandMultis x)
expandMultis (Cos x) = fmap Cos (expandMultis x)
expandMultis (Cosh x) = fmap Cosh (expandMultis x)
expandMultis (Exp x) = fmap Exp (expandMultis x)
expandMultis (Floor x) = fmap Floor (expandMultis x)
expandMultis (Log x) = fmap Log (expandMultis x)
expandMultis (Log2 x) = fmap Log2 (expandMultis x)
expandMultis (Log10 x) = fmap Log10 (expandMultis x)
expandMultis (Round x) = fmap Round (expandMultis x)
expandMultis (Sign x) = fmap Sign (expandMultis x)
expandMultis (Sin x) = fmap Sin (expandMultis x)
expandMultis (Sinh x) = fmap Sinh (expandMultis x)
expandMultis (Sqrt x) = fmap Sqrt (expandMultis x)
expandMultis (Tan x) = fmap Tan (expandMultis x)
expandMultis (Tanh x) = fmap Tanh (expandMultis x)
expandMultis (Trunc x) = fmap Trunc (expandMultis x)
-- other unary functions
expandMultis (MidiCps x) = fmap MidiCps (expandMultis x)
expandMultis (CpsMidi x) = fmap CpsMidi (expandMultis x)
expandMultis (DbAmp x) = fmap DbAmp (expandMultis x)
expandMultis (AmpDb x) = fmap AmpDb (expandMultis x)
expandMultis (Fract x) = fmap Fract (expandMultis x)
-- binary functions with two multi-modes
expandMultis (Sum mm x y) = expandWith mm (Sum mm) x y
expandMultis (Product mm x y) = expandWith mm (Product mm) x y
expandMultis (Division mm x y) = expandWith mm (Division mm) x y
expandMultis (Mod mm x y) = expandWith mm (Mod mm) x y
expandMultis (Pow mm x y) = expandWith mm (Pow mm) x y
expandMultis (Equal mm x y) = expandWith mm (Equal mm) x y
expandMultis (NotEqual mm x y) = expandWith mm (NotEqual mm) x y
expandMultis (GreaterThan mm x y) = expandWith mm (GreaterThan mm) x y
expandMultis (GreaterThanOrEqual mm x y) = expandWith mm (GreaterThanOrEqual mm) x y
expandMultis (LessThan mm x y) = expandWith mm (LessThan mm) x y
expandMultis (LessThanOrEqual mm x y) = expandWith mm (LessThanOrEqual mm) x y
-- other binary functions (generally with combinatorial semantics)
expandMultis (Max mm x y) = expandWith mm (Max mm) x y
expandMultis (Min mm x y) = expandWith mm (Min mm) x y
expandMultis (Gate mm x y) = expandWith mm (Gate mm) x y
expandMultis (Delay maxT t i) = expandWith Combinatorial (Delay maxT) t i
expandMultis (Clip r x) = [ Clip r'' x' | r'' <- r', x' <- expandMultis x]
  where r' = fmap (\(a,b) -> Multi [a,b]) $ listIntoTuples $ expandMultis r -- *** VERY HACKY
expandMultis (Between r x) = [ Between r'' x' | r'' <- r', x' <- expandMultis x]
  where r' = fmap (\(a,b) -> Multi [a,b]) $ listIntoTuples $ expandMultis r -- *** VERY HACKY
expandMultis (Step xs y) = fmap (Step xs) $ expandMultis y

-- ternary functions
expandMultis (LinLin r1 r2 x) = [ LinLin r1' r2' x' | r1' <- r1s, r2' <- r2s, x' <- expandMultis x]
  where
    r1s = fmap (\(a,b) -> Multi [a,b]) $ listIntoTuples $ expandMultis r1 -- *** VERY HACKY
    r2s = fmap (\(a,b) -> Multi [a,b]) $ listIntoTuples $ expandMultis r2 -- *** VERY HACKY
expandMultis (LPF i f q) = expandWith3 LPF i f q
expandMultis (HPF i f q) = expandWith3 HPF i f q
expandMultis (BPF i f q) = expandWith3 BPF i f q
expandMultis (IfThenElse x y z) = expandWith3 IfThenElse x y z
expandMultis _ = []

listIntoTuples :: [a] -> [(a,a)]
listIntoTuples (x:y:xs) = (x,y):listIntoTuples xs
listIntoTuples (x:[]) = [(x,x)]
listIntoTuples [] = []

graphsToMono :: [Graph] -> Graph
graphsToMono [] = Constant 0
graphsToMono xs = foldl1 (+) xs

expandWith :: MultiMode -> (Graph -> Graph -> Graph) -> Graph -> Graph -> [Graph]
expandWith Combinatorial = expandWithCombinatorial
expandWith PairWise = expandWithPairWise

expandWithPairWise :: (Graph -> Graph -> Graph) -> Graph -> Graph -> [Graph]
expandWithPairWise f x y = zipWith f x'' y''
  where
    x' = expandMultis x
    y' = expandMultis y
    n = maximum [length x',length y']
    x'' = take n (cycle x')
    y'' = take n (cycle y')

expandWithCombinatorial :: (Graph -> Graph -> Graph) -> Graph -> Graph -> [Graph]
expandWithCombinatorial f x y = [ f x' y' | x' <- expandMultis x, y' <- expandMultis y ]

expandWith3 :: (Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> [Graph]
expandWith3 f x y z = [ f x' y' z' | x' <- expandMultis x, y' <- expandMultis y, z' <- expandMultis z ]
