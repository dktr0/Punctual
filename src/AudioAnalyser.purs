module AudioAnalyser where

import Prelude (Unit, bind, discard, pure, unit, when, ($), (<>))
import Effect (Effect)
import Effect.Ref (Ref,new,read,write)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Monoid.Disj (Disj)
import Data.Newtype (unwrap)

import WebAudio

type AudioAnalyser = {
  webAudioContext :: WebAudioContext,
  defaultSource :: Effect WebAudioNode,
  intendedSource :: Ref (Effect WebAudioNode),
  sourceAndAnalyser :: Ref (Maybe (Tuple WebAudioNode WebAudioNode)),
  analyserArray :: AnalyserArray,
  lo :: Ref Number,
  mid :: Ref Number,
  hi :: Ref Number
  }
  
newInputAnalyser :: WebAudioContext -> Effect AudioAnalyser
newInputAnalyser webAudioContext = do
  let x = _defaultAudioInputNode webAudioContext
  _newAnalyser webAudioContext x
  
newOutputAnalyser :: WebAudioContext -> WebAudioNode -> Effect AudioAnalyser
newOutputAnalyser webAudioContext defaultOutput = do
  let x = pure defaultOutput
  _newAnalyser webAudioContext x
  
_newAnalyser :: WebAudioContext -> Effect WebAudioNode -> Effect AudioAnalyser
_newAnalyser webAudioContext defaultSource = do
  intendedSource <- new defaultSource
  analyserArray <- _analyserArray 512
  sourceAndAnalyser <- new Nothing
  lo <- new 0.0
  mid <- new 0.0
  hi <- new 0.0
  pure {
    webAudioContext,
    defaultSource,
    intendedSource,
    sourceAndAnalyser,
    analyserArray,
    lo,
    mid,
    hi
  }
  
setAnalysisSource :: AudioAnalyser -> Maybe (Effect WebAudioNode) -> Effect Unit
setAnalysisSource a mEffectNode = do
  let effectNode = case mEffectNode of
                     Nothing -> a.defaultSource
                     Just x -> x
  write effectNode a.intendedSource
  mSourceAndAnalyser <- read a.sourceAndAnalyser
  case mSourceAndAnalyser of
    Nothing -> pure unit  -- analysis is not currently active, so nothing more to do
    Just (Tuple source analyser) -> do -- analysis currently active, so disconnect and reconnect
      disconnect source analyser
      newSource <- effectNode
      connect newSource analyser
    
_disactivateAnalysis :: AudioAnalyser -> Effect Unit
_disactivateAnalysis a = do
  mSourceAndAnalyser <- read a.sourceAndAnalyser
  case mSourceAndAnalyser of
    Nothing -> pure unit  -- analysis is not currently active, so nothing more to do
    Just (Tuple source analyser)-> do -- disactivate
      disconnect source analyser
      write Nothing a.sourceAndAnalyser
      log "punctual: disactivating an audio analyser..."
      
_activateAnalysis :: AudioAnalyser -> Effect WebAudioNode -- WebAudioNode is current AnalyserNode
_activateAnalysis a = do
  mSourceAndAnalyser <- read a.sourceAndAnalyser
  case mSourceAndAnalyser of
    Just (Tuple _ analyserNode) -> pure analyserNode -- analysis is already active, so nothing more to do
    Nothing -> do -- analysis is not active, so need to make new source and analyser nodes
      intendedSource' <- read a.intendedSource
      sourceNode <- intendedSource'
      analyserNode <- _analyserNode a.webAudioContext 1024 0.5
      connect sourceNode analyserNode
      write (Just $ Tuple sourceNode analyserNode) a.sourceAndAnalyser
      log "punctual: activating an audio analyser..."
      pure analyserNode
      
updateAnalyser :: AudioAnalyser -> forall r. { fft :: Disj Boolean, lo :: Disj Boolean, mid :: Disj Boolean, hi :: Disj Boolean | r } -> Effect Unit
updateAnalyser a needs = do
  case (unwrap $ needs.fft <> needs.lo <> needs.mid <> needs.hi) of
    false -> _disactivateAnalysis a
    true -> do
      resumeWebAudioContext a.webAudioContext
      analyserNode <- _activateAnalysis a
      _getByteFrequencyData analyserNode a.analyserArray
      when (unwrap needs.lo) $ do
        x <- _getLo a.analyserArray
        write x a.lo
      when (unwrap needs.mid) $ do
        x <- _getMid a.analyserArray
        write x a.mid
      when (unwrap needs.hi) $ do
        x <- _getHi a.analyserArray
        write x a.hi
        
foreign import data AnalyserArray :: Type

foreign import _analyserArray :: Int -> Effect AnalyserArray

foreign import _getByteFrequencyData :: WebAudioNode -> AnalyserArray -> Effect Unit

foreign import _getLo :: AnalyserArray -> Effect Number
foreign import _getMid :: AnalyserArray -> Effect Number
foreign import _getHi:: AnalyserArray -> Effect Number
