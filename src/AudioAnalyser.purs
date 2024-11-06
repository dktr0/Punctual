module AudioAnalyser where

import Prelude (Unit, bind, discard, pure, unit, when, ($), (<>))
import Effect (Effect)
import Effect.Ref (Ref,new,read,write)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj)
import Data.Newtype (unwrap)

import WebAudio

type AudioAnalyser = {
  webAudioContext :: WebAudioContext,
  sourceNode :: Ref WebAudioNode,
  mAnalyserNode :: Ref (Maybe WebAudioNode),
  analyserArray :: AnalyserArray,
  lo :: Ref Number,
  mid :: Ref Number,
  hi :: Ref Number
  }
      
newAudioAnalyser :: WebAudioContext -> WebAudioNode -> Effect AudioAnalyser
newAudioAnalyser webAudioContext n = do
  sourceNode <- new n
  mAnalyserNode <- new Nothing
  analyserArray <- _analyserArray 512
  lo <- new 0.0
  mid <- new 0.0
  hi <- new 0.0
  pure {
    webAudioContext,
    sourceNode,
    mAnalyserNode,
    analyserArray,
    lo,
    mid,
    hi
  }
    
_disactivateAnalysis :: AudioAnalyser -> Effect Unit
_disactivateAnalysis a = do
  mAnalyserNode <- read a.mAnalyserNode
  case mAnalyserNode of
    Nothing -> pure unit  -- analysis is not currently active, so nothing more to do
    Just analyserNode -> do -- disactivate
      sourceNode <- read a.sourceNode
      disconnect sourceNode analyserNode
      write Nothing a.mAnalyserNode
      log "punctual: disactivating an audio analyser..."
      
_activateAnalysis :: AudioAnalyser -> Effect WebAudioNode -- WebAudioNode is current AnalyserNode
_activateAnalysis a = do
  mAnalyserNode <- read a.mAnalyserNode
  case mAnalyserNode of
    Just analyserNode -> pure analyserNode -- analysis is already active, so nothing more to do
    Nothing -> do -- analysis is not active, so need to make new source and analyser nodes
      analyserNode <- _analyserNode a.webAudioContext 1024 0.5
      sourceNode <- read a.sourceNode
      connect sourceNode analyserNode
      write (Just analyserNode) a.mAnalyserNode
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

setSourceNode :: AudioAnalyser -> WebAudioNode -> Effect Unit
setSourceNode a newSourceNode = do
  mAnalyserNode <- read a.mAnalyserNode
  case mAnalyserNode of
    Just analyserNode -> do
      oldSourceNode <- read a.sourceNode
      disconnect oldSourceNode analyserNode
      connect newSourceNode analyserNode
    Nothing -> pure unit
  write newSourceNode a.sourceNode
        
foreign import data AnalyserArray :: Type

foreign import _analyserArray :: Int -> Effect AnalyserArray

foreign import _getByteFrequencyData :: WebAudioNode -> AnalyserArray -> Effect Unit

foreign import _getLo :: AnalyserArray -> Effect Number
foreign import _getMid :: AnalyserArray -> Effect Number
foreign import _getHi:: AnalyserArray -> Effect Number
