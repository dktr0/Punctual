module AudioAnalyser where

import Prelude (bind,discard,pure,Unit,unit,(>>=),($),when,(||))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo, newTempo)
import Data.Map (Map, empty, lookup, insert)
import Data.Tuple (Tuple(..))

type AudioAnalyser = {
  webAudioContext :: WebAudioContext,
  defaultSource :: Effect WebAudioNode,
  intendedSource :: Ref (Effect WebAudioNode),
  analyserArray :: AnalyserArray,
  sourceAndAnalyser :: Ref (Maybe (Tuple WebAudioNode WebAudioNode)),
  lo :: Ref Number,
  mid :: Ref Number,
  hi :: Ref Number
  }
  
newAnalyser :: WebAudioContext -> Effect WebAudioNode -> Effect AudioAnalyser
newAnalyser webAudioContext defaultSource = do
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
    analyserArray,
    sourceAndAnalyser,
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
      _disconnect source analyser
      newSource <- effectNode
      _connect newSource analyser
    
setAnalysisActive :: AudioAnalyser -> Boolean -> Effect Unit
setAnalysisActive a false = do
  mSourceAndAnalyser <- read a.sourceAndAnalyser
  case mSourceAndAnalyser of
    Nothing -> pure unit  -- analysis is not currently active, so nothing more to do
    Just (Tuple source analyser)-> do -- disactivate
      _disconnect source analyser
      write Nothing a.sourceAndAnalyser
setAnalysisActive a true = do
  mSourceAndAnalyser <- read a.sourceAndAnalyser
  case mSourceAndAnalyser of
    Just _ -> pure unit -- analysis is already active, so nothing more to do
    Nothing -> do -- analysis is not active, so need to make new source and analyser nodes
      intendedSource' <- read a.intendedSource
      sourceNode <- intendedSource'
      analyserNode <- _analyserNode a.webAudioContext 1024 0.5
      _connect sourceNode analyserNode
      write (Just $ Tuple sourceNode analyserNode) a.sourceAndAnalyser
         
      
    {-
updateAudioAnalysis :: SharedResources -> forall r. { ifft :: Boolean, ilo :: Boolean, imid :: Boolean, ihi :: Boolean, fft :: Boolean, lo :: Boolean, mid :: Boolean, hi :: Boolean | r } -> Effect Unit
updateAudioAnalysis sr needs = do
  case (needs.ifft || needs.ilo || needs.imid || needs.ihi) of
    false -> do
      mConnected <- read sr.audioAnalysisNodeInputConnected
      case mConnected of
        Nothing -> pure unit
        Just n -> do
          _disconnectNodes n sr.audioInputAnalyser
          write Nothing sr.audioAnalysisNodeInputConnected    
    true -> do
      mAudioAnalysisNodeInput <- read sr.audioAnalysisNodeInput
      case mAudioAnalysisNodeInput of
        Nothing -> ...connect to microphone...
        Just audioAnalysisNodeInput -> _connectNodes audioAnalysisNodeInput sr.audioInputAnalyser
      _getByteFrequencyData sr.audioInputAnalyser sr.audioInputAnalysisArray    
  case (needs.fft || needs.lo || needs.mid || needs.hi) of
    false -> do
      mConnected <- read sr.audioAnalysisNodeOutputConnected
      case mConnected of
        Nothing -> pure unit
        Just n -> do
          _disconnectNodes n sr.audioOutputAnalyser
          write Nothing sr.audioAnalysisNodeOutputConnected    
    true -> do
      mAudioAnalysisNodeOutput <- read sr.audioAnalysisNodeOutput
      case mAudioAnalysisNodeOutput of
        Nothing -> ...connect to ?
        Just audioAnalysisNodeOutput -> _connectNodes audioAnalysisNodeInput sr.audioInputAnalyser
     _getByteFrequencyData sr.audioOutputAnalyser sr.audioOutputAnalysisArray
     
  when needs.ilo $ do
    x <- _getLo sr.audioInputAnalysisArray
    write x sr.ilo 
  when needs.imid $ do
    x <- _getMid sr.audioInputAnalysisArray
    write x sr.imid
  when needs.ihi $ do
    x <- _getHi sr.audioInputAnalysisArray
    write x sr.ihi
  when needs.lo $ do
    x <- _getLo sr.audioOutputAnalysisArray
    write x sr.lo 
  when needs.mid $ do
    x <- _getMid sr.audioOutputAnalysisArray
    write x sr.mid
  when needs.hi $ do
    x <- _getHi sr.audioOutputAnalysisArray
    write x sr.hi
-}

foreign import data WebAudioContext :: Type

foreign import defaultWebAudioContext :: Effect WebAudioContext

foreign import data WebAudioNode :: Type

foreign import _monoGainNode :: WebAudioContext -> Number -> Effect WebAudioNode

foreign import _analyserNode :: WebAudioContext -> Int -> Number -> Effect WebAudioNode

foreign import _connect :: WebAudioNode -> WebAudioNode -> Effect Unit

foreign import _disconnect :: WebAudioNode -> WebAudioNode -> Effect Unit

foreign import data AnalyserArray :: Type

foreign import _analyserArray :: Int -> Effect AnalyserArray

foreign import _getByteFrequencyData :: WebAudioNode -> AnalyserArray -> Effect Unit

foreign import _getLo :: AnalyserArray -> Effect Number
foreign import _getMid :: AnalyserArray -> Effect Number
foreign import _getHi:: AnalyserArray -> Effect Number
