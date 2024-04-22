module SharedResources where

-- management of all resources (tempo, images, videos, webcam, audio analysis, audio files, etc)
-- that are shared across different Punctual zones (and between different WebGL contexts, so no WebGL types)

import Prelude (bind,discard,pure,Unit,unit,(>>=),($))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo, newTempo)
import Data.Map (Map, empty, lookup, insert)
import Data.Monoid.Disj (Disj)

import WebGLCanvas (WebGLCanvas, WebGLContext, WebGLTexture)
import WebAudio
import AudioAnalyser (AudioAnalyser,newInputAnalyser,newOutputAnalyser,updateAnalyser)

type SharedResources = {
  tempo :: Ref Tempo,
  mWebcamElementRef :: Ref (Maybe WebcamElement),
  images :: Ref (Map String Image),
  videos :: Ref (Map String Video),
  webAudioContext :: WebAudioContext,
  audioOutputNode :: WebAudioNode,
  inputAnalyser :: AudioAnalyser,
  outputAnalyser :: AudioAnalyser,
  audioWorkletCount :: Ref Int
  }
  

newSharedResources :: Maybe WebAudioContext -> Effect SharedResources
newSharedResources mWebAudioContext = do
  tempo <- newTempo (1 % 1) >>= new
  mWebcamElementRef <- new Nothing
  images <- new empty
  videos <- new empty
  webAudioContext <- case mWebAudioContext of
                       Nothing -> defaultWebAudioContext
                       Just x -> pure x
  audioOutputNode <- gainNode webAudioContext 1.0
  destination webAudioContext >>= connect audioOutputNode
  inputAnalyser <- newInputAnalyser webAudioContext
  outputAnalyser <- newOutputAnalyser webAudioContext audioOutputNode
  audioWorkletCount <- new 0
  pure {
    tempo,
    mWebcamElementRef,
    images,
    videos,
    webAudioContext,
    audioOutputNode,
    inputAnalyser,
    outputAnalyser,
    audioWorkletCount
    }
    
updateAudioAnalysers :: SharedResources -> forall r. { ifft::Disj Boolean, ilo::Disj Boolean, imid::Disj Boolean, ihi::Disj Boolean, fft::Disj Boolean, lo::Disj Boolean, mid::Disj Boolean, hi::Disj Boolean | r } -> Effect Unit
updateAudioAnalysers sr needs = do
  updateAnalyser sr.inputAnalyser { fft: needs.ifft, lo: needs.ilo, mid: needs.imid, hi: needs.ihi }
  updateAnalyser sr.outputAnalyser needs


-- Tempo

setTempo :: SharedResources -> Tempo -> Effect Unit
setTempo sr t = write t sr.tempo

getTempo :: SharedResources -> Effect Tempo
getTempo sr = read sr.tempo

-- Webcam

foreign import data WebcamElement :: Type

setWebcamActive :: SharedResources -> Boolean -> Effect Unit
setWebcamActive sr true = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just _ -> pure unit
    Nothing -> do
      log "punctual: activating webcam"
      e <- _newWebcamElement
      write (Just e) sr.mWebcamElementRef   
setWebcamActive sr false = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just e -> do
      log "punctual: disactivating webcam"
      _stopWebcamElement e
      write Nothing sr.mWebcamElementRef
    Nothing -> pure unit

foreign import _newWebcamElement :: Effect WebcamElement

foreign import _stopWebcamElement :: WebcamElement -> Effect Unit

foreign import _updateWebcamTexture :: WebGLContext -> WebGLTexture -> WebcamElement -> Effect Unit

updateWebcamTexture :: SharedResources -> WebGLCanvas -> Effect Unit
updateWebcamTexture sr glc = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just e -> _updateWebcamTexture glc.gl glc.webcamTexture e
    Nothing -> pure unit


-- Images

foreign import data Image :: Type

getImage :: SharedResources -> String -> Effect (Maybe Image)
getImage sr url = do
  images <- read sr.images
  case lookup url images of
    Nothing -> do
      i <- _newImage url
      write (insert url i images) sr.images
      pure Nothing
    Just i -> do
      loaded <- _imageIsLoaded i
      case loaded of
        true -> pure $ Just i
        false -> pure Nothing

foreign import _newImage :: String -> Effect Image

foreign import _imageIsLoaded :: Image -> Effect Boolean
  
  
-- Videos

foreign import data Video :: Type

getVideo :: SharedResources -> String -> Effect (Maybe Video)
getVideo sr url = do
  videos <- read sr.videos
  case lookup url videos of
    Nothing -> do
      v <- _newVideo url
      write (insert url v videos) sr.videos
      pure Nothing
    Just v -> do
      isPlaying <- _videoIsPlaying v
      case isPlaying of
        true -> pure $ Just v
        false -> pure Nothing

foreign import _newVideo :: String -> Effect Video

foreign import _videoIsPlaying :: Video -> Effect Boolean



