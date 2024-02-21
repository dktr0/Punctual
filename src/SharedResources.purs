module SharedResources where

-- management of all resources (tempo, images, videos, webcam, audio analysis, audio files, etc)
-- that are shared across different Punctual zones (and between different WebGL contexts, so no WebGL types)

import Prelude (bind,discard,pure,Unit,unit,(>>=))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo, newTempo)

import WebGLCanvas (WebGLCanvas, WebGLContext, WebGLTexture)

type SharedResources = {
  tempo :: Ref Tempo,
  mWebcamElementRef :: Ref (Maybe WebcamElement)
  }
  

newSharedResources :: Effect SharedResources
newSharedResources = do
  tempo <- newTempo (1 % 1) >>= new
  mWebcamElementRef <- new Nothing
  pure {
    tempo,
    mWebcamElementRef
  }

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

