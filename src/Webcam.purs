module Webcam where

import Prelude (bind,discard,pure,Unit,unit)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Maybe (Maybe(..))

import WebGLCanvas

type Webcam = {
  texture :: WebGLTexture,
  mElementRef :: Ref (Maybe WebcamElement)
  }
  
foreign import data WebcamElement :: Type
  
newWebcam :: WebGLCanvas -> Effect Webcam
newWebcam glc = do
  texture <- createTexture glc
  mElementRef <- new Nothing
  pure { texture, mElementRef }

setActive :: Webcam -> Boolean -> Effect Unit
setActive wc true = do
  mElement <- read wc.mElementRef
  case mElement of
    Just _ -> pure unit
    Nothing -> do
      log "punctual: activating webcam"
      e <- newWebcamElement
      write (Just e) wc.mElementRef 
      
setActive wc false = do
  mElement <- read wc.mElementRef
  case mElement of
    Just e -> do
      log "punctual: disactivating webcam"
      stopWebcamElement e
      write Nothing wc.mElementRef
    Nothing -> pure unit
  
foreign import newWebcamElement :: Effect WebcamElement

foreign import stopWebcamElement :: WebcamElement -> Effect Unit
  
updateTexture :: WebGLCanvas -> Webcam -> Effect Unit
updateTexture glc wc = do
  mElement <- read wc.mElementRef
  case mElement of
    Just e -> updateWebcamTexture glc wc.texture e
    Nothing -> pure unit

foreign import updateWebcamTexture :: WebGLCanvas -> WebGLTexture -> WebcamElement -> Effect Unit

