{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.Webcam
  (
  Webcam(..),
  new,
  setActive,
  updateTexture,
  )
  where

import GHCJS.DOM.Types hiding (Text)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Text.IO as T

import Sound.Punctual.GL

data Webcam = Webcam {
  _texture :: WebGLTexture,
  _video :: IORef (Maybe HTMLVideoElement)
  }
  
new :: GL Webcam
new = do
  v <- liftIO $ newIORef Nothing
  t <- createTexture
  pure $ Webcam {
    _video = v,
    _texture = t
  }


setActive :: Webcam -> Bool -> GL ()
setActive wc True = liftIO $ do
  mve <- readIORef $ _video wc
  case mve of
    Just _ -> pure ()
    Nothing -> do
      T.putStrLn "Punctual: activating webcam"
      ve <- _videoForWebcam
      writeIORef (_video wc) (Just ve)
      
setActive wc False = liftIO $ do
  mve <- readIORef $ _video wc
  case mve of
    Just ve -> do
      T.putStrLn "Punctual: disactivating webcam"
      _stopVideo ve
      writeIORef (_video wc) Nothing
    Nothing -> pure ()
  

updateTexture :: Webcam -> GL ()
updateTexture wc = do
  mve <- liftIO $ readIORef $ _video wc
  case mve of
    Just ve -> do
      glCtx <- gl
      liftIO $ _updateTexture glCtx (_texture wc) ve
    Nothing -> pure ()


foreign import javascript safe
  "$r = document.createElement('video');\
  \$r.width = 2048; $r.height = 2048; $r.autoplay = true; $r.isPlaying = false;\
  \$r.addEventListener('playing',function() { $r.isPlaying = true; });\
  \navigator.mediaDevices.getUserMedia({video: true}).then( function(stream) { $r.srcObject = stream; } );"
  _videoForWebcam :: IO HTMLVideoElement
  
foreign import javascript safe
  "$1.srcObject.getTracks().forEach(function(track) { track.stop(); });"
  _stopVideo :: HTMLVideoElement -> IO ()

foreign import javascript safe
  "if($3.isPlaying) { $1.activeTexture($1.TEXTURE3);\
  \$1.bindTexture($1.TEXTURE_2D,$2);\
  \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, $3);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR); } "
  _updateTexture :: WebGLRenderingContext -> WebGLTexture -> HTMLVideoElement -> IO ()

