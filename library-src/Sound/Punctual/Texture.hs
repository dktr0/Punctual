module Sound.Punctual.Texture
  (
  Texture(..),
  loadTexture,
  updateTexture
  )
  where

import GHCJS.DOM.Types hiding (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Data.Maybe (isJust,fromJust)

import Sound.Punctual.GL
import Sound.Punctual.Graph

-- An abstraction providing a common interface to both static image
-- and video textures for the WebGL side of Punctual.

data Texture = Texture {
  videoObject :: Maybe Video,
  imageObject :: Maybe Image,
  webGLTexture :: WebGLTexture
  }
  
instance Show Texture where
  show x = "a Texture, video=" ++ show (isJust (videoObject x)) ++ " image=" ++ show (isJust (imageObject x))

newtype Video = Video JSVal

newtype Image = Image JSVal

loadTexture :: TextureRef -> GL Texture
loadTexture (ImgRef url) = loadImageTexture url
loadTexture (VidRef url) = loadVideoTexture url

loadImageTexture :: Text -> GL Texture
loadImageTexture url = do
  ctx <- gl
  t <- createTexture
  img <- liftIO _createImage
  liftIO $ _imageOnLoad ctx img t
  liftIO $ _loadImage img url
  return $ Texture {
    videoObject = Nothing,
    imageObject = Just img,
    webGLTexture = t
  }

loadVideoTexture :: Text -> GL Texture
loadVideoTexture url = do
  t <- createTexture
  vid <- liftIO _createVideo
  liftIO $ _loadVideo vid url
  liftIO $ _videoOnPlaying vid
  return $ Texture {
    videoObject = Just vid,
    imageObject = Nothing,
    webGLTexture = t
  }

updateTexture :: Texture -> GL ()
updateTexture t
  | isJust (videoObject t) = do
    ctx <- gl
    liftIO $ _updateVideoTexture ctx (fromJust $ videoObject t) (webGLTexture t)
  | otherwise = return ()


foreign import javascript safe
  "$r = new Image(); $r.crossOrigin = \"Anonymous\";"
  _createImage :: IO Image

foreign import javascript safe
  "$2.onload = function() {\
     \$1.bindTexture($1.TEXTURE_2D, $3);\
     \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, $2);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR);\
     \};"
  _imageOnLoad :: WebGLRenderingContext -> Image -> WebGLTexture -> IO ()

foreign import javascript safe
  "$1.src = $2;"
  _loadImage :: Image -> Text -> IO ()


foreign import javascript safe
  "$r = document.createElement('video'); $r.crossOrigin = \"Anonymous\"; $r.autoplay = true; $r.muted = true; $r.loop = true; $r.isPlaying = false;"
  _createVideo :: IO Video

foreign import javascript safe
  "var closure = $1; $1.addEventListener('playing', function() { closure.isPlaying = true; });"
  _videoOnPlaying :: Video -> IO ()

foreign import javascript safe
  "$1.src = $2; $1.play();"
  _loadVideo :: Video -> Text -> IO ()

foreign import javascript safe
  "if($2.isPlaying){$1.bindTexture($1.TEXTURE_2D, $3);\
  \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, $2);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR);}"
  _updateVideoTexture :: WebGLRenderingContext -> Video -> WebGLTexture -> IO ()
