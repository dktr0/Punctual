module Sound.Punctual.Texture
  (Texture(..),
  createImageTexture)
  where

import GHCJS.DOM.Types hiding (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Text

import Sound.Punctual.GL

-- An abstraction providing a common interface to both static image
-- and video textures for the WebGL side of Punctual.

data Texture = Texture {
--  videoObject :: Maybe Video,
  imageObject :: Maybe Image,
  webGLTexture :: WebGLTexture
  }

-- newtype Video = Video JSVal

newtype Image = Image JSVal

createImageTexture :: Text -> GL Texture
createImageTexture url = do
  ctx <- gl
  img <- liftIO _createImage
  t <- createTexture
  liftIO $ _imageOnLoad ctx img t
  liftIO $ _imageURL img url
  return $ Texture {
--    videoObject = Nothing,
    imageObject = Just img,
    webGLTexture = t
  }

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
  _imageURL :: Image -> Text -> IO ()
