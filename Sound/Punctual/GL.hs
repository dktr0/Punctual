{-# LANGUAGE FlexibleInstances, JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.GL where

import GHCJS.DOM.Types hiding (Text)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

type GL = ReaderT WebGLRenderingContext IO

runGL :: WebGLRenderingContext -> GL a -> IO a
runGL ctx x = (runReaderT x) ctx

foreign import javascript unsafe
  "$1.getContext('webgl')"
  getWebGLRenderingContext :: HTMLCanvasElement -> IO WebGLRenderingContext

gl :: GL WebGLRenderingContext
gl = ask

flush :: GL ()
flush = gl >>= (liftIO . _flush)

foreign import javascript unsafe
  "$1.flush()"
  _flush :: WebGLRenderingContext -> IO ()

createProgram :: GL WebGLProgram
createProgram = gl >>= (liftIO . _createProgram)

foreign import javascript unsafe
  "$1.createProgram()"
  _createProgram :: WebGLRenderingContext -> IO WebGLProgram

createVertexShader :: GL WebGLShader
createVertexShader = gl >>= (liftIO . _createVertexShader)

foreign import javascript unsafe
  "$1.createShader($1.VERTEX_SHADER)"
  _createVertexShader :: WebGLRenderingContext -> IO WebGLShader

createFragmentShader :: GL WebGLShader
createFragmentShader = gl >>= (liftIO . _createFragmentShader)

foreign import javascript unsafe
  "$1.createShader($1.FRAGMENT_SHADER)"
  _createFragmentShader :: WebGLRenderingContext -> IO WebGLShader

attachShader :: WebGLProgram -> WebGLShader -> GL ()
attachShader p s = gl >>= (liftIO . _attachShader p s)

foreign import javascript unsafe
  "$3.attachShader($1,$2);"
  _attachShader :: WebGLProgram -> WebGLShader -> WebGLRenderingContext -> IO ()

shaderSource :: WebGLShader -> Text -> GL ()
shaderSource s t = gl >>= (liftIO . _shaderSource s t)

foreign import javascript unsafe
  "$3.shaderSource($1,$2);"
  _shaderSource :: WebGLShader -> Text -> WebGLRenderingContext -> IO ()

compileShader :: WebGLShader -> GL ()
compileShader s = gl >>= (liftIO . _compileShader s)

foreign import javascript unsafe
  "$2.compileShader($1);"
  _compileShader :: WebGLShader -> WebGLRenderingContext -> IO ()

linkProgram :: WebGLProgram -> GL ()
linkProgram p = gl >>= (liftIO . _linkProgram p)

foreign import javascript unsafe
  "$2.linkProgram($1);"
  _linkProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()

linkStatus :: WebGLProgram -> GL Int
linkStatus p = gl >>= (liftIO . _linkStatus p)

foreign import javascript unsafe
  "$2.getProgramParameter($1,$2.LINK_STATUS)"
  _linkStatus :: WebGLProgram -> WebGLRenderingContext -> IO Int

useProgram :: WebGLProgram -> GL ()
useProgram p = gl >>= (liftIO . _useProgram p)

foreign import javascript unsafe
  "$2.useProgram($1);"
  _useProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()

getUniformLocation :: WebGLProgram -> Text -> GL WebGLUniformLocation
getUniformLocation p n = gl >>= (liftIO . _getUniformLocation p n)

foreign import javascript unsafe
  "$3.getUniformLocation($1,$2)"
  _getUniformLocation :: WebGLProgram -> Text -> WebGLRenderingContext -> IO WebGLUniformLocation

getAttribLocation :: WebGLProgram -> Text -> GL Int
getAttribLocation p n = gl >>= (liftIO . _getAttribLocation p n)

foreign import javascript unsafe
  "$3.getAttribLocation($1,$2)"
  _getAttribLocation :: WebGLProgram -> Text -> WebGLRenderingContext -> IO Int

deleteProgram :: WebGLProgram -> GL ()
deleteProgram p = gl >>= (liftIO . _deleteProgram p)

foreign import javascript unsafe
  "$2.deleteProgram($1);"
  _deleteProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()

deleteShader :: WebGLShader -> GL ()
deleteShader s = gl >>= (liftIO . _deleteShader s)

foreign import javascript unsafe
  "$2.deleteShader($1);"
  _deleteShader :: WebGLShader -> WebGLRenderingContext -> IO ()

createFramebuffer :: GL WebGLFramebuffer
createFramebuffer = gl >>= (liftIO . _createFramebuffer)

foreign import javascript unsafe
  "$1.createFramebuffer()"
  _createFramebuffer :: WebGLRenderingContext -> IO WebGLFramebuffer

createTexture :: GL WebGLTexture
createTexture = gl >>= (liftIO . _createTexture)

foreign import javascript unsafe
  "$1.createTexture()"
  _createTexture :: WebGLRenderingContext -> IO WebGLTexture

createBuffer :: GL WebGLBuffer
createBuffer = gl >>= (liftIO . _createBuffer)

foreign import javascript unsafe
  "$1.createBuffer()"
  _createBuffer :: WebGLRenderingContext -> IO WebGLBuffer

defaultBlendFunc :: GL ()
defaultBlendFunc = gl >>= (liftIO . _defaultBlendFunc)

foreign import javascript unsafe
  "$1.enable($1.BLEND); $1.blendFunc($1.ONE, $1.ONE_MINUS_SRC_ALPHA);"
  _defaultBlendFunc :: WebGLRenderingContext -> IO ()

uniform1f :: WebGLUniformLocation -> Double -> GL ()
uniform1f l v = gl >>= (liftIO . _uniform1f l v)

foreign import javascript unsafe
  "$3.uniform1f($1,$2);"
  _uniform1f :: WebGLUniformLocation -> Double -> WebGLRenderingContext -> IO ()

uniform2f :: WebGLUniformLocation -> Double -> Double -> GL ()
uniform2f l v1 v2 = gl >>= (liftIO . _uniform2f l v1 v2)

foreign import javascript unsafe
  "$4.uniform2f($1,$2,$3);"
  _uniform2f :: WebGLUniformLocation -> Double -> Double -> WebGLRenderingContext -> IO ()

clearColor :: Double -> Double -> Double -> Double -> GL ()
clearColor r g b a = gl >>= (liftIO . _clearColor r g b a)

foreign import javascript unsafe
  "$5.clearColor($1,$2,$3,$4);"
  _clearColor :: Double -> Double -> Double -> Double -> WebGLRenderingContext -> IO ()

clearColorBuffer :: GL ()
clearColorBuffer = gl >>= (liftIO . _clearColorBuffer)

foreign import javascript unsafe
  "$1.clear($1.COLOR_BUFFER_BIT);"
  _clearColorBuffer :: WebGLRenderingContext -> IO ()

vertexAttribPointer :: Int -> GL ()
vertexAttribPointer i = gl >>= (liftIO . _vertexAttribPointer i)

foreign import javascript unsafe
  "$2.vertexAttribPointer($1,2,$2.FLOAT,false,0,0);"
  _vertexAttribPointer :: Int -> WebGLRenderingContext -> IO ()

enableVertexAttribArray :: Int -> GL ()
enableVertexAttribArray i = gl >>= (liftIO . _enableVertexAttribArray i)

foreign import javascript unsafe
  "$2.enableVertexAttribArray($1);"
  _enableVertexAttribArray :: Int -> WebGLRenderingContext -> IO ()

drawArraysTriangleStrip :: Int -> Int -> GL ()
drawArraysTriangleStrip x y = gl >>= (liftIO . _drawArraysTriangleStrip x y)

foreign import javascript unsafe
  "$3.drawArrays($3.TRIANGLE_STRIP,$1,$2);"
  _drawArraysTriangleStrip :: Int -> Int -> WebGLRenderingContext -> IO ()

unpackFlipY :: GL ()
unpackFlipY = gl >>= (liftIO . _unpackFlipY)

foreign import javascript unsafe
  "$1.pixelStorei($1.UNPACK_FLIP_Y_WEBGL, true)"
  _unpackFlipY :: WebGLRenderingContext -> IO ()

bindBufferArray :: WebGLBuffer -> GL ()
bindBufferArray b = gl >>= (liftIO . _bindBufferArray b)

foreign import javascript unsafe
  "$2.bindBuffer($2.ARRAY_BUFFER,$1);"
  _bindBufferArray :: WebGLBuffer -> WebGLRenderingContext -> IO ()

bindTexture2D :: WebGLTexture -> GL ()
bindTexture2D t = gl >>= (liftIO . _bindTexture2D t)

foreign import javascript unsafe
  "$2.bindTexture($2.TEXTURE_2D,$1);"
  _bindTexture2D :: WebGLTexture -> WebGLRenderingContext -> IO ()

bindFramebuffer :: WebGLFramebuffer -> GL ()
bindFramebuffer f = gl >>= (liftIO . _bindFramebuffer f)

foreign import javascript unsafe
  "$2.bindFramebuffer($2.FRAMEBUFFER,$1);"
  _bindFramebuffer :: WebGLFramebuffer -> WebGLRenderingContext -> IO ()

bindFramebufferNull :: GL ()
bindFramebufferNull = gl >>= (liftIO . _bindFramebufferNull)

foreign import javascript unsafe
  "$1.bindFramebuffer($1.FRAMEBUFFER,null);"
  _bindFramebufferNull :: WebGLRenderingContext -> IO ()

activeTexture :: Int -> GL ()
activeTexture 0 = gl >>= (liftIO . _activeTexture0)
activeTexture 1 = gl >>= (liftIO . _activeTexture1)
activeTexture 2 = gl >>= (liftIO . _activeTexture2)
activeTexture 3 = gl >>= (liftIO . _activeTexture3)
activeTexture 4 = gl >>= (liftIO . _activeTexture4)
activeTexture 5 = gl >>= (liftIO . _activeTexture5)
activeTexture 6 = gl >>= (liftIO . _activeTexture6)
activeTexture 7 = gl >>= (liftIO . _activeTexture7)
activeTexture _ = error "activeTexture called with out of range argument"

foreign import javascript unsafe "$1.activeTexture($1.TEXTURE0)" _activeTexture0 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE1)" _activeTexture1 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE2)" _activeTexture2 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE3)" _activeTexture3 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE4)" _activeTexture4 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE5)" _activeTexture5 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE6)" _activeTexture6 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE7)" _activeTexture7 :: WebGLRenderingContext -> IO ()

uniform1i :: WebGLUniformLocation -> Int -> GL ()
uniform1i loc i = gl >>= (liftIO . _uniform1i loc i)

foreign import javascript unsafe
  "$3.uniform1i($1,$2);"
  _uniform1i :: WebGLUniformLocation -> Int -> WebGLRenderingContext -> IO ()

{- unused???
foreign import javascript unsafe
  "$1.getShaderParameter($2,$1.COMPILE_STATUS)"
  compileStatus :: WebGLRenderingContext -> WebGLShader -> IO Int

foreign import javascript unsafe
  "$1.getShaderInfoLog($2)"
  getShaderInfoLog :: WebGLRenderingContext -> WebGLShader -> IO Text

foreign import javascript unsafe
  "$1.getProgramInfoLog($2)"
  getProgramInfoLog :: WebGLRenderingContext -> WebGLProgram -> IO Text
-}
