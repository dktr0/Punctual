{-# LANGUAGE FlexibleInstances, JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.GL where

import GHCJS.DOM.Types hiding (Text)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.Reader
import Data.Time
import TextShow

logTime :: MonadIO m => Text -> m a -> m a
logTime l m = do
  t1 <- liftIO $ getCurrentTime
  a <- m
  t2 <- liftIO $ getCurrentTime
  liftIO $ T.putStrLn $ l <> " " <> showt (round (diffUTCTime t2 t1 * 1000) :: Int) <> " ms"
  return a

data GLContext = GLContext {
  _webGLRenderingContext :: WebGLRenderingContext,
  _khr_parallel_shader_compile :: Bool,
  _completion_status_khr :: JSVal -- cached value of ext.COMPLETION_STATUS_KHR if KHR_parallel_shader_compile enabled
}

type GL = ReaderT GLContext IO

gl :: GL WebGLRenderingContext
gl = _webGLRenderingContext <$> ask

khr_parallel_shader_compile :: GL Bool
khr_parallel_shader_compile = _khr_parallel_shader_compile <$> ask

runGL :: GLContext -> GL a -> IO a
runGL ctx x = (runReaderT x) ctx

newGLContext :: HTMLCanvasElement -> IO GLContext
newGLContext cv = do
  ctx <- _getWebGLRenderingContext cv
  psc <- _extensionIsAvailable "KHR_parallel_shader_compile" ctx
  when psc $ T.putStrLn "Punctual: WebGL extension khr_parallel_shader_compile is available"
  when (not psc) $ T.putStrLn "Punctual: WebGL extension khr_parallel_shader_compile is not available (this is okay)"
  cs <- _getCompletionStatusKhr ctx
  return $ GLContext {
    _webGLRenderingContext = ctx,
    _khr_parallel_shader_compile = psc,
    _completion_status_khr = cs
  }

foreign import javascript unsafe
  "$1.getContext('webgl',{ powerPreference: 'high-performance', antialias: true })"
  _getWebGLRenderingContext :: HTMLCanvasElement -> IO WebGLRenderingContext

foreign import javascript unsafe
  "$2.getExtension($1) != null"
  _extensionIsAvailable :: Text -> WebGLRenderingContext -> IO Bool

foreign import javascript unsafe
  "$r = null; if($1.getExtension('KHR_parallel_shader_compile') != null) $r = $1.getExtension('KHR_parallel_shader_compile').COMPLETION_STATUS_KHR;"
  _getCompletionStatusKhr :: WebGLRenderingContext -> IO JSVal

getProgramParameterCompletionStatus :: WebGLProgram -> GL Bool
getProgramParameterCompletionStatus p = do
  ctx <- gl
  psc <- khr_parallel_shader_compile
  cs <- _completion_status_khr <$> ask
  liftIO $ if not psc then return True else _getProgramParameterCompletionStatus p cs ctx

foreign import javascript unsafe
  "$3.getProgramParameter($1,$2)"
  _getProgramParameterCompletionStatus :: WebGLProgram -> JSVal -> WebGLRenderingContext -> IO Bool

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

deleteTexture :: WebGLTexture -> GL ()
deleteTexture x = gl >>= (liftIO . _deleteTexture x)

foreign import javascript unsafe
  "$2.deleteTexture($1)"
  _deleteTexture :: WebGLTexture -> WebGLRenderingContext -> IO ()

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
activeTexture 8 = gl >>= (liftIO . _activeTexture8)
activeTexture 9 = gl >>= (liftIO . _activeTexture9)
activeTexture 10 = gl >>= (liftIO . _activeTexture10)
activeTexture 11 = gl >>= (liftIO . _activeTexture11)
activeTexture 12 = gl >>= (liftIO . _activeTexture12)
activeTexture 13 = gl >>= (liftIO . _activeTexture13)
activeTexture 14 = gl >>= (liftIO . _activeTexture14)
activeTexture 15 = gl >>= (liftIO . _activeTexture15)
activeTexture _ = error "activeTexture called with out of range argument"

foreign import javascript unsafe "$1.activeTexture($1.TEXTURE0)" _activeTexture0 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE1)" _activeTexture1 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE2)" _activeTexture2 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE3)" _activeTexture3 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE4)" _activeTexture4 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE5)" _activeTexture5 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE6)" _activeTexture6 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE7)" _activeTexture7 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE8)" _activeTexture8 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE9)" _activeTexture9 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE10)" _activeTexture10 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE11)" _activeTexture11 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE12)" _activeTexture12 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE13)" _activeTexture13 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE14)" _activeTexture14 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE15)" _activeTexture15 :: WebGLRenderingContext -> IO ()

uniform1i :: WebGLUniformLocation -> Int -> GL ()
uniform1i loc i = gl >>= (liftIO . _uniform1i loc i)

foreign import javascript unsafe
  "$3.uniform1i($1,$2);"
  _uniform1i :: WebGLUniformLocation -> Int -> WebGLRenderingContext -> IO ()

getShaderInfoLog :: WebGLShader -> GL Text
getShaderInfoLog s = gl >>= (liftIO . _getShaderInfoLog s)

foreign import javascript unsafe
  "$2.getShaderInfoLog($1)"
  _getShaderInfoLog :: WebGLShader -> WebGLRenderingContext -> IO Text

getShaderParameterCompileStatus :: WebGLShader -> GL Int
getShaderParameterCompileStatus s = gl >>= (liftIO . _getShaderParameterCompileStatus s)

foreign import javascript unsafe
  "$2.getShaderParameter($1,$2.COMPILE_STATUS)"
  _getShaderParameterCompileStatus :: WebGLShader -> WebGLRenderingContext -> IO Int

getProgramInfoLog :: WebGLProgram -> GL Text
getProgramInfoLog p = gl >>= (liftIO . _getProgramInfoLog p)

foreign import javascript unsafe
  "$2.getProgramInfoLog($1)"
  _getProgramInfoLog :: WebGLProgram -> WebGLRenderingContext -> IO Text

disableScissorTest :: GL ()
disableScissorTest = gl >>= (liftIO . _disableScissorTest)

foreign import javascript unsafe
  "$1.disable($1.SCISSOR_TEST);"
  _disableScissorTest :: WebGLRenderingContext -> IO ()

viewport :: Int -> Int -> Int -> Int -> GL ()
viewport x y w h = gl >>= (liftIO . _viewport x y w h)

foreign import javascript unsafe
  "$5.viewport($1,$2,$3,$4)"
  _viewport :: Int -> Int -> Int -> Int -> WebGLRenderingContext -> IO ()
