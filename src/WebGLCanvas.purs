module WebGLCanvas where

import Prelude (Unit, bind, discard, pure, unit, (<$>))
import Effect
import Effect.Console (log)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..),isJust)

type WebGLCanvas = {
  canvas :: HTMLCanvasElement,
  gl :: WebGLContext,
  webGL2 :: Boolean,
  khr_parallel_shader_compile :: Boolean,
  webcamTexture :: WebGLTexture
  }

newWebGLCanvas :: Effect (Maybe WebGLCanvas)
newWebGLCanvas = do
  canvas <- createHTMLCanvasElement
  m2 <- getWebGL2Context canvas
  case m2 of
    Just gl -> Just <$> setupWebGLCanvas canvas gl true
    Nothing -> do
      m1 <- getWebGL1Context canvas
      case m1 of 
        Just gl -> Just <$> setupWebGLCanvas canvas gl false
        Nothing -> pure Nothing

setupWebGLCanvas :: HTMLCanvasElement -> WebGLContext -> Boolean -> Effect WebGLCanvas
setupWebGLCanvas canvas gl webGL2 = do
  case webGL2 of
    false -> log "punctual will use WebGL1"
    true -> log "punctual will use WebGl2"
  appendCanvasToDocumentBody canvas
  khr_parallel_shader_compile <- isJust <$> getExtension gl "KHR_parallel_shader_compile"
  case khr_parallel_shader_compile of
    false -> pure unit
    true -> log "punctual can use WebGL extension KHR_parallel_shader_compile"
  defaultBlendFunc gl
  unpackFlipY gl
  webcamTexture <- _createTexture gl
  pure {
    canvas,
    gl,
    webGL2,
    khr_parallel_shader_compile,
    webcamTexture
    }
 
deleteWebGLCanvas :: WebGLCanvas -> Effect Unit
deleteWebGLCanvas webGL = deleteCanvasFromDocumentBody webGL.canvas

foreign import data HTMLCanvasElement :: Type

foreign import createHTMLCanvasElement :: Effect HTMLCanvasElement

foreign import appendCanvasToDocumentBody :: HTMLCanvasElement -> Effect Unit

foreign import deleteCanvasFromDocumentBody :: HTMLCanvasElement -> Effect Unit

foreign import data WebGLContext :: Type

foreign import _getWebGL1Context :: HTMLCanvasElement -> Effect (Nullable WebGLContext)

getWebGL1Context :: HTMLCanvasElement -> Effect (Maybe WebGLContext)
getWebGL1Context c = toMaybe <$> _getWebGL1Context c

foreign import _getWebGL2Context :: HTMLCanvasElement -> Effect (Nullable WebGLContext)

getWebGL2Context :: HTMLCanvasElement -> Effect (Maybe WebGLContext)
getWebGL2Context c = toMaybe <$> _getWebGL2Context c

foreign import data WebGLExtension :: Type

foreign import _getExtension :: WebGLContext -> String -> Effect (Nullable WebGLExtension)

getExtension :: WebGLContext -> String -> Effect (Maybe WebGLExtension)
getExtension gl n = toMaybe <$> _getExtension gl n

foreign import defaultBlendFunc :: WebGLContext -> Effect Unit

foreign import unpackFlipY :: WebGLContext -> Effect Unit

foreign import data WebGLBuffer :: Type

foreign import createBuffer :: WebGLCanvas -> Effect WebGLBuffer

foreign import bindBufferArray :: WebGLCanvas -> WebGLBuffer -> Effect Unit

foreign import bufferData_defaultTriangleStrip :: WebGLCanvas -> Effect Unit

newDefaultTriangleStrip :: WebGLCanvas -> Effect WebGLBuffer
newDefaultTriangleStrip gl = do
  b <- createBuffer gl
  bindBufferArray gl b
  bufferData_defaultTriangleStrip gl
  pure b

foreign import data WebGLProgram :: Type

foreign import createProgram :: WebGLCanvas -> Effect WebGLProgram

foreign import data WebGLShader :: Type

foreign import createVertexShader :: WebGLCanvas -> Effect WebGLShader

foreign import createFragmentShader :: WebGLCanvas -> Effect WebGLShader

foreign import attachShader :: WebGLCanvas -> WebGLProgram -> WebGLShader -> Effect Unit

foreign import shaderSource :: WebGLCanvas -> WebGLShader -> String -> Effect Unit

foreign import compileShader :: WebGLCanvas -> WebGLShader -> Effect Unit

foreign import linkProgram :: WebGLCanvas -> WebGLProgram -> Effect Unit

foreign import flush :: WebGLCanvas -> Effect Unit

foreign import useProgram :: WebGLCanvas -> WebGLProgram -> Effect Unit

foreign import getAttribLocation :: WebGLCanvas -> WebGLProgram -> String -> Effect Int

foreign import vertexAttribPointer :: WebGLCanvas -> Int -> Effect Unit

foreign import enableVertexAttribArray :: WebGLCanvas -> Int -> Effect Unit
  
foreign import viewport :: WebGLCanvas -> Int -> Int -> Int -> Int -> Effect Unit

foreign import clearColor :: WebGLCanvas -> Number -> Number -> Number -> Number -> Effect Unit

foreign import clearColorBuffer :: WebGLCanvas -> Effect Unit

foreign import drawDefaultTriangleStrip :: WebGLCanvas -> Effect Unit

foreign import data WebGLUniformLocation :: Type

foreign import getUniformLocation :: WebGLCanvas -> WebGLProgram -> String -> Effect WebGLUniformLocation

foreign import _uniform1i :: WebGLContext -> WebGLUniformLocation -> Int -> Effect Unit

foreign import _uniform1f :: WebGLContext -> WebGLUniformLocation -> Number -> Effect Unit

foreign import _uniform2f :: WebGLContext -> WebGLUniformLocation -> Number -> Number -> Effect Unit

setUniform1i :: WebGLCanvas -> WebGLProgram -> String -> Int -> Effect Unit
setUniform1i glc p n x = do
  loc <- getUniformLocation glc p n
  _uniform1i glc.gl loc x

setUniform1f :: WebGLCanvas -> WebGLProgram -> String -> Number -> Effect Unit
setUniform1f glc p n x = do
  loc <- getUniformLocation glc p n
  _uniform1f glc.gl loc x
  
setUniform2f :: WebGLCanvas -> WebGLProgram -> String -> Number -> Number -> Effect Unit
setUniform2f glc p n x y = do
  loc <- getUniformLocation glc p n
  _uniform2f glc.gl loc x y

foreign import data WebGLTexture :: Type

foreign import _createTexture :: WebGLContext -> Effect WebGLTexture

createTexture :: WebGLCanvas -> Effect WebGLTexture
createTexture glc = _createTexture glc.gl

activeTexture :: WebGLCanvas -> Int -> Effect Unit
activeTexture glc 0 = _activeTexture0 glc.gl
activeTexture glc 1 = _activeTexture1 glc.gl
activeTexture glc 2 = _activeTexture2 glc.gl
activeTexture glc 3 = _activeTexture3 glc.gl
activeTexture glc 4 = _activeTexture4 glc.gl
activeTexture glc 5 = _activeTexture5 glc.gl
activeTexture glc 6 = _activeTexture6 glc.gl
activeTexture glc 7 = _activeTexture7 glc.gl
activeTexture glc 8 = _activeTexture8 glc.gl
activeTexture glc 9 = _activeTexture9 glc.gl
activeTexture glc 10 = _activeTexture10 glc.gl
activeTexture glc 11 = _activeTexture11 glc.gl
activeTexture glc 12 = _activeTexture12 glc.gl
activeTexture glc 13 = _activeTexture13 glc.gl
activeTexture glc 14 = _activeTexture14 glc.gl
activeTexture glc 15 = _activeTexture15 glc.gl
activeTexture _ _ = log "strange error in punctual: activeTexture called with texture slot not >=0 && <= 15"

foreign import _activeTexture0 :: WebGLContext -> Effect Unit
foreign import _activeTexture1 :: WebGLContext -> Effect Unit
foreign import _activeTexture2 :: WebGLContext -> Effect Unit
foreign import _activeTexture3 :: WebGLContext -> Effect Unit
foreign import _activeTexture4 :: WebGLContext -> Effect Unit
foreign import _activeTexture5 :: WebGLContext -> Effect Unit
foreign import _activeTexture6 :: WebGLContext -> Effect Unit
foreign import _activeTexture7 :: WebGLContext -> Effect Unit
foreign import _activeTexture8 :: WebGLContext -> Effect Unit
foreign import _activeTexture9 :: WebGLContext -> Effect Unit
foreign import _activeTexture10 :: WebGLContext -> Effect Unit
foreign import _activeTexture11 :: WebGLContext -> Effect Unit
foreign import _activeTexture12 :: WebGLContext -> Effect Unit
foreign import _activeTexture13 :: WebGLContext -> Effect Unit
foreign import _activeTexture14 :: WebGLContext -> Effect Unit
foreign import _activeTexture15 :: WebGLContext -> Effect Unit

foreign import _bindTexture2D :: WebGLContext -> WebGLTexture -> Effect Unit

bindTexture2D :: WebGLCanvas -> WebGLTexture -> Effect Unit
bindTexture2D glc = _bindTexture2D glc.gl



