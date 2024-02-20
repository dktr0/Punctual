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
  khr_parallel_shader_compile :: Boolean
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
  pure { canvas, gl, webGL2, khr_parallel_shader_compile }
 
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

foreign import uniform1f :: WebGLCanvas -> WebGLUniformLocation -> Number -> Effect Unit

foreign import uniform2f :: WebGLCanvas -> WebGLUniformLocation -> Number -> Number -> Effect Unit

setUniform1f :: WebGLCanvas -> WebGLProgram -> String -> Number -> Effect Unit
setUniform1f gl p n x = do
  loc <- getUniformLocation gl p n
  uniform1f gl loc x
  
setUniform2f :: WebGLCanvas -> WebGLProgram -> String -> Number -> Number -> Effect Unit
setUniform2f gl p n x y = do
  loc <- getUniformLocation gl p n
  uniform2f gl loc x y

foreign import data WebGLTexture :: Type

foreign import createTexture :: WebGLCanvas -> Effect WebGLTexture

