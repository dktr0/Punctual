module WebGLCanvas where

import Prelude (Unit, bind, discard, pure, unit, (<$>), (<>), show, (/=),(||),(==))
import Effect
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Nullable (Nullable, toMaybe, null, notNull)
import Data.Maybe (Maybe(..),isJust)
import Data.Int (toNumber)

type WebGLCanvas = {
  canvas :: HTMLCanvasElement,
  width :: Ref Int,
  height :: Ref Int,
  gl :: WebGLContext,
  webGL2 :: Boolean,
  khr_parallel_shader_compile :: Boolean,
  postProgram :: WebGLProgram,
  webcamTexture :: WebGLTexture,
  frameBufferTexture0 :: WebGLTexture,
  frameBufferTexture1 :: WebGLTexture,
  frameBuffer0 :: WebGLFrameBuffer,
  frameBuffer1 :: WebGLFrameBuffer,
  frameBufferIndex :: Ref Int -- the index of the frame buffer to which drawing should be directed
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
  w <- _getCanvasWidth canvas
  h <- _getCanvasHeight canvas
  width <- new w
  height <- new h
  case webGL2 of
    false -> log "punctual will use WebGL1"
    true -> log "punctual will use WebGl2"
  appendCanvasToDocumentBody canvas
  khr_parallel_shader_compile <- isJust <$> getExtension gl "KHR_parallel_shader_compile"
  case khr_parallel_shader_compile of
    false -> pure unit
    true -> log "punctual can use WebGL extension KHR_parallel_shader_compile"
  -- defaultBlendFunc gl
  unpackFlipY gl
  postProgram <- _newPostProgram gl
  webcamTexture <- _createTexture gl
  frameBufferTexture0 <- _createTexture gl
  frameBufferTexture1 <- _createTexture gl
  frameBuffer0 <- _createFrameBuffer gl
  frameBuffer1 <- _createFrameBuffer gl
  frameBufferIndex <- new 0
  let glc = {
    canvas,
    width,
    height,
    gl,
    webGL2,
    khr_parallel_shader_compile,
    postProgram,
    webcamTexture,
    frameBufferTexture0,
    frameBufferTexture1,
    frameBuffer0,
    frameBuffer1,
    frameBufferIndex
    }
  _initializeFrameBufferTexture gl frameBufferTexture0 frameBuffer0 w h
  _initializeFrameBufferTexture gl frameBufferTexture1 frameBuffer1 w h
  pure glc
  
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

foreign import _createProgram :: WebGLContext -> Effect WebGLProgram

createProgram :: WebGLCanvas -> Effect WebGLProgram
createProgram glc = _createProgram glc.gl

foreign import data WebGLShader :: Type

foreign import _createVertexShader :: WebGLContext -> Effect WebGLShader

createVertexShader :: WebGLCanvas -> Effect WebGLShader
createVertexShader glc = _createVertexShader glc.gl

foreign import _createFragmentShader :: WebGLContext -> Effect WebGLShader

createFragmentShader :: WebGLCanvas -> Effect WebGLShader
createFragmentShader glc = _createFragmentShader glc.gl

foreign import _attachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> Effect Unit

attachShader :: WebGLCanvas -> WebGLProgram -> WebGLShader -> Effect Unit
attachShader glc = _attachShader glc.gl

foreign import _shaderSource :: WebGLContext -> WebGLShader -> String -> Effect Unit

shaderSource :: WebGLCanvas -> WebGLShader -> String -> Effect Unit
shaderSource glc = _shaderSource glc.gl

foreign import _compileShader :: WebGLContext -> WebGLShader -> Effect Unit

compileShader :: WebGLCanvas -> WebGLShader -> Effect Unit
compileShader glc = _compileShader glc.gl

foreign import _linkProgram :: WebGLContext-> WebGLProgram -> Effect Unit

linkProgram :: WebGLCanvas -> WebGLProgram -> Effect Unit
linkProgram glc = _linkProgram glc.gl

foreign import _flush :: WebGLContext -> Effect Unit

flush :: WebGLCanvas -> Effect Unit
flush glc = _flush glc.gl

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

foreign import data WebGLFrameBuffer :: Type

foreign import _createFrameBuffer :: WebGLContext -> Effect WebGLFrameBuffer

foreign import _initializeFrameBufferTexture :: WebGLContext -> WebGLTexture -> WebGLFrameBuffer -> Int -> Int -> Effect Unit

foreign import _reinitializeFrameBufferTexture :: WebGLContext -> WebGLTexture -> Int -> Int -> Effect Unit

postFragmentShaderSrc :: String
postFragmentShaderSrc = """
precision mediump float;
uniform vec2 r;
uniform sampler2D t;
uniform float b;
void main(){
  vec4 t = texture2D(t,gl_FragCoord.xy/r);
  gl_FragColor = vec4(t.xyz*b,t.w);
}
"""
  
configureFrameBufferTextures :: WebGLCanvas -> Effect Unit
configureFrameBufferTextures glc = do
  wPrev <- read glc.width
  hPrev <- read glc.height
  w <- getCanvasWidth glc
  h <- getCanvasHeight glc
  case (wPrev /= w || hPrev /= h) of
    false -> pure unit
    true -> do
      _reinitializeFrameBufferTexture glc.gl glc.frameBufferTexture0 w h
      _reinitializeFrameBufferTexture glc.gl glc.frameBufferTexture1 w h
      write w glc.width
      write h glc.height
  
  
_newPostProgram :: WebGLContext -> Effect WebGLProgram
_newPostProgram gl = do
  glProg <- _createProgram gl
  vShader <- _createVertexShader gl
  _attachShader gl glProg vShader
  _shaderSource gl vShader "attribute vec4 p; void main() { gl_Position = p; }"
  _compileShader gl vShader
  fShader <- _createFragmentShader gl
  _attachShader gl glProg fShader
  _shaderSource gl fShader postFragmentShaderSrc
  _compileShader gl fShader
  _linkProgram gl glProg
  _flush gl
  pure glProg

drawPostProgram :: WebGLCanvas -> Effect Unit
drawPostProgram glc = do
  let p = glc.postProgram
  useProgram glc p
  t <- getOutputTexture glc
  w <- getCanvasWidth glc
  h <- getCanvasHeight glc
  bindTexture glc p t 0 "t"
  setUniform1f glc p "b" 1.0
  setUniform2f glc p "r" (toNumber w) (toNumber h) 
  viewport glc 0 0 w h
  harmonizeCanvasDimensions glc
  bindFrameBuffer glc Nothing
  drawDefaultTriangleStrip glc
  toggleFrameBuffers glc
    
-- bind a texture to a specified active texture slot and pass that location into a shader program by setting a uniform
bindTexture :: WebGLCanvas -> WebGLProgram -> WebGLTexture -> Int -> String -> Effect Unit
bindTexture glc shader t n uName = do
  activeTexture glc n
  bindTexture2D glc t
  setUniform1i glc shader uName n

bindFrameBuffer :: WebGLCanvas -> Maybe WebGLFrameBuffer -> Effect Unit
bindFrameBuffer glc Nothing = _bindFrameBuffer glc.gl null
bindFrameBuffer glc (Just b) = _bindFrameBuffer glc.gl (notNull b)

foreign import _bindFrameBuffer :: WebGLContext -> Nullable WebGLFrameBuffer -> Effect Unit

getOutputTexture :: WebGLCanvas -> Effect WebGLTexture
getOutputTexture glc = do
  frameBufferIndex <- read glc.frameBufferIndex
  case frameBufferIndex of
    0 -> pure glc.frameBufferTexture0
    _ -> pure glc.frameBufferTexture1
    
getOutputFrameBuffer :: WebGLCanvas -> Effect WebGLFrameBuffer
getOutputFrameBuffer glc = do
  frameBufferIndex <- read glc.frameBufferIndex
  case frameBufferIndex of
    0 -> pure glc.frameBuffer0
    _ -> pure glc.frameBuffer1
    
getFeedbackTexture :: WebGLCanvas -> Effect WebGLTexture
getFeedbackTexture glc = do
  frameBufferIndex <- read glc.frameBufferIndex
  case frameBufferIndex of
    0 -> pure glc.frameBufferTexture1
    _ -> pure glc.frameBufferTexture0
    
toggleFrameBuffers :: WebGLCanvas -> Effect Unit
toggleFrameBuffers glc = do
  frameBufferIndex <- read glc.frameBufferIndex
  case frameBufferIndex of
    0 -> write 1 glc.frameBufferIndex
    _ -> write 0 glc.frameBufferIndex
    
getCanvasWidth :: WebGLCanvas -> Effect Int
getCanvasWidth glc = _getCanvasWidth glc.canvas

foreign import _getCanvasWidth :: HTMLCanvasElement -> Effect Int

getCanvasHeight :: WebGLCanvas -> Effect Int
getCanvasHeight glc = _getCanvasHeight glc.canvas

foreign import _getCanvasHeight :: HTMLCanvasElement -> Effect Int

harmonizeCanvasDimensions :: WebGLCanvas -> Effect Unit
harmonizeCanvasDimensions glc = _harmonizeCanvasDimensions glc.canvas

foreign import _harmonizeCanvasDimensions :: HTMLCanvasElement -> Effect Unit


