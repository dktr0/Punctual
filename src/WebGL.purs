module WebGL where

import Prelude ((<$>),bind,discard,pure,Unit,($),(<>),show)
import Effect
import Effect.Console (log)
import Effect.Ref (Ref, new, write, read)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..),isJust)
import Data.Tempo (Tempo)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds,Seconds)
import Data.DateTime (DateTime,diff)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Data.Tempo (origin)

import Program (Program,emptyProgram)
import FragmentShader (fragmentShader)


type WebGL = {
  canvas :: Canvas,
  gl :: WebGLContext,
  webGL2 :: Boolean,
  khr_parallel_shader_compile :: Boolean,
  triangleStripBuffer :: WebGLBuffer,
  program :: Ref Program,
  shaderSrc :: Ref String,
  shader :: Ref WebGLProgram
  }  

newWebGL :: Tempo -> Program -> Effect (Maybe WebGL)
newWebGL tempo program = do
  canvas <- createCanvas
  m2 <- getWebGL2Context canvas
  case m2 of
    Just gl -> Just <$> setupWebGL canvas gl true tempo program
    Nothing -> do
      m1 <- getWebGL1Context canvas
      case m1 of 
        Just gl -> Just <$> setupWebGL canvas gl false tempo program
        Nothing -> pure Nothing

setupWebGL :: Canvas -> WebGLContext -> Boolean -> Tempo -> Program -> Effect WebGL
setupWebGL canvas gl webGL2 tempo prog = do
  log $ "webGL2: " <> show webGL2
  appendCanvasToDocumentBody canvas
  khr_parallel_shader_compile <- isJust <$> getExtension gl "KHR_parallel_shader_compile"
  log $ "khr_parallel_shader_compile: " <> show khr_parallel_shader_compile
  defaultBlendFunc gl
  unpackFlipY gl
  triangleStripBuffer <- newDefaultTriangleStrip gl
  previousProgram <- emptyProgram
  Tuple shaderSrc' shader' <- updateFragmentShader gl webGL2 tempo previousProgram prog
  program <- new prog
  shaderSrc <- new shaderSrc'
  shader <- new shader'
  pure {
    canvas,
    gl,
    webGL2,
    khr_parallel_shader_compile,
    triangleStripBuffer,
    program,
    shaderSrc,
    shader
    }
    
updateWebGL :: WebGL -> Tempo -> Program -> Effect Unit
updateWebGL webGL tempo program = do
  previousProgram <- read webGL.program
  Tuple shaderSrc shader <- updateFragmentShader webGL.gl webGL.webGL2 tempo previousProgram program
  write program webGL.program
  write shaderSrc webGL.shaderSrc
  write shader webGL.shader
  
  
updateFragmentShader :: WebGLContext -> Boolean -> Tempo -> Program -> Program -> Effect (Tuple String WebGLProgram)
updateFragmentShader gl webGL2 tempo oldProg newProg = do
  t0 <- nowDateTime
  let shaderSrc = fragmentShader webGL2 tempo oldProg newProg
  t1 <- nowDateTime
  log $ " GLSL transpile time = " <> show (diff t1 t0 :: Milliseconds)
  glProg <- createProgram gl
  vShader <- createVertexShader gl
  attachShader gl glProg vShader
  shaderSource gl vShader "attribute vec4 p; void main() { gl_Position = p; }"
  compileShader gl vShader
  fShader <- createFragmentShader gl
  attachShader gl glProg fShader
  shaderSource gl fShader shaderSrc
  compileShader gl fShader
  linkProgram gl glProg
  flush gl
  pure $ Tuple shaderSrc glProg
 
 
deleteWebGL :: WebGL -> Effect Unit
deleteWebGL webGL = deleteCanvasFromDocumentBody webGL.canvas


drawWebGL :: WebGL -> Tempo -> DateTime -> Effect Unit
drawWebGL webGL tempo now = do
  let gl = webGL.gl
  shader <- read webGL.shader
  useProgram gl shader
  pLoc <- getAttribLocation gl shader "p"
  bindBufferArray gl webGL.triangleStripBuffer
  vertexAttribPointer gl pLoc
  enableVertexAttribArray gl pLoc
  viewport gl 0 0 1920 1080 -- placeholder, need to read current width and height
  clearColor gl 0.0 0.0 0.0 0.0
  clearColorBuffer gl
  setUniform2f gl shader "res" 1920.0 1080.0
  setUniform1f gl shader "_time" $ unwrap (diff now (origin tempo) :: Seconds)
  eTime <- _.evalTime <$> read webGL.program
  setUniform1f gl shader "_etime" $ unwrap (diff now eTime :: Seconds)
  -- setUniform1f gl shader "_beat" $ timeToCount tempo now
  -- setUniform1f gl shader "_ebeat" $ ??? (eTime' * realToFrac (freq tempo))
  drawDefaultTriangleStrip gl


foreign import data Canvas :: Type

foreign import createCanvas :: Effect Canvas

foreign import appendCanvasToDocumentBody :: Canvas -> Effect Unit

foreign import deleteCanvasFromDocumentBody :: Canvas -> Effect Unit

foreign import data WebGLContext :: Type

foreign import _getWebGL1Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL1Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL1Context c = toMaybe <$> _getWebGL1Context c

foreign import _getWebGL2Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL2Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL2Context c = toMaybe <$> _getWebGL2Context c

foreign import data WebGLExtension :: Type

foreign import _getExtension :: WebGLContext -> String -> Effect (Nullable WebGLExtension)

getExtension :: WebGLContext -> String -> Effect (Maybe WebGLExtension)
getExtension gl n = toMaybe <$> _getExtension gl n

foreign import defaultBlendFunc :: WebGLContext -> Effect Unit

foreign import unpackFlipY :: WebGLContext -> Effect Unit

foreign import data WebGLBuffer :: Type

foreign import createBuffer :: WebGLContext -> Effect WebGLBuffer

foreign import bindBufferArray :: WebGLContext -> WebGLBuffer -> Effect Unit

foreign import bufferData_defaultTriangleStrip :: WebGLContext -> Effect Unit

newDefaultTriangleStrip :: WebGLContext -> Effect WebGLBuffer
newDefaultTriangleStrip gl = do
  b <- createBuffer gl
  bindBufferArray gl b
  bufferData_defaultTriangleStrip gl
  pure b

foreign import data WebGLProgram :: Type

foreign import createProgram :: WebGLContext -> Effect WebGLProgram

foreign import data WebGLShader :: Type

foreign import createVertexShader :: WebGLContext -> Effect WebGLShader

foreign import createFragmentShader :: WebGLContext -> Effect WebGLShader

foreign import attachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> Effect Unit

foreign import shaderSource :: WebGLContext -> WebGLShader -> String -> Effect Unit

foreign import compileShader :: WebGLContext -> WebGLShader -> Effect Unit

foreign import linkProgram :: WebGLContext -> WebGLProgram -> Effect Unit

foreign import flush :: WebGLContext -> Effect Unit

foreign import useProgram :: WebGLContext -> WebGLProgram -> Effect Unit

foreign import getAttribLocation :: WebGLContext -> WebGLProgram -> String -> Effect Int

foreign import vertexAttribPointer :: WebGLContext -> Int -> Effect Unit

foreign import enableVertexAttribArray :: WebGLContext -> Int -> Effect Unit
  
foreign import viewport :: WebGLContext -> Int -> Int -> Int -> Int -> Effect Unit

foreign import clearColor :: WebGLContext -> Number -> Number -> Number -> Number -> Effect Unit

foreign import clearColorBuffer :: WebGLContext -> Effect Unit

foreign import drawDefaultTriangleStrip :: WebGLContext -> Effect Unit

foreign import data WebGLUniformLocation :: Type

foreign import getUniformLocation :: WebGLContext -> WebGLProgram -> String -> Effect WebGLUniformLocation

foreign import uniform1f :: WebGLContext -> WebGLUniformLocation -> Number -> Effect Unit

foreign import uniform2f :: WebGLContext -> WebGLUniformLocation -> Number -> Number -> Effect Unit

setUniform1f :: WebGLContext -> WebGLProgram -> String -> Number -> Effect Unit
setUniform1f gl p n x = do
  loc <- getUniformLocation gl p n
  uniform1f gl loc x
  
setUniform2f :: WebGLContext -> WebGLProgram -> String -> Number -> Number -> Effect Unit
setUniform2f gl p n x y = do
  loc <- getUniformLocation gl p n
  uniform2f gl loc x y



