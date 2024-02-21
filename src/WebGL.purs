module WebGL where

import Prelude ((<$>),bind,discard,pure,Unit,($),(<>),show,(-))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, write, read)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo,origin,timeToCount)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds,Seconds)
import Data.DateTime (DateTime,diff)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Data.Rational (toNumber)

import Program (Program,emptyProgram)
import FragmentShader (fragmentShader)
import WebGLCanvas (WebGLBuffer, WebGLCanvas, WebGLProgram, attachShader, bindBufferArray, clearColor, clearColorBuffer, compileShader, createFragmentShader, createProgram, createVertexShader, deleteWebGLCanvas, drawDefaultTriangleStrip, enableVertexAttribArray, flush, getAttribLocation, linkProgram, newDefaultTriangleStrip, newWebGLCanvas, setUniform1f, setUniform2f, shaderSource, useProgram, vertexAttribPointer, viewport, WebGLTexture, activeTexture,bindTexture2D,setUniform1i)
import SharedResources

type WebGL = {
  sharedResources :: SharedResources,
  glc :: WebGLCanvas,
  triangleStripBuffer :: WebGLBuffer,
  program :: Ref Program,
  shaderSrc :: Ref String,
  shader :: Ref WebGLProgram
  }  

newWebGL :: SharedResources -> Program -> Effect (Maybe WebGL)
newWebGL sharedResources program = do
  mglc <- newWebGLCanvas
  case mglc of 
    Just glc -> Just <$> setupWebGL sharedResources glc program
    Nothing -> pure Nothing

setupWebGL :: SharedResources -> WebGLCanvas -> Program -> Effect WebGL
setupWebGL sharedResources glc prog = do
  triangleStripBuffer <- newDefaultTriangleStrip glc
  previousProgram <- emptyProgram
  tempo <- getTempo sharedResources
  Tuple shaderSrc' shader' <- updateFragmentShader glc tempo previousProgram prog
  program <- new prog
  shaderSrc <- new shaderSrc'
  shader <- new shader'
  pure {
    sharedResources,
    glc,
    triangleStripBuffer,
    program,
    shaderSrc,
    shader
    }
    
updateWebGL :: WebGL -> Program -> Effect Unit
updateWebGL webGL program = do
  previousProgram <- read webGL.program
  tempo <- getTempo webGL.sharedResources
  Tuple shaderSrc shader <- updateFragmentShader webGL.glc tempo previousProgram program
  write program webGL.program
  write shaderSrc webGL.shaderSrc
  write shader webGL.shader
  
  
updateFragmentShader :: WebGLCanvas -> Tempo -> Program -> Program -> Effect (Tuple String WebGLProgram)
updateFragmentShader glc tempo oldProg newProg = do
  t0 <- nowDateTime
  let shaderSrc = fragmentShader glc.webGL2 tempo oldProg newProg
  t1 <- nowDateTime
  log $ " GLSL transpile time = " <> show (diff t1 t0 :: Milliseconds)
  glProg <- createProgram glc
  vShader <- createVertexShader glc
  attachShader glc glProg vShader
  shaderSource glc vShader "attribute vec4 p; void main() { gl_Position = p; }"
  compileShader glc vShader
  fShader <- createFragmentShader glc
  attachShader glc glProg fShader
  shaderSource glc fShader shaderSrc
  compileShader glc fShader
  linkProgram glc glProg
  flush glc
  {-
  setActive :: Webcam -> Boolean -> Effect Unit
  ...if old or new -}
  pure $ Tuple shaderSrc glProg
 
 
deleteWebGL :: WebGL -> Effect Unit
deleteWebGL webGL = deleteWebGLCanvas webGL.glc


drawWebGL :: WebGL -> DateTime -> Effect Unit
drawWebGL webGL now = do
  t0 <- nowDateTime
  let glc = webGL.glc
  shader <- read webGL.shader
  useProgram glc shader
  pLoc <- getAttribLocation glc shader "p"
  bindBufferArray glc webGL.triangleStripBuffer
  vertexAttribPointer glc pLoc
  enableVertexAttribArray glc pLoc
  viewport glc 0 0 1920 1080 -- placeholder, need to read current width and height
  clearColor glc 0.0 0.0 0.0 0.0
  clearColorBuffer glc
  setUniform2f glc shader "res" 1920.0 1080.0
  tempo <- getTempo webGL.sharedResources
  setUniform1f glc shader "_time" $ unwrap (diff now (origin tempo) :: Seconds)
  eTime <- _.evalTime <$> read webGL.program
  setUniform1f glc shader "_etime" $ unwrap (diff now eTime :: Seconds)
  setUniform1f glc shader "_beat" $ toNumber $ timeToCount tempo now
  setUniform1f glc shader "_ebeat" $ toNumber $ timeToCount tempo now - timeToCount tempo eTime
  updateWebcamTexture webGL.sharedResources glc
  setTexture glc shader glc.webcamTexture 3 "w"
  drawDefaultTriangleStrip glc
  t1 <- nowDateTime
  log $ " draw time = " <> show (diff t1 t0 :: Milliseconds)


-- bind a texture to a specified active texture slot and pass that location into a shader program by setting a uniform
-- (TODO: try doing this only once instead of every draw call...)
setTexture :: WebGLCanvas -> WebGLProgram -> WebGLTexture -> Int -> String -> Effect Unit
setTexture glc shader t n uName = do
  activeTexture glc n
  bindTexture2D glc t
  setUniform1i glc shader uName n

