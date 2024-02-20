module WebGL where

import Prelude ((<$>),bind,discard,pure,Unit,($),(<>),show)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, write, read)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo,origin)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds,Seconds)
import Data.DateTime (DateTime,diff)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)

import Program (Program,emptyProgram)
import FragmentShader (fragmentShader)
import WebGLCanvas (WebGLBuffer, WebGLCanvas, WebGLProgram, attachShader, bindBufferArray, clearColor, clearColorBuffer, compileShader, createFragmentShader, createProgram, createVertexShader, deleteWebGLCanvas, drawDefaultTriangleStrip, enableVertexAttribArray, flush, getAttribLocation, linkProgram, newDefaultTriangleStrip, newWebGLCanvas, setUniform1f, setUniform2f, shaderSource, useProgram, vertexAttribPointer, viewport)


type WebGL = {
  glc :: WebGLCanvas,
  triangleStripBuffer :: WebGLBuffer,
  program :: Ref Program,
  shaderSrc :: Ref String,
  shader :: Ref WebGLProgram
  }  

newWebGL :: Tempo -> Program -> Effect (Maybe WebGL)
newWebGL tempo program = do
  mglc <- newWebGLCanvas
  case mglc of 
    Just glc -> Just <$> setupWebGL glc tempo program
    Nothing -> pure Nothing

setupWebGL :: WebGLCanvas -> Tempo -> Program -> Effect WebGL
setupWebGL glc tempo prog = do
  triangleStripBuffer <- newDefaultTriangleStrip glc
  previousProgram <- emptyProgram
  Tuple shaderSrc' shader' <- updateFragmentShader glc tempo previousProgram prog
  program <- new prog
  shaderSrc <- new shaderSrc'
  shader <- new shader'
  pure {
    glc,
    triangleStripBuffer,
    program,
    shaderSrc,
    shader
    }
    
updateWebGL :: WebGL -> Tempo -> Program -> Effect Unit
updateWebGL webGL tempo program = do
  previousProgram <- read webGL.program
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
  pure $ Tuple shaderSrc glProg
 
 
deleteWebGL :: WebGL -> Effect Unit
deleteWebGL webGL = deleteWebGLCanvas webGL.glc


drawWebGL :: WebGL -> Tempo -> DateTime -> Effect Unit
drawWebGL webGL tempo now = do
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
  setUniform1f glc shader "_time" $ unwrap (diff now (origin tempo) :: Seconds)
  eTime <- _.evalTime <$> read webGL.program
  setUniform1f glc shader "_etime" $ unwrap (diff now eTime :: Seconds)
  -- setUniform1f glc shader "_beat" $ timeToCount tempo now
  -- setUniform1f glc shader "_ebeat" $ ??? (eTime' * realToFrac (freq tempo))
  drawDefaultTriangleStrip glc

