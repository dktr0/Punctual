{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
  newPunctualWebGL,
  updatePunctualWebGL,
  drawFrame)
  where

import Control.Monad
import Control.Exception
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.Marshal.Pure
import Data.Time

import Sound.Punctual.FragmentShader
import Sound.Punctual.Evaluation
import Sound.Punctual.Types

foreign import javascript unsafe
  "$1.getContext('webgl')"
  getWebGLRenderingContext :: HTMLCanvasElement -> IO WebGLRenderingContext

foreign import javascript unsafe
  "$1.createShader($1.VERTEX_SHADER)"
  createVertexShader :: WebGLRenderingContext -> IO WebGLShader

foreign import javascript unsafe
  "$1.createShader($1.FRAGMENT_SHADER)"
  createFragmentShader :: WebGLRenderingContext -> IO WebGLShader

foreign import javascript unsafe
  "$1.shaderSource($2,$3);"
  shaderSource :: WebGLRenderingContext -> WebGLShader -> JSString -> IO ()

foreign import javascript unsafe
  "$1.compileShader($2);"
  compileShader :: WebGLRenderingContext -> WebGLShader -> IO ()

foreign import javascript unsafe
  "$1.getShaderParameter($2,$1.COMPILE_STATUS)"
  compileStatus :: WebGLRenderingContext -> WebGLShader -> IO Int

foreign import javascript unsafe
  "$1.getShaderInfoLog($2)"
  getShaderInfoLog :: WebGLRenderingContext -> WebGLShader -> IO JSString

makeVertexShader :: WebGLRenderingContext -> JSString -> IO WebGLShader
makeVertexShader glCtx srcCode = do
  shader <- createVertexShader glCtx
  shaderSource glCtx shader srcCode
  compileShader glCtx shader
  success <- compileStatus glCtx shader
  log <- getShaderInfoLog glCtx shader
  when (success == 0) $ throwIO $ userError $ "exception making vertex shader: " ++ fromJSString log
  return shader

makeFragmentShader :: WebGLRenderingContext -> JSString -> IO WebGLShader
makeFragmentShader glCtx srcCode = do
  shader <- createFragmentShader glCtx
  shaderSource glCtx shader srcCode
  compileShader glCtx shader
  success <- compileStatus glCtx shader
  log <- getShaderInfoLog glCtx shader
  when (success == 0) $ throwIO $ userError $ "exception making fragment shader: " ++ fromJSString log
  return shader

foreign import javascript unsafe
  "$1.createProgram()"
  createProgram :: WebGLRenderingContext -> IO WebGLProgram

foreign import javascript unsafe
  "$1.attachShader($2,$3);"
  attachShader :: WebGLRenderingContext -> WebGLProgram -> WebGLShader -> IO ()

foreign import javascript unsafe
  "$1.linkProgram($2);"
  linkProgram :: WebGLRenderingContext -> WebGLProgram -> IO ()

foreign import javascript unsafe
  "$1.getProgramParameter($2,$1.LINK_STATUS)"
  linkStatus :: WebGLRenderingContext -> WebGLProgram -> IO Int

foreign import javascript unsafe
  "$1.getProgramInfoLog($2)"
  getProgramInfoLog :: WebGLRenderingContext -> WebGLProgram -> IO JSString

foreign import javascript unsafe
  "$1.useProgram($2);"
  useProgram :: WebGLRenderingContext -> WebGLProgram -> IO ()

makeProgram :: WebGLRenderingContext -> WebGLShader -> WebGLShader -> IO WebGLProgram
makeProgram glCtx vShader fShader = do
  program <- createProgram glCtx
  attachShader glCtx program vShader
  attachShader glCtx program fShader
  linkProgram glCtx program
  putStrLn "post-link"
  success <- linkStatus glCtx program
  log <- getProgramInfoLog glCtx program
  when (success == 0) $ throwIO $ userError $ "exception linking program: " ++ fromJSString log
  useProgram glCtx program
  return program

defaultVertexShader :: String
defaultVertexShader = "attribute vec4 p; void main() { gl_Position = p; }"

foreign import javascript unsafe
  "$1.getAttribLocation($2,$3)"
  getAttribLocation :: WebGLRenderingContext -> WebGLProgram -> JSString -> IO Int

foreign import javascript unsafe
  "$1.createBuffer()"
  createBuffer :: WebGLRenderingContext -> IO WebGLBuffer

data PunctualWebGL = PunctualWebGL {
  renderingContext :: WebGLRenderingContext,
  vShader :: WebGLShader,
  fShader :: WebGLShader,
  drawingBuffer :: WebGLBuffer,
  tLocation :: WebGLUniformLocation,
  resLocation :: WebGLUniformLocation,
  prevExpressions :: [Expression]
  }

newPunctualWebGL :: HTMLCanvasElement -> IO PunctualWebGL
newPunctualWebGL canvas = do
  -- stuff we only need to do once per canvas
  glCtx <- getWebGLRenderingContext canvas
  v <- makeVertexShader glCtx (toJSString defaultVertexShader)
  b <- createBuffer glCtx
  bindBufferArray glCtx b
  bufferDataArrayStatic glCtx
  -- stuff we need to do everytime fragment shader changes
  f <- makeFragmentShader glCtx (toJSString defaultFragmentShader)
  program <- makeProgram glCtx v f
  p <- getAttribLocation glCtx program "p"
  bindBufferArray glCtx b
  vertexAttribPointer glCtx p
  enableVertexAttribArray glCtx p
  t <- getUniformLocation glCtx program "t"
  res <- getUniformLocation glCtx program "res"
  return $ PunctualWebGL {
    renderingContext = glCtx,
    vShader = v,
    fShader = f,
    drawingBuffer = b,
    tLocation = t,
    resLocation = res,
    prevExpressions = []
  }

foreign import javascript unsafe
  "$1.getUniformLocation($2,$3)"
  getUniformLocation :: WebGLRenderingContext -> WebGLProgram -> JSString -> IO WebGLUniformLocation

foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

updatePunctualWebGL :: PunctualWebGL -> (UTCTime,Double) -> Evaluation -> IO PunctualWebGL
updatePunctualWebGL st tempo e = do
  -- fragmentShader :: [Expression] -> (UTCTime,Double) -> Evaluation -> String
  let shaderSrc = fragmentShader (prevExpressions st) tempo e
  putStrLn $ "new shader source: " ++ shaderSrc
  st' <- replaceFragmentShader st (toJSString shaderSrc)
  return $ st' { prevExpressions = fst e }

replaceFragmentShader :: PunctualWebGL -> JSString -> IO PunctualWebGL
replaceFragmentShader st src = do
  let glCtx = renderingContext st
  f <- makeFragmentShader glCtx (toJSString src)
  program <- makeProgram glCtx (vShader st) f
  p <- getAttribLocation glCtx program "p"
  bindBufferArray glCtx (drawingBuffer st)
  vertexAttribPointer glCtx p
  enableVertexAttribArray glCtx p
  t <- getUniformLocation glCtx program "t"
  res <- getUniformLocation glCtx program "res"
  return $ st {
    fShader = f,
    tLocation = t,
    resLocation = res
  }

foreign import javascript unsafe
  "$1.bindBuffer($1.ARRAY_BUFFER,$2);"
  bindBufferArray :: WebGLRenderingContext -> WebGLBuffer -> IO ()

drawFrame :: Double -> PunctualWebGL -> IO ()
drawFrame t st = do
  let glCtx = renderingContext st
  clearColor glCtx 0.0 0.0 0.0 1.0 -- probably should comment this out
  clearColorBuffer glCtx -- probably should comment this out
  uniform1f glCtx (tLocation st) t
  uniform2f glCtx (resLocation st) 1920 1080
  drawArraysTriangleStrip glCtx 0 4

foreign import javascript unsafe
  "$1.uniform1f($2,$3);"
  uniform1f :: WebGLRenderingContext -> WebGLUniformLocation -> Double -> IO ()

foreign import javascript unsafe
  "$1.uniform2f($2,$3,$4);"
  uniform2f :: WebGLRenderingContext -> WebGLUniformLocation -> Double -> Double -> IO ()

foreign import javascript unsafe
  "$1.clearColor($2,$3,$4,$5);"
  clearColor :: WebGLRenderingContext -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe
  "$1.clear($1.COLOR_BUFFER_BIT);"
  clearColorBuffer :: WebGLRenderingContext -> IO ()

foreign import javascript unsafe
  "$1.vertexAttribPointer($2,2,$1.FLOAT,false,0,0);"
  vertexAttribPointer :: WebGLRenderingContext -> Int -> IO ()

foreign import javascript unsafe
  "$1.enableVertexAttribArray($2);"
  enableVertexAttribArray :: WebGLRenderingContext -> Int -> IO ()

foreign import javascript unsafe
  "$1.drawArrays($1.TRIANGLE_STRIP,$2,$3);"
  drawArraysTriangleStrip :: WebGLRenderingContext -> Int -> Int -> IO ()
