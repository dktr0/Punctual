{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
  emptyPunctualWebGL,
  updateRenderingContext,
  updateFragmentShader,
  evaluatePunctualWebGL,
  drawFrame)
  where

import Control.Monad
import Control.Exception
import GHCJS.Types
import GHCJS.DOM.Types hiding (Text)
import GHCJS.Marshal.Pure
import Data.Time
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.FragmentShader

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
  shaderSource :: WebGLRenderingContext -> WebGLShader -> Text -> IO ()

foreign import javascript unsafe
  "$1.compileShader($2);"
  compileShader :: WebGLRenderingContext -> WebGLShader -> IO ()

foreign import javascript unsafe
  "$1.getShaderParameter($2,$1.COMPILE_STATUS)"
  compileStatus :: WebGLRenderingContext -> WebGLShader -> IO Int

foreign import javascript unsafe
  "$1.getShaderInfoLog($2)"
  getShaderInfoLog :: WebGLRenderingContext -> WebGLShader -> IO Text

makeVertexShader :: WebGLRenderingContext -> Text -> IO WebGLShader
makeVertexShader glCtx srcCode = do
  shader <- createVertexShader glCtx
  shaderSource glCtx shader srcCode
  compileShader glCtx shader
  success <- compileStatus glCtx shader
  log <- getShaderInfoLog glCtx shader
  when (success == 0) $ throwIO $ userError $ "exception making vertex shader: " <> T.unpack log
  return shader

makeFragmentShader :: WebGLRenderingContext -> Text -> IO WebGLShader
makeFragmentShader glCtx srcCode = do
  shader <- createFragmentShader glCtx
  shaderSource glCtx shader srcCode
  compileShader glCtx shader
  success <- compileStatus glCtx shader
  log <- getShaderInfoLog glCtx shader
  when (success == 0) $ throwIO $ userError $ "exception making fragment shader: " <> T.unpack log
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
  getProgramInfoLog :: WebGLRenderingContext -> WebGLProgram -> IO Text

foreign import javascript unsafe
  "$1.useProgram($2);"
  useProgram :: WebGLRenderingContext -> WebGLProgram -> IO ()

makeProgram :: WebGLRenderingContext -> WebGLShader -> WebGLShader -> IO WebGLProgram
makeProgram glCtx vShader fShader = do
  program <- createProgram glCtx
  attachShader glCtx program vShader
  attachShader glCtx program fShader
  linkProgram glCtx program
  success <- linkStatus glCtx program
  log <- getProgramInfoLog glCtx program
  when (success == 0) $ throwIO $ userError $ "exception linking program: " <> T.unpack log
  useProgram glCtx program
  return program

defaultVertexShader :: Text
defaultVertexShader = "attribute vec4 p; void main() { gl_Position = p; }"

foreign import javascript unsafe
  "$1.getAttribLocation($2,$3)"
  getAttribLocation :: WebGLRenderingContext -> WebGLProgram -> Text -> IO Int

foreign import javascript unsafe
  "$1.createBuffer()"
  createBuffer :: WebGLRenderingContext -> IO WebGLBuffer

-- a PunctualWebGLContext represents a valid WebGL rendering context together
-- with handles/locations for the most recently compiled/linked shader programs
data PunctualWebGLContext = PunctualWebGLContext {
  renderingContext :: WebGLRenderingContext,
  vShader :: WebGLShader,
  fShader :: WebGLShader,
  shaderProgram :: WebGLProgram,
  drawingBuffer :: WebGLBuffer,
  tLocation :: WebGLUniformLocation,
  resLocation :: WebGLUniformLocation
  }

-- a PunctualWebGl might have a a PunctualWebGLContext (eg. if there is indeed
-- a canvas on which everything can be run) but regardless it keeps track of the
-- most recent, calculated source code of shader programs as well as the previously
-- evaluated expressions (in order to be able to generate suitable crossfades in
-- a subsequent evaluation).
data PunctualWebGL = PunctualWebGL {
  context :: Maybe PunctualWebGLContext,
  vShaderSrc :: Text,
  fShaderSrc :: Text,
  prevExpressions :: [Expression]
  }

emptyPunctualWebGL :: PunctualWebGL
emptyPunctualWebGL = PunctualWebGL {
  context = Nothing,
  vShaderSrc = defaultVertexShader,
  fShaderSrc = defaultFragmentShader,
  prevExpressions = []
  }

updateRenderingContext :: PunctualWebGL -> Maybe HTMLCanvasElement -> IO PunctualWebGL
updateRenderingContext s Nothing = return $ s { context = Nothing }
updateRenderingContext s (Just canvas) = do
  glCtx <- getWebGLRenderingContext canvas
  v <- makeVertexShader glCtx (vShaderSrc s)
  b <- createBuffer glCtx
  bindBufferArray glCtx b
  bufferDataArrayStatic glCtx
  f <- makeFragmentShader glCtx (fShaderSrc s)
  program <- makeProgram glCtx v f
  p <- getAttribLocation glCtx program "p"
  bindBufferArray glCtx b
  vertexAttribPointer glCtx p
  enableVertexAttribArray glCtx p
  t <- getUniformLocation glCtx program "t"
  res <- getUniformLocation glCtx program "res"
  let newContext = PunctualWebGLContext {
    renderingContext = glCtx,
    vShader = v,
    fShader = f,
    drawingBuffer = b,
    shaderProgram = program,
    tLocation = t,
    resLocation  = res
    }
  return $ s { context = Just newContext }

updateFragmentShader :: PunctualWebGL -> Text -> IO PunctualWebGL
updateFragmentShader st src | isNothing (context st) = return $ st { fShaderSrc = src }
updateFragmentShader st src | otherwise = do
  let oldCtx = fromJust $ context st
  let glCtx = renderingContext oldCtx
  f <- makeFragmentShader glCtx src
  program <- makeProgram glCtx (vShader oldCtx) f
  p <- getAttribLocation glCtx program "p"
  bindBufferArray glCtx (drawingBuffer oldCtx)
  vertexAttribPointer glCtx p
  enableVertexAttribArray glCtx p
  t <- getUniformLocation glCtx program "t"
  res <- getUniformLocation glCtx program "res"
  let newContext = oldCtx {
    fShader = f,
    shaderProgram = program,
    tLocation = t,
    resLocation = res
  }
  return $ st { context = Just newContext, fShaderSrc = src }

foreign import javascript unsafe
  "$1.getUniformLocation($2,$3)"
  getUniformLocation :: WebGLRenderingContext -> WebGLProgram -> Text -> IO WebGLUniformLocation

foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

evaluatePunctualWebGL :: PunctualWebGL -> (UTCTime,Double) -> Evaluation -> IO PunctualWebGL
evaluatePunctualWebGL st tempo e = do
  let shaderSrc = fragmentShader (prevExpressions st) tempo e
  -- putStrLn $ "new shader source: " ++ show shaderSrc
  st' <- updateFragmentShader st shaderSrc
  return $ st' { prevExpressions = fst e }

foreign import javascript unsafe
  "$1.bindBuffer($1.ARRAY_BUFFER,$2);"
  bindBufferArray :: WebGLRenderingContext -> WebGLBuffer -> IO ()

drawFrame :: Double -> PunctualWebGL -> IO ()
drawFrame t st | isNothing (context st) = return ()
drawFrame t st | otherwise = do
  let ctx = fromJust (context st)
  let glCtx = renderingContext ctx
  useProgram glCtx $ shaderProgram ctx
  defaultBlendFunc glCtx
--  clearColor glCtx 0.0 0.0 0.0 1.0 -- probably should comment this out
--  clearColorBuffer glCtx -- probably should comment this out
  uniform1f glCtx (tLocation ctx) t
  uniform2f glCtx (resLocation ctx) 1920 1080
  drawArraysTriangleStrip glCtx 0 4

foreign import javascript unsafe
  "$1.enable($1.BLEND); $1.blendFunc($1.ONE, $1.ONE_MINUS_SRC_ALPHA);"
  defaultBlendFunc :: WebGLRenderingContext -> IO ()

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
