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
import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.FragmentShader

foreign import javascript safe
  "$1.createFramebuffer()"
  createFramebuffer :: WebGLRenderingContext -> IO WebGLFramebuffer

foreign import javascript safe
  "$1.createTexture()"
  createTexture :: WebGLRenderingContext -> IO WebGLTexture

foreign import javascript safe
  "$1.bindTexture($1.TEXTURE_2D, $3);\
  \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $4, $5, 0, $1.RGBA, $1.UNSIGNED_BYTE, null);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MAG_FILTER, $1.NEAREST);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.NEAREST);\
  \$1.bindFramebuffer($1.FRAMEBUFFER, $2);\
  \$1.framebufferTexture2D($1.FRAMEBUFFER, $1.COLOR_ATTACHMENT0, $1.TEXTURE_2D, $3, 0);\
  \$1.bindTexture($1.TEXTURE_2D, null);\
  \$1.bindFramebuffer($1.FRAMEBUFFER, null);"
  configureFrameBufferTexture :: WebGLRenderingContext -> WebGLFramebuffer -> WebGLTexture -> Int -> Int -> IO ()

makeFrameBufferTexture :: WebGLRenderingContext -> Int -> Int -> IO (WebGLFramebuffer,WebGLTexture)
makeFrameBufferTexture gl w h = do
  fb <- createFramebuffer gl
  t <- createTexture gl
  configureFrameBufferTexture gl fb t w h
  return (fb,t)

foreign import javascript safe
  "$r = $1.createTexture();\
  \var image = new Image();\
  \image.onload = function() {\
     \$1.bindTexture($1.TEXTURE_2D, $r);\
     \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, image);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR);\
     \};\
   \image.src = $2;"
   loadTexture :: WebGLRenderingContext -> Text -> IO WebGLTexture

foreign import javascript unsafe
  "$1.activeTexture($1.TEXTURE0);\
  \$1.bindTexture($1.TEXTURE_2D,$2);\
  \$1.uniform1i($3,0);"
  bindTex0 :: WebGLRenderingContext -> WebGLTexture -> WebGLUniformLocation -> IO ()

foreign import javascript unsafe
  "$1.activeTexture($1.TEXTURE1);\
  \$1.bindTexture($1.TEXTURE_2D,$2);\
  \$1.uniform1i($3,1);"
  bindTex1 :: WebGLRenderingContext -> WebGLTexture -> WebGLUniformLocation -> IO ()

foreign import javascript unsafe
  "$1.activeTexture($1.TEXTURE2);\
  \$1.bindTexture($1.TEXTURE_2D,$2);\
  \$1.uniform1i($3,2);"
  bindTex2 :: WebGLRenderingContext -> WebGLTexture -> WebGLUniformLocation -> IO ()

foreign import javascript unsafe
  "$1.activeTexture($1.TEXTURE3);\
  \$1.bindTexture($1.TEXTURE_2D,$2);\
  \$1.uniform1i($3,3);"
  bindTex3 :: WebGLRenderingContext -> WebGLTexture -> WebGLUniformLocation -> IO ()

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

foreign import javascript unsafe
  "$1.deleteProgram($2);"
  deleteProgram :: WebGLRenderingContext -> WebGLProgram -> IO ()

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
  resLocation :: WebGLUniformLocation,
  tex0Location :: WebGLUniformLocation,
  tex1Location :: WebGLUniformLocation,
  tex2Location :: WebGLUniformLocation,
  tex3Location :: WebGLUniformLocation,
  tex0Texture :: WebGLTexture,
  tex1Texture :: WebGLTexture,
  tex2Texture :: WebGLTexture,
  tex3Texture :: WebGLTexture,
  loLocation :: WebGLUniformLocation,
  midLocation :: WebGLUniformLocation,
  hiLocation :: WebGLUniformLocation,
  fb0 :: (WebGLFramebuffer,WebGLTexture),
  fb1 :: (WebGLFramebuffer,WebGLTexture),
  pingPong :: Bool,
  postProgram :: WebGLProgram,
  postResLocation :: WebGLUniformLocation,
  postTexLocation :: WebGLUniformLocation
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

postFragmentShaderSrc :: Text
postFragmentShaderSrc =
  "precision mediump float;\
  \uniform vec2 res;\
  \uniform sampler2D tex;\
  \void main(){\
  \  vec2 uv = vec2(gl_FragCoord.x/res.x,gl_FragCoord.y/res.y);\
  \  gl_FragColor = texture2D(tex,uv);\
  \}"

setupPostProgram :: WebGLRenderingContext -> WebGLBuffer -> IO (WebGLProgram,WebGLUniformLocation,WebGLUniformLocation)
setupPostProgram gl b = do
  v <- makeVertexShader gl defaultVertexShader
  f <- makeFragmentShader gl postFragmentShaderSrc
  prog <- makeProgram gl v f
  resLoc <- getUniformLocation gl prog "res"
  texLoc <- getUniformLocation gl prog "tex"
  pLoc <- getAttribLocation gl prog "p"
  bindBufferArray gl b
  vertexAttribPointer gl pLoc
  enableVertexAttribArray gl pLoc
  return (prog,resLoc,texLoc)

foreign import javascript unsafe
  "$1.pixelStorei($1.UNPACK_FLIP_Y_WEBGL, true)"
  unpackFlipY :: WebGLRenderingContext -> IO ()

updateRenderingContext :: PunctualWebGL -> Maybe HTMLCanvasElement -> IO PunctualWebGL
updateRenderingContext s Nothing = return $ s { context = Nothing }
updateRenderingContext s (Just canvas) = do
  glCtx <- getWebGLRenderingContext canvas
  unpackFlipY glCtx
  -- create a buffer representing a triangle strip covering the screen
  -- (used by both the "main" and "post" programs)
  b <- createBuffer glCtx
  bindBufferArray glCtx b
  bufferDataArrayStatic glCtx
  -- setup the "post" program that transfers framebuffers to display
  (pp,ppRes,ppTex) <- setupPostProgram glCtx b
  -- setup the "main" program that writes to a framebuffer
  v <- makeVertexShader glCtx (vShaderSrc s)
  f <- makeFragmentShader glCtx (fShaderSrc s)
  program <- makeProgram glCtx v f
  p <- getAttribLocation glCtx program "p"
  bindBufferArray glCtx b
  vertexAttribPointer glCtx p
  enableVertexAttribArray glCtx p
  t <- getUniformLocation glCtx program "t"
  res <- getUniformLocation glCtx program "res"
  tex0l <- getUniformLocation glCtx program "tex0"
  tex1l <- getUniformLocation glCtx program "tex1"
  tex2l <- getUniformLocation glCtx program "tex2"
  tex3l <- getUniformLocation glCtx program "tex3"
  tex0t <- loadTexture glCtx "tex0.jpg"
  tex1t <- loadTexture glCtx "tex1.jpg"
  tex2t <- loadTexture glCtx "tex2.jpg"
  tex3t <- loadTexture glCtx "tex3.jpg"
  bindTex0 glCtx tex0t tex0l
  bindTex1 glCtx tex1t tex1l
  bindTex2 glCtx tex2t tex2l
  bindTex3 glCtx tex3t tex3l
  loL <- getUniformLocation glCtx program "lo"
  midL <- getUniformLocation glCtx program "mid"
  hiL <- getUniformLocation glCtx program "hi"
  frameBuffer0 <- makeFrameBufferTexture glCtx 1920 1080
  frameBuffer1 <- makeFrameBufferTexture glCtx 1920 1080
  let newContext = PunctualWebGLContext {
    renderingContext = glCtx,
    vShader = v,
    fShader = f,
    drawingBuffer = b,
    shaderProgram = program,
    tLocation = t,
    resLocation  = res,
    tex0Location = tex0l,
    tex1Location = tex1l,
    tex2Location = tex2l,
    tex3Location = tex3l,
    tex0Texture = tex0t,
    tex1Texture = tex1t,
    tex2Texture = tex2t,
    tex3Texture = tex3t,
    loLocation = loL,
    midLocation = midL,
    hiLocation = hiL,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1,
    pingPong = False,
    postProgram = pp,
    postResLocation = ppRes,
    postTexLocation = ppTex
    }
  flip (maybe (return ())) (context s) $ \c -> do
    deleteProgram glCtx $ shaderProgram c
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
  tex0l <- getUniformLocation glCtx program "tex0"
  tex1l <- getUniformLocation glCtx program "tex1"
  tex2l <- getUniformLocation glCtx program "tex2"
  tex3l <- getUniformLocation glCtx program "tex3"
  bindTex0 glCtx (tex0Texture oldCtx) tex0l
  bindTex1 glCtx (tex1Texture oldCtx) tex1l
  bindTex2 glCtx (tex2Texture oldCtx) tex2l
  bindTex3 glCtx (tex3Texture oldCtx) tex3l
  loL <- getUniformLocation glCtx program "lo"
  midL <- getUniformLocation glCtx program "mid"
  hiL <- getUniformLocation glCtx program "hi"
  let newContext = oldCtx {
    fShader = f,
    shaderProgram = program,
    tLocation = t,
    resLocation = res,
    tex0Location = tex0l,
    tex1Location = tex1l,
    tex2Location = tex2l,
    tex3Location = tex3l,
    loLocation = loL,
    midLocation = midL,
    hiLocation = hiL
  }
  return $ st { context = Just newContext, fShaderSrc = src }

foreign import javascript unsafe
  "$1.getUniformLocation($2,$3)"
  getUniformLocation :: WebGLRenderingContext -> WebGLProgram -> Text -> IO WebGLUniformLocation

foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

evaluatePunctualWebGL :: PunctualWebGL -> (AudioTime,Double) -> Evaluation -> IO PunctualWebGL
evaluatePunctualWebGL st tempo e = do
  let shaderSrc = fragmentShader (prevExpressions st) tempo e
  st' <- updateFragmentShader st shaderSrc
  return $ st' { prevExpressions = fst e }

foreign import javascript unsafe
  "$1.bindBuffer($1.ARRAY_BUFFER,$2);"
  bindBufferArray :: WebGLRenderingContext -> WebGLBuffer -> IO ()

foreign import javascript unsafe "$1.activeTexture($1.TEXTURE0)" activeTexture0 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE1)" activeTexture1 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE2)" activeTexture2 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE3)" activeTexture3 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE4)" activeTexture4 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE5)" activeTexture5 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE6)" activeTexture6 :: WebGLRenderingContext -> IO ()
foreign import javascript unsafe "$1.activeTexture($1.TEXTURE7)" activeTexture7 :: WebGLRenderingContext -> IO ()

foreign import javascript unsafe
  "$1.bindTexture($1.TEXTURE_2D,$2);"
  bindTexture2D :: WebGLRenderingContext -> WebGLTexture -> IO ()

foreign import javascript unsafe
  "$1.bindFramebuffer($1.FRAMEBUFFER,$2);"
  bindFramebuffer :: WebGLRenderingContext -> WebGLFramebuffer -> IO ()

foreign import javascript unsafe
  "$1.bindFramebuffer($1.FRAMEBUFFER,null);"
  bindFramebufferNull :: WebGLRenderingContext -> IO ()


pingPongFrameBuffers :: PunctualWebGLContext -> IO PunctualWebGLContext
pingPongFrameBuffers c = do
  let gl = renderingContext c
  let p = pingPong c
  let fb = if p then (fst $ fb0 c) else (fst $ fb1 c)
  let t = if p then (snd $ fb1 c) else (snd $ fb0 c)
  activeTexture0 gl
  bindTexture2D gl t
  bindFramebuffer gl fb
  return $ c { pingPong = not p }

drawFrame :: (AudioTime,Double,Double,Double) -> PunctualWebGL -> IO PunctualWebGL
drawFrame _ st | isNothing (context st) = return st
drawFrame (t,lo,mid,hi) st | otherwise = do
  let ctx = fromJust (context st)
  let glCtx = renderingContext ctx
  -- 1. run "main" program to
  useProgram glCtx $ shaderProgram ctx
  defaultBlendFunc glCtx
--  clearColor glCtx 0.0 0.0 0.0 1.0 -- probably should comment this out
--  clearColorBuffer glCtx -- probably should comment this out
  uniform1f glCtx (tLocation ctx) (realToFrac t)
  uniform2f glCtx (resLocation ctx) 1920 1080
  uniform1f glCtx (loLocation ctx) lo
  uniform1f glCtx (midLocation ctx) mid
  uniform1f glCtx (hiLocation ctx) hi
  newCtx <- pingPongFrameBuffers ctx
  drawArraysTriangleStrip glCtx 0 4
  -- 2. run "post" program to transfer from framebuffer to screen
  useProgram glCtx $ postProgram ctx
  let tPost = if (pingPong ctx) then (snd $ fb0 ctx) else (snd $ fb1 ctx)
  bindTex0 glCtx tPost $ postTexLocation ctx
  uniform2f glCtx (postResLocation ctx) 1920 1080
  bindFramebufferNull glCtx
  drawArraysTriangleStrip glCtx 0 4
  return $ st { context = Just newCtx }

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
