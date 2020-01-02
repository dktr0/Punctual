{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL,
  newPunctualWebGL,
  evaluatePunctualWebGL,
  drawFrame)
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import GHCJS.Types
import GHCJS.DOM.Types hiding (Text)
import GHCJS.Marshal.Pure
import Data.Time
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
<<<<<<< HEAD
import Sound.MusicW.AudioContext (AudioTime)
import Data.IntMap.Strict
=======
import qualified Data.Text.IO as T
import Data.Map.Strict
>>>>>>> async

import Sound.Punctual.Types hiding ((<>))
import Sound.Punctual.Evaluation
import Sound.Punctual.FragmentShader
import Sound.Punctual.GL
import Sound.Punctual.AsyncProgram

<<<<<<< HEAD
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
  resLocation :: WebGLUniformLocation
  }

-- a PunctualWebGl might have a a PunctualWebGLContext (eg. if there is indeed
-- a canvas on which everything can be run) but regardless it keeps track of the
-- most recent, calculated source code of shader programs as well as the previously
-- evaluated Program (in order to be able to generate suitable crossfades in
-- a subsequent evaluation).
data PunctualWebGL = PunctualWebGL {
  context :: Maybe PunctualWebGLContext,
  vShaderSrc :: Text,
  fShaderSrc :: Text,
  prevProgram :: Program
  }

emptyPunctualWebGL :: PunctualWebGL
emptyPunctualWebGL = PunctualWebGL {
  context = Nothing,
  vShaderSrc = defaultVertexShader,
  fShaderSrc = defaultFragmentShader,
  prevProgram = empty
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

=======

data PunctualWebGL = PunctualWebGL {
  triangleStrip :: WebGLBuffer,
  tex0Texture :: WebGLTexture,
  tex1Texture :: WebGLTexture,
  tex2Texture :: WebGLTexture,
  tex3Texture :: WebGLTexture,
  fb0 :: (WebGLFramebuffer,WebGLTexture),
  fb1 :: (WebGLFramebuffer,WebGLTexture),
  pingPong :: Bool,
  mainProgram :: AsyncProgram,
  postProgram :: AsyncProgram,
  prevProgram :: Program -- note: this is a Punctual program, above two lines are GL AsyncPrograms so quite different...
  }

newPunctualWebGL :: GLContext -> IO PunctualWebGL
newPunctualWebGL ctx = runGL ctx $ do
  glCtx <- gl
  defaultBlendFunc
  unpackFlipY
  -- create a buffer representing default triangle strip used by main and "post" programs
  ts <- createBuffer
  bindBufferArray ts
  liftIO $ bufferDataArrayStatic glCtx
  -- load textures
  tex0t <- loadTexture "tex0.jpg"
  tex1t <- loadTexture "tex1.jpg"
  tex2t <- loadTexture "tex2.jpg"
  tex3t <- loadTexture "tex3.jpg"
  -- create two framebuffers to ping-pong between as backbuffer/feedback
  frameBuffer0 <- makeFrameBufferTexture 1920 1080
  frameBuffer1 <- makeFrameBufferTexture 1920 1080
  -- asynchronously compile/link the main and "post" programs
  mp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader defaultFragmentShader
  pp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader postFragmentShaderSrc
  return $ PunctualWebGL {
    triangleStrip = ts,
    tex0Texture = tex0t,
    tex1Texture = tex1t,
    tex2Texture = tex2t,
    tex3Texture = tex3t,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1,
    pingPong = False,
    mainProgram = mp,
    postProgram = pp,
    prevProgram = emptyProgram
  }

>>>>>>> async
foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

<<<<<<< HEAD
evaluatePunctualWebGL :: PunctualWebGL -> (AudioTime,Double) -> Evaluation -> IO PunctualWebGL
evaluatePunctualWebGL st tempo e = do
  let shaderSrc = fragmentShader (prevProgram st) tempo e
  st' <- updateFragmentShader st shaderSrc
  return $ st' { prevProgram = fst e }

foreign import javascript unsafe
  "$1.bindBuffer($1.ARRAY_BUFFER,$2);"
  bindBufferArray :: WebGLRenderingContext -> WebGLBuffer -> IO ()

drawFrame :: AudioTime -> PunctualWebGL -> IO ()
drawFrame t st | isNothing (context st) = return ()
drawFrame t st | otherwise = do
  let ctx = fromJust (context st)
  let glCtx = renderingContext ctx
  useProgram glCtx $ shaderProgram ctx
  defaultBlendFunc glCtx
--  clearColor glCtx 0.0 0.0 0.0 1.0 -- probably should comment this out
--  clearColorBuffer glCtx -- probably should comment this out
  uniform1f glCtx (tLocation ctx) (realToFrac t)
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
=======
loadTexture :: Text -> GL WebGLTexture
loadTexture t = do
  ctx <- gl
  liftIO $ _loadTexture ctx t

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
   _loadTexture :: WebGLRenderingContext -> Text -> IO WebGLTexture

configureFrameBufferTexture :: WebGLFramebuffer -> WebGLTexture -> Int -> Int -> GL ()
configureFrameBufferTexture fb t w h = do
  ctx <- gl
  liftIO $ _configureFrameBufferTexture ctx fb t w h

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
  _configureFrameBufferTexture :: WebGLRenderingContext -> WebGLFramebuffer -> WebGLTexture -> Int -> Int -> IO ()

makeFrameBufferTexture :: Int -> Int -> GL (WebGLFramebuffer,WebGLTexture)
makeFrameBufferTexture w h = do
  fb <- createFramebuffer
  t <- createTexture
  configureFrameBufferTexture fb t w h
  return (fb,t)
>>>>>>> async

defaultVertexShader :: Text
defaultVertexShader = "attribute vec4 p; void main() { gl_Position = p; }"

postFragmentShaderSrc :: Text
postFragmentShaderSrc =
  "precision mediump float;\
  \uniform vec2 res;\
  \uniform sampler2D tex;\
  \void main(){\
  \  vec2 uv = vec2(gl_FragCoord.x/res.x,gl_FragCoord.y/res.y);\
  \  gl_FragColor = texture2D(tex,uv);\
  \}"


evaluatePunctualWebGL :: GLContext -> PunctualWebGL -> (AudioTime,Double) -> Evaluation -> IO PunctualWebGL
evaluatePunctualWebGL ctx st tempo e = runGL ctx $ do
  let prevExpressions = expressions $ prevProgram st
  newFragmentShader <- logTime "write fragment shader" $ (return $! fragmentShader prevExpressions tempo e)
  mp <- updateAsyncProgram (mainProgram st) defaultVertexShader newFragmentShader
  return $ st { prevProgram = fst e, mainProgram = mp }


drawFrame :: GLContext -> (AudioTime,Double,Double,Double) -> PunctualWebGL -> IO PunctualWebGL
drawFrame ctx (t,lo,mid,hi) st = runGL ctx $ do
  st' <- useMainProgram st
  drawMainProgram (t,lo,mid,hi) st'
  st'' <- usePostProgram st'
  drawPostProgram st''
  return $ st'' { pingPong = not (pingPong st'') }


useMainProgram :: PunctualWebGL -> GL PunctualWebGL
useMainProgram st = do
  let mainUniforms = ["t","res","tex0","tex1","tex2","tex3","lo","mid","hi"]
  let mainAttribs = ["p"]
  (newProgramReady,asyncProgram) <- useAsyncProgram (mainProgram st) mainUniforms mainAttribs
  when newProgramReady $ do
    let program = fromJust $ activeProgram asyncProgram
    p <- getAttribLocation program "p"
    bindBufferArray $ triangleStrip st
    vertexAttribPointer p
    enableVertexAttribArray p
    uniform2fAsync asyncProgram "res" 1920 1080
    -- bind textures to uniforms representing textures in the program
    let uMap = uniformsMap asyncProgram
    bindTex 0 (tex0Texture st) (uMap ! "tex0")
    bindTex 1 (tex1Texture st) (uMap ! "tex1")
    bindTex 2 (tex2Texture st) (uMap ! "tex2")
    bindTex 3 (tex3Texture st) (uMap ! "tex3")
  return $ st { mainProgram = asyncProgram }

bindTex :: Int -> WebGLTexture -> WebGLUniformLocation -> GL ()
bindTex n t loc = do
  activeTexture n
  bindTexture2D t
  uniform1i loc n

drawMainProgram :: (AudioTime,Double,Double,Double) -> PunctualWebGL -> GL ()
drawMainProgram (t,lo,mid,hi) st = when (isJust $ activeProgram $ mainProgram st) $ do
  --  clearColor 0.0 0.0 0.0 1.0 -- probably should comment this back in?
  --  clearColorBuffer -- probably should comment this back in?
  let program = mainProgram st
  uniform1fAsync program "t" (realToFrac t)
  uniform1fAsync program "lo" lo
  uniform1fAsync program "mid" mid
  uniform1fAsync program "hi" hi
  pingPongFrameBuffers st
  drawArraysTriangleStrip 0 4

pingPongFrameBuffers :: PunctualWebGL -> GL ()
pingPongFrameBuffers st = do
  let p = pingPong st
  let fb = if p then (fst $ fb0 st) else (fst $ fb1 st)
  let t = if p then (snd $ fb1 st) else (snd $ fb0 st)
  activeTexture 0
  bindTexture2D t
  bindFramebuffer fb

usePostProgram :: PunctualWebGL -> GL PunctualWebGL
usePostProgram st = do
  let postUniforms = ["res","tex"]
  let postAttribs = ["p"]
  (newProgramReady,asyncProgram) <- useAsyncProgram (postProgram st) postUniforms postAttribs
  when newProgramReady $ do
    let program = fromJust $ activeProgram asyncProgram
    p <- getAttribLocation program "p"
    bindBufferArray $ triangleStrip st
    vertexAttribPointer p
    enableVertexAttribArray p
    uniform2fAsync asyncProgram "res" 1920 1080
  return $ st { postProgram = asyncProgram }

drawPostProgram :: PunctualWebGL -> GL ()
drawPostProgram st = when (isJust $ activeProgram $ postProgram st) $ do
  let tPost = if (pingPong st) then (snd $ fb0 st) else (snd $ fb1 st)
  let loc =  (uniformsMap $ postProgram st) ! "tex"
  bindTex 0 tPost loc
  bindFramebufferNull
  drawArraysTriangleStrip 0 4
