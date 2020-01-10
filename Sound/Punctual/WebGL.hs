{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
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
import Data.Map.Strict
import qualified Data.Text.IO as T

import Sound.Punctual.AudioTime
import Sound.Punctual.Action hiding ((<>))
import Sound.Punctual.Program
import Sound.Punctual.FragmentShader
import Sound.Punctual.GL
import Sound.Punctual.AsyncProgram

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
  let mp = emptyAsyncProgram
  -- mp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader defaultFragmentShader
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

foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

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

defaultVertexShader :: Text
defaultVertexShader = "attribute vec4 p; void main() { gl_Position = p; }"

postFragmentShaderSrc :: Text
postFragmentShaderSrc =
  "precision mediump float;\
  \uniform vec2 res;\
  \uniform sampler2D tex;\
  \void main(){\
  \  vec2 uv = vec2(gl_FragCoord.x/res.x,gl_FragCoord.y/res.y) ;\
  \  gl_FragColor = texture2D(tex,uv);\
  \}"


evaluatePunctualWebGL :: GLContext -> PunctualWebGL -> (AudioTime,Double) -> Program -> IO PunctualWebGL
evaluatePunctualWebGL ctx st tempo p = runGL ctx $ do
  newFragmentShader <- logTime "write fragment shader" $ (return $! fragmentShader tempo (prevProgram st) p)
  -- liftIO $ T.putStrLn newFragmentShader
  mp <- updateAsyncProgram (mainProgram st) defaultVertexShader newFragmentShader
  return $ st { prevProgram = p, mainProgram = mp }


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
  -- BEGIN experiment
  p <- getAttribLocation (fromJust $ activeProgram program) "p"
  bindBufferArray $ triangleStrip st
  vertexAttribPointer p
  enableVertexAttribArray p
  -- END experiment
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
