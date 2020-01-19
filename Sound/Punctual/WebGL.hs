{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
  newPunctualWebGL,
  evaluatePunctualWebGL,
  drawPunctualWebGL,
  displayPunctualWebGL)
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
import qualified Data.Text.IO as T
import TextShow
import Data.Map as Map
import qualified Data.IntMap as IntMap

import Sound.Punctual.AudioTime
import Sound.Punctual.Action hiding ((<>))
import Sound.Punctual.Program
import Sound.Punctual.FragmentShader
import Sound.Punctual.GL
import Sound.Punctual.AsyncProgram

data PunctualWebGL = PunctualWebGL {
  triangleStrip :: WebGLBuffer,
  textures :: Map Text WebGLTexture,
  fb0 :: (WebGLFramebuffer,WebGLTexture),
  fb1 :: (WebGLFramebuffer,WebGLTexture),
  pingPong :: Bool,
  postProgram :: AsyncProgram,
  mainPrograms :: IntMap.IntMap AsyncProgram,
  prevPrograms :: IntMap.IntMap Program, -- note: these are Punctual programs, above two lines are GL AsyncPrograms so quite different...
  firstZone :: Int
  }

-- given a list of requested texture sources (images) and the previous map of requested texture sources
-- request any textures we don't already have AND delete any textures we no longer need

updateTextures :: [Text] -> Map Text WebGLTexture -> GL (Map Text WebGLTexture)
updateTextures xs prevTextures = do
  let xs' = fromList $ zip xs xs
  newTextures <- mapM loadTexture $ difference xs' prevTextures
  let continuingTextures = intersection prevTextures xs'
  mapM_ deleteTexture $ difference prevTextures xs'
  return $ union newTextures continuingTextures

loadTexture :: Text -> GL WebGLTexture
loadTexture t = do
  ctx <- gl
  liftIO $ _loadTexture ctx t

foreign import javascript safe
  "$r = $1.createTexture();\
  \var image = new Image();\
  \image.crossOrigin = \"Anonymous\";\
  \image.onload = function() {\
     \$1.bindTexture($1.TEXTURE_2D, $r);\
     \$1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, image);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
     \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR);\
     \};\
   \image.src = $2;"
   _loadTexture :: WebGLRenderingContext -> Text -> IO WebGLTexture

newPunctualWebGL :: GLContext -> IO PunctualWebGL
newPunctualWebGL ctx = runGL ctx $ do
  glCtx <- gl
  defaultBlendFunc
  unpackFlipY
  -- create a buffer representing default triangle strip used by main and "post" programs
  ts <- createBuffer
  bindBufferArray ts
  liftIO $ bufferDataArrayStatic glCtx
  -- create two framebuffers to ping-pong between as backbuffer/feedback
  frameBuffer0 <- makeFrameBufferTexture 1920 1080
  frameBuffer1 <- makeFrameBufferTexture 1920 1080
  -- asynchronously compile/link the "post" program (which transfers imagery from framebuffer to display)
  pp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader postFragmentShaderSrc
  return $ PunctualWebGL {
    triangleStrip = ts,
    textures = Map.empty,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1,
    pingPong = False,
    postProgram = pp,
    mainPrograms = IntMap.empty,
    prevPrograms = IntMap.empty,
    firstZone = 0
  }

foreign import javascript unsafe
  "$1.bufferData($1.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),$1.STATIC_DRAW);"
  bufferDataArrayStatic :: WebGLRenderingContext -> IO ()

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


evaluatePunctualWebGL :: GLContext -> (AudioTime,Double) -> Int -> Program -> PunctualWebGL -> IO PunctualWebGL
evaluatePunctualWebGL ctx tempo z p st = runGL ctx $ do
  let newPrograms = IntMap.insert z p $ prevPrograms st
  let allTextures = Map.keys $ Map.unions $ fmap textureMap newPrograms
  newTextures <- updateTextures allTextures (textures st)
  let prevP = IntMap.findWithDefault emptyProgram z (prevPrograms st)
  let newFragmentShader = fragmentShader tempo prevP p
  let prevAsync = IntMap.findWithDefault emptyAsyncProgram z (mainPrograms st)
  newAsync <- updateAsyncProgram prevAsync defaultVertexShader newFragmentShader
  return $ st {
    prevPrograms = newPrograms,
    mainPrograms = IntMap.insert z newAsync (mainPrograms st),
    textures = newTextures,
    firstZone = head $ IntMap.keys newPrograms -- recalculate which zone is first in drawing order so that defaultAlpha can be correct
    }


drawPunctualWebGL :: GLContext -> (AudioTime,Double,Double,Double) -> Int -> PunctualWebGL -> IO PunctualWebGL
drawPunctualWebGL ctx (t,lo,mid,hi) z st = runGL ctx $ do
  let mainUniforms = ["t","res","tex0","tex1","tex2","tex3","tex4","tex5","tex6","tex7","lo","mid","hi","_defaultAlpha"]
  let mainAttribs = ["p"]
  let prevAsync = IntMap.findWithDefault emptyAsyncProgram z $ mainPrograms st
  (newProgramReady,asyncProgram) <- useAsyncProgram prevAsync mainUniforms mainAttribs
  when newProgramReady $ do
    let program = fromJust $ activeProgram asyncProgram
    p <- getAttribLocation program "p"
    bindBufferArray $ triangleStrip st
    vertexAttribPointer p
    enableVertexAttribArray p
    uniform2fAsync asyncProgram "res" 1920 1080
    -- bind textures to uniforms representing textures in the program
    let uMap = uniformsMap asyncProgram
    let prevProgram = IntMap.findWithDefault emptyProgram z $ prevPrograms st
    let texs = textureMap prevProgram -- Map Text Int
    sequence_ $ mapWithKey (\k a -> bindTex a (textures st ! k) (uMap ! ("tex" <> showt a))) texs
  when (isJust $ activeProgram asyncProgram) $ do
    --  clearColor 0.0 0.0 0.0 1.0 -- probably should comment this back in?
    --  clearColorBuffer -- probably should comment this back in?
    uniform1fAsync asyncProgram "t" (realToFrac t)
    uniform1fAsync asyncProgram "lo" lo
    uniform1fAsync asyncProgram "mid" mid
    uniform1fAsync asyncProgram "hi" hi
    let defaultAlpha = if z == firstZone st then 1.0 else 0.0
    uniform1fAsync asyncProgram "_defaultAlpha" defaultAlpha
    pingPongFrameBuffers st
    viewport 0 0 1920 1080
    drawArraysTriangleStrip 0 4
  return $ st { mainPrograms = IntMap.insert z asyncProgram (mainPrograms st) }


pingPongFrameBuffers :: PunctualWebGL -> GL ()
pingPongFrameBuffers st = do
  let p = pingPong st
  let fb = if p then (fst $ fb0 st) else (fst $ fb1 st)
  let t = if p then (snd $ fb1 st) else (snd $ fb0 st)
  activeTexture 0
  bindTexture2D t
  bindFramebuffer fb


displayPunctualWebGL :: GLContext -> PunctualWebGL -> IO PunctualWebGL
displayPunctualWebGL ctx st = runGL ctx $ do
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
  when (isJust $ activeProgram asyncProgram) $ do
    let tPost = if (pingPong st) then (snd $ fb0 st) else (snd $ fb1 st)
    let loc = uniformsMap asyncProgram ! "tex"
    bindTex 0 tPost loc
    bindFramebufferNull
    viewport 0 0 1920 1080
    drawArraysTriangleStrip 0 4
  return $ st { postProgram = asyncProgram, pingPong = not (pingPong st) }


bindTex :: Int -> WebGLTexture -> WebGLUniformLocation -> GL ()
bindTex n t loc = do
  activeTexture n
  bindTexture2D t
  uniform1i loc n
