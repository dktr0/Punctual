{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
  newPunctualWebGL,
  setResolution,
  setBrightness,
  evaluatePunctualWebGL,
  drawPunctualWebGL,
  displayPunctualWebGL,
  arrayForAnalysis,
  getByteFrequencyData,
  getLo,getMid,getHi
  )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.Types hiding (Text)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as T
import TextShow
import Data.Map as Map
import Data.Set as Set
import Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import Sound.MusicW as MusicW hiding (createBuffer,AudioTime)

import Sound.Punctual.AudioTime
import Sound.Punctual.Program
import Sound.Punctual.FragmentShader
import Sound.Punctual.GL
import Sound.Punctual.AsyncProgram
import Sound.Punctual.Resolution

data PunctualWebGL = PunctualWebGL {
  resolution :: Resolution,
  brightness :: Double,
  triangleStrip :: WebGLBuffer,
  textures :: Map Text WebGLTexture,
  fb0 :: (WebGLFramebuffer,WebGLTexture),
  fb1 :: (WebGLFramebuffer,WebGLTexture),
  pingPong :: Bool,
  postProgram :: AsyncProgram,
  mainPrograms :: IntMap.IntMap AsyncProgram,
  prevPrograms :: IntMap.IntMap Program, -- note: these are Punctual programs, above two lines are GL AsyncPrograms so quite different...
  currPrograms :: IntMap.IntMap Program,
  textureMapsEval :: IntMap.IntMap (Map Text Int),
  textureMapsDraw :: IntMap.IntMap (Map Text Int),
  firstZone :: Int,
  -- audio analysis
  needsAudioInputAnalysis :: Bool,
  needsAudioOutputAnalysis :: Bool,
  microphoneNode :: Maybe MusicW.Node,
  audioOutputNode :: Maybe MusicW.Node,
  audioInputAnalyser :: Maybe MusicW.Node,
  audioOutputAnalyser :: Maybe MusicW.Node,
  audioInputArray :: Maybe JSVal,
  audioOutputArray :: Maybe JSVal
  }


updateAudioAnalysis :: PunctualWebGL -> IO PunctualWebGL
updateAudioAnalysis st = do
  a <- maybeActivateAudioInputAnalysis st
  b <- maybeDisactivateAudioInputAnalysis a
  c <- maybeActivateAudioOutputAnalysis b
  maybeDisactivateAudioOutputAnalysis c

maybeActivateAudioInputAnalysis :: PunctualWebGL -> IO PunctualWebGL
maybeActivateAudioInputAnalysis st = case (needsAudioInputAnalysis st && isNothing (audioInputAnalyser st) && isJust (microphoneNode st)) of
  True -> do
    T.putStrLn "Punctual: activating audio input analysis"
    x <- MusicW.liftAudioIO $ createAnalyser 128 0.5
    y <- arrayForAnalysis x
    connectNodes (fromJust $ microphoneNode st) x
    return $ st {
      audioInputAnalyser = Just x,
      audioInputArray = Just y
    }
  False -> return st

maybeDisactivateAudioInputAnalysis :: PunctualWebGL -> IO PunctualWebGL
maybeDisactivateAudioInputAnalysis st = case ((not $ needsAudioInputAnalysis st) && isJust (audioInputAnalyser st)) of
  True -> do
    T.putStrLn "Punctual: disactivating audio input analysis"
    MusicW.disconnectAll $ fromJust (audioInputAnalyser st)
    return $ st {
      audioInputAnalyser = Nothing,
      audioInputArray = Nothing
    }
  False -> return st

maybeActivateAudioOutputAnalysis :: PunctualWebGL -> IO PunctualWebGL
maybeActivateAudioOutputAnalysis st = case (needsAudioOutputAnalysis st && isNothing (audioOutputAnalyser st) && isJust (audioOutputNode st)) of
  True -> do
    T.putStrLn "Punctual: activating audio output analysis"
    x <- MusicW.liftAudioIO $ createAnalyser 128 0.5
    y <- arrayForAnalysis x
    connectNodes (fromJust $ audioOutputNode st) x
    return $ st {
      audioOutputAnalyser = Just x,
      audioOutputArray = Just y
    }
  False -> return st

maybeDisactivateAudioOutputAnalysis :: PunctualWebGL -> IO PunctualWebGL
maybeDisactivateAudioOutputAnalysis st = case ((not $ needsAudioOutputAnalysis st) && isJust (audioOutputAnalyser st)) of
  True -> do
    T.putStrLn "Punctual: disactivating audio output analysis"
    MusicW.disconnectAll $ fromJust (audioOutputAnalyser st)
    return $ st {
      audioOutputAnalyser = Nothing,
      audioOutputArray = Nothing
    }
  False -> return st

getAudioInputAnalysis :: PunctualWebGL -> IO (Double,Double,Double)
getAudioInputAnalysis st = case (audioInputAnalyser st) of
  Just n -> do
    let a = fromJust $ audioInputArray st
    getByteFrequencyData n a
    x <- getLo a
    y <- getMid a
    z <- getHi a
    return (x,y,z)
  Nothing -> return (0,0,0)

getAudioOutputAnalysis :: PunctualWebGL -> IO (Double,Double,Double)
getAudioOutputAnalysis st = case (audioOutputAnalyser st) of
  Just n -> do
    let a = fromJust $ audioOutputArray st
    getByteFrequencyData n a
    x <- getLo a
    y <- getMid a
    z <- getHi a
    return (x,y,z)
  Nothing -> return (0,0,0)


-- given a list of requested texture sources (images) and the previous map of requested texture sources
-- request any textures we don't already have
-- Note: deleting of unused textures disactivated, ie. to cache them against imminent reuse
-- Later we should rework so that unused textures are *eventually* "garbage-collected".

updateTextures :: Set Text -> Map Text WebGLTexture -> GL (Map Text WebGLTexture)
updateTextures texSet prevTextures = do
  let x = Map.fromSet id texSet
  newTextures <- mapM loadTexture $ Map.difference x prevTextures
  return $ Map.union newTextures prevTextures

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

newPunctualWebGL :: Maybe MusicW.Node -> Maybe MusicW.Node -> Resolution -> Double -> GLContext -> IO PunctualWebGL
newPunctualWebGL mic out res _brightness ctx = runGL ctx $ do
  glCtx <- gl
  defaultBlendFunc
  unpackFlipY
  -- create a buffer representing default triangle strip used by main and "post" programs
  ts <- createBuffer
  bindBufferArray ts
  liftIO $ bufferDataArrayStatic glCtx
  -- create two framebuffers to ping-pong between as backbuffer/feedback
  frameBuffer0 <- makeFrameBufferTexture res
  frameBuffer1 <- makeFrameBufferTexture res
  -- asynchronously compile/link the "post" program (which transfers imagery from framebuffer to display)
  pp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader postFragmentShaderSrc
  return $ PunctualWebGL {
    resolution = res,
    brightness = _brightness,
    triangleStrip = ts,
    textures = Map.empty,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1,
    pingPong = False,
    postProgram = pp,
    mainPrograms = IntMap.empty,
    prevPrograms = IntMap.empty,
    currPrograms = IntMap.empty,
    textureMapsEval = IntMap.empty,
    textureMapsDraw = IntMap.empty,
    firstZone = 0,
    needsAudioInputAnalysis = False,
    needsAudioOutputAnalysis = False,
    microphoneNode = mic,
    audioOutputNode = out,
    audioInputAnalyser = Nothing,
    audioOutputAnalyser = Nothing,
    audioInputArray = Nothing,
    audioOutputArray = Nothing
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

makeFrameBufferTexture :: Resolution -> GL (WebGLFramebuffer,WebGLTexture)
makeFrameBufferTexture r = do
  fb <- createFramebuffer
  t <- createTexture
  let (w,h) = pixels r
  configureFrameBufferTexture fb t w h
  return (fb,t)

defaultVertexShader :: Text
defaultVertexShader = "attribute vec4 p; void main() { gl_Position = p; }"

postFragmentShaderSrc :: Text
postFragmentShaderSrc =
  "precision mediump float;\
  \uniform vec2 res;\
  \uniform sampler2D tex;\
  \uniform float brightness;\
  \void main(){\
  \  vec2 uv = vec2(gl_FragCoord.x/res.x,gl_FragCoord.y/res.y);\
  \  vec4 t = texture2D(tex,uv);\
  \  gl_FragColor = vec4(t.xyz*brightness,t.w);\
  \}"

setResolution :: GLContext -> Resolution -> PunctualWebGL -> IO PunctualWebGL
setResolution ctx r st = if r == resolution st then return st else runGL ctx $ do
  frameBuffer0 <- makeFrameBufferTexture r
  frameBuffer1 <- makeFrameBufferTexture r
  return $ st {
    resolution = r,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1
  }

setBrightness :: Double -> PunctualWebGL -> IO PunctualWebGL
setBrightness _brightness st = return $ st { brightness = _brightness }

evaluatePunctualWebGL :: GLContext -> (AudioTime,Double) -> Int -> Program -> PunctualWebGL -> IO PunctualWebGL
evaluatePunctualWebGL ctx tempo z p st = runGL ctx $ do
  let newCurrPrograms = IntMap.insert z p $ currPrograms st
  let prevProgram = IntMap.lookup z $ currPrograms st
  let newPrevPrograms = maybe (prevPrograms st) (\x -> IntMap.insert z x $ prevPrograms st) $ prevProgram
  let prevTexSet = Foldable.fold $ fmap textureSet newPrevPrograms
  let newTexSet = Foldable.fold $ fmap textureSet newCurrPrograms
  let allTextures = Set.union prevTexSet newTexSet
  newTextures <- updateTextures allTextures (textures st)
  let prevProgram' = maybe emptyProgram id prevProgram
  let progTexSet = Set.union (textureSet p) (textureSet prevProgram')
  let progTexMap = Map.fromList $ zip (Set.elems progTexSet) [0..]
  let newTextureMaps = IntMap.insert z progTexMap $ textureMapsEval st
  let newFragmentShader = fragmentShader tempo progTexMap prevProgram' p
  -- liftIO $ T.putStrLn $ newFragmentShader

  let prevAsync = IntMap.findWithDefault emptyAsyncProgram z (mainPrograms st)
  newAsync <- updateAsyncProgram prevAsync defaultVertexShader newFragmentShader
  let prevProgramsNeedAudioInputAnalysis = elem True $ fmap programNeedsAudioInputAnalysis $ IntMap.elems $ newPrevPrograms
  let prevProgramsNeedAudioOutputAnalysis = elem True $ fmap programNeedsAudioOutputAnalysis $ IntMap.elems $ newPrevPrograms

  let st' = st {
    prevPrograms = newPrevPrograms,
    currPrograms = newCurrPrograms,
    mainPrograms = IntMap.insert z newAsync (mainPrograms st),
    textures = newTextures,
    textureMapsEval = newTextureMaps,
    firstZone = head $ IntMap.keys newCurrPrograms, -- recalculate which zone is first in drawing order so that defaultAlpha can be correct
    needsAudioInputAnalysis = programNeedsAudioInputAnalysis p || prevProgramsNeedAudioInputAnalysis,
    needsAudioOutputAnalysis = programNeedsAudioOutputAnalysis p || prevProgramsNeedAudioOutputAnalysis
    }
  liftIO $ updateAudioAnalysis st'


drawPunctualWebGL :: GLContext -> AudioTime -> Int -> PunctualWebGL -> IO PunctualWebGL
drawPunctualWebGL ctx t z st = runGL ctx $ do
  let mainUniforms = ["t","res","_fb","tex0","tex1","tex2","tex3","tex4","tex5","tex6","tex7","tex8","tex9","tex10","tex11","tex12","tex13","tex14","lo","mid","hi","ilo","imid","ihi","_defaultAlpha"]
  let mainAttribs = ["p"]
  let prevAsync = IntMap.findWithDefault emptyAsyncProgram z $ mainPrograms st
  (newProgramReady,asyncProgram) <- useAsyncProgram prevAsync mainUniforms mainAttribs
  st' <- case newProgramReady of
    False -> return st
    True -> do
      let program = fromJust $ activeProgram asyncProgram
      p <- getAttribLocation program "p"
      bindBufferArray $ triangleStrip st
      vertexAttribPointer p
      enableVertexAttribArray p
      let texMap = IntMap.findWithDefault Map.empty z $ textureMapsEval st
      return $ st { textureMapsDraw = IntMap.insert z texMap $ textureMapsDraw st }
  when (isJust $ activeProgram asyncProgram) $ do
    -- bind textures to uniforms representing textures in the program
    let uMap = uniformsMap asyncProgram
    let texs = IntMap.findWithDefault (Map.empty) z $ textureMapsDraw st' -- Map Text Int
    sequence_ $ mapWithKey (\k a -> bindTex (a+1) (textures st' ! k) (uMap ! ("tex" <> showt a))) texs
    --  clearColor 0.0 0.0 0.0 1.0 -- probably should comment this back in?
    --  clearColorBuffer -- probably should comment this back in?
    uniform1fAsync asyncProgram "t" (realToFrac t)
    (lo,mid,hi) <- liftIO $ getAudioOutputAnalysis st'
    (ilo,imid,ihi) <- liftIO $ getAudioInputAnalysis st'
    uniform1fAsync asyncProgram "lo" lo
    uniform1fAsync asyncProgram "hi" hi
    uniform1fAsync asyncProgram "mid" mid
    uniform1fAsync asyncProgram "ilo" ilo
    uniform1fAsync asyncProgram "imid" imid
    uniform1fAsync asyncProgram "ihi" ihi
    let defaultAlpha = if z == firstZone st' then 1.0 else 0.0
    uniform1fAsync asyncProgram "_defaultAlpha" defaultAlpha
    pingPongFrameBuffers 0 (uniformsMap asyncProgram ! "_fb") st'
    let (w,h) = pixels (resolution st')
    uniform2fAsync asyncProgram "res" (fromIntegral w) (fromIntegral h)
    viewport 0 0 w h
    drawArraysTriangleStrip 0 4
  return $ st' { mainPrograms = IntMap.insert z asyncProgram (mainPrograms st') }


pingPongFrameBuffers :: Int -> WebGLUniformLocation -> PunctualWebGL -> GL ()
pingPongFrameBuffers i l st = do
  let p = pingPong st
  let fb = if p then (fst $ fb0 st) else (fst $ fb1 st)
  let t = if p then (snd $ fb1 st) else (snd $ fb0 st)
  bindTex i t l
  bindFramebuffer fb


displayPunctualWebGL :: GLContext -> PunctualWebGL -> IO PunctualWebGL
displayPunctualWebGL ctx st = runGL ctx $ do
  let postUniforms = ["res","tex","brightness"]
  let postAttribs = ["p"]
  (newProgramReady,asyncProgram) <- useAsyncProgram (postProgram st) postUniforms postAttribs
  when newProgramReady $ do
    let program = fromJust $ activeProgram asyncProgram
    p <- getAttribLocation program "p"
    bindBufferArray $ triangleStrip st
    vertexAttribPointer p
    enableVertexAttribArray p
  when (isJust $ activeProgram asyncProgram) $ do
    let tPost = if (pingPong st) then (snd $ fb0 st) else (snd $ fb1 st)
    let loc = uniformsMap asyncProgram ! "tex"
    bindTex 0 tPost loc
    bindFramebufferNull
    let (w,h) = pixels (resolution st)
    uniform1fAsync asyncProgram "brightness" (brightness st)
    uniform2fAsync asyncProgram "res" (fromIntegral w) (fromIntegral h)
    viewport 0 0 w h
    drawArraysTriangleStrip 0 4
  return $ st { postProgram = asyncProgram, pingPong = not (pingPong st) }


bindTex :: Int -> WebGLTexture -> WebGLUniformLocation -> GL ()
bindTex n t loc = do
  activeTexture n
  bindTexture2D t
  uniform1i loc n


foreign import javascript unsafe
  "new Uint8Array($1.frequencyBinCount)"
  arrayForAnalysis :: MusicW.Node -> IO JSVal

foreign import javascript unsafe
  "$1.getByteFrequencyData($2);"
  getByteFrequencyData :: MusicW.Node -> JSVal -> IO ()

foreign import javascript unsafe
  "var acc=0; for(var x=0;x<1;x++) { acc=acc+$1[x] }; acc=acc/(1*256); $r = acc"
  getLo :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=1;x<10;x++) { acc=acc+$1[x] }; acc=acc/(9*256); $r = acc"
  getMid :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=10;x<64;x++) { acc=acc+$1[x] }; acc=acc/(54*256); $r = acc"
  getHi :: JSVal -> IO Double
