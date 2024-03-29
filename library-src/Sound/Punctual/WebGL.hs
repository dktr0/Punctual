{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.WebGL
  (PunctualWebGL(..),
  newPunctualWebGL,
  setResolution,
  setBrightness,
  evaluatePunctualWebGL,
  deletePunctualWebGL,
  drawPunctualWebGL,
  displayPunctualWebGL,
  arrayForAnalysis,
  getByteFrequencyData,
  getLo,getMid,getHi,
  setAudioInput,setAudioOutput
  )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.Types hiding (Text)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Data.Map as Map
import Data.Set as Set
import Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import Sound.MusicW as MusicW hiding (createBuffer,AudioTime)
import Data.Time
import Data.Tempo

import Sound.Punctual.Graph hiding (when)
import Sound.Punctual.Program
import Sound.Punctual.FragmentShader
import Sound.Punctual.GL
import Sound.Punctual.AsyncProgram
import Sound.Punctual.Resolution
import Sound.Punctual.Texture
import qualified Sound.Punctual.Webcam as Webcam

data PunctualWebGL = PunctualWebGL {
  theCanvas :: HTMLCanvasElement,
  glContext :: GLContext,
  resolution :: Resolution,
  brightness :: Double,
  triangleStrip :: WebGLBuffer,
  fftTexture :: WebGLTexture,
  ifftTexture :: WebGLTexture,
  webcam :: Webcam.Webcam,
  textures :: Map TextureRef Texture,
  fb0 :: (WebGLFramebuffer,WebGLTexture),
  fb1 :: (WebGLFramebuffer,WebGLTexture),
  pingPong :: Bool,
  postProgram :: AsyncProgram,
  mainPrograms :: IntMap.IntMap AsyncProgram,
  prevPrograms :: IntMap.IntMap Program, -- note: these are Punctual programs, above two lines are GL AsyncPrograms so quite different...
  currPrograms :: IntMap.IntMap Program,
  textureMapsEval :: IntMap.IntMap (Map TextureRef Int),
  textureMapsDraw :: IntMap.IntMap (Map TextureRef Int),
  evalTimes :: IntMap.IntMap UTCTime,
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
    x <- MusicW.liftAudioIO $ createAnalyser 1024 0.5
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
    x <- MusicW.liftAudioIO $ createAnalyser 1024 0.5
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

getAudioInputAnalysis :: GLContext -> PunctualWebGL -> IO (Double,Double,Double)
getAudioInputAnalysis glCtx st = case (audioInputAnalyser st) of
  Just n -> do
    let a = fromJust $ audioInputArray st
    getByteFrequencyData n a
    x <- getLo a
    y <- getMid a
    z <- getHi a
    let rCtx = _webGLRenderingContext glCtx
    _activeTexture2 rCtx
    _fftToTexture rCtx a (ifftTexture st)
    return (x,y,z)
  Nothing -> return (0,0,0)

getAudioOutputAnalysis :: GLContext -> PunctualWebGL -> IO (Double,Double,Double)
getAudioOutputAnalysis glCtx st = case (audioOutputAnalyser st) of
  Just n -> do
    let a = fromJust $ audioOutputArray st
    getByteFrequencyData n a
    x <- getLo a
    y <- getMid a
    z <- getHi a
    let rCtx = _webGLRenderingContext glCtx
    _activeTexture1 rCtx
    _fftToTexture rCtx a (fftTexture st)
    return (x,y,z)
  Nothing -> return (0,0,0)

foreign import javascript safe
  "$1.bindTexture($1.TEXTURE_2D, $3);\
  \$1.texImage2D($1.TEXTURE_2D, 0, $1.LUMINANCE, 512, 1, 0, $1.LUMINANCE, $1.UNSIGNED_BYTE, $2);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_S, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_WRAP_T, $1.CLAMP_TO_EDGE);\
  \$1.texParameteri($1.TEXTURE_2D, $1.TEXTURE_MIN_FILTER, $1.LINEAR);"
  _fftToTexture :: WebGLRenderingContext -> JSVal -> WebGLTexture -> IO ()


-- given a list of requested texture sources (images) and the previous map of requested texture sources
-- request any textures we don't already have
-- Note: deleting of unused textures disactivated, ie. to cache them against imminent reuse
-- Later we should rework so that unused textures are *eventually* "garbage-collected".

updateTextures :: Set TextureRef -> Map TextureRef Texture -> GL (Map TextureRef Texture)
updateTextures texSet prevTextures = do
  let x = Map.fromSet id texSet
  newTextures <- mapM loadTexture $ Map.difference x prevTextures
  return $ Map.union newTextures prevTextures


newPunctualWebGL :: Maybe MusicW.Node -> Maybe MusicW.Node -> Resolution -> Double -> HTMLCanvasElement -> GLContext -> IO PunctualWebGL
newPunctualWebGL mic out res _brightness cvs ctx = runGL ctx $ do
  glCtx <- gl
  defaultBlendFunc
  unpackFlipY
  -- create a buffer representing default triangle strip used by main and "post" programs
  ts <- createBuffer
  bindBufferArray ts
  liftIO $ bufferDataArrayStatic glCtx
  -- create textures to hold output fft (FFT), input fft (IFFT)
  oFFT <- createTexture
  iFFT <- createTexture
  wc <- Webcam.new
  -- create two framebuffers to ping-pong between as backbuffer/feedback
  frameBuffer0 <- makeFrameBufferTexture res
  frameBuffer1 <- makeFrameBufferTexture res
  -- asynchronously compile/link the "post" program (which transfers imagery from framebuffer to display)
  pp <- updateAsyncProgram emptyAsyncProgram defaultVertexShader postFragmentShaderSrc
  return $ PunctualWebGL {
    theCanvas = cvs,
    glContext = ctx,
    resolution = res,
    brightness = _brightness,
    triangleStrip = ts,
    fftTexture = oFFT,
    ifftTexture = iFFT,
    webcam = wc,
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
    evalTimes = IntMap.empty,
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

setResolution :: Resolution -> PunctualWebGL -> IO PunctualWebGL
setResolution r st = if r == resolution st then return st else runGL (glContext st) $ do
  frameBuffer0 <- makeFrameBufferTexture r
  frameBuffer1 <- makeFrameBufferTexture r
  return $ st {
    resolution = r,
    fb0 = frameBuffer0,
    fb1 = frameBuffer1
  }

setBrightness :: Double -> PunctualWebGL -> IO PunctualWebGL
setBrightness _brightness st = return $ st { brightness = _brightness }

-- TODO: rework so takes effect immmediately, instead of at next eval
setAudioInput :: Maybe MusicW.Node -> PunctualWebGL -> IO PunctualWebGL
setAudioInput x pwgl = pure $ pwgl { microphoneNode = x }

-- TODO: rework so takes effect immmediately, instead of at next eval
setAudioOutput :: Maybe MusicW.Node -> PunctualWebGL -> IO PunctualWebGL
setAudioOutput x pwgl = pure $ pwgl { audioOutputNode = x }


deletePunctualWebGL :: Int -> PunctualWebGL -> IO PunctualWebGL
deletePunctualWebGL z st = runGL (glContext st) $ do
  case IntMap.lookup z (mainPrograms st) of
    Just x -> deleteAsyncProgram x
    Nothing -> return ()
  let newCurrPrograms = IntMap.delete z $ currPrograms st
  return $ st {
    prevPrograms = IntMap.delete z $ prevPrograms st,
    currPrograms = newCurrPrograms,
    mainPrograms = IntMap.delete z $ mainPrograms st,
    textureMapsEval = IntMap.delete z $ textureMapsEval st,
    textureMapsDraw = IntMap.delete z $ textureMapsDraw st,
    evalTimes = IntMap.delete z $ evalTimes st,
    firstZone = head $ IntMap.keys newCurrPrograms
  }

evaluatePunctualWebGL :: Tempo -> Int -> Program -> PunctualWebGL -> IO (PunctualWebGL,Text)
evaluatePunctualWebGL tempo z p st = runGL (glContext st) $ do
  let newCurrPrograms = IntMap.insert z p $ currPrograms st
  let prevProgram = IntMap.lookup z $ currPrograms st
  let newPrevPrograms = maybe (prevPrograms st) (\x -> IntMap.insert z x $ prevPrograms st) $ prevProgram
  let prevTexSet = Foldable.fold $ fmap textureSet newPrevPrograms
  let newTexSet = Foldable.fold $ fmap textureSet newCurrPrograms
  let allTextures = Set.union prevTexSet newTexSet
  newTextures <- updateTextures allTextures (textures st)
  prevProgram' <- case prevProgram of
    Just x -> return x
    Nothing -> liftIO getCurrentTime >>= (return . emptyProgram)
  let progTexSet = Set.union (textureSet p) (textureSet prevProgram')
  let progTexMap = Map.fromList $ zip (Set.elems progTexSet) [0..]
  let newTextureMaps = IntMap.insert z progTexMap $ textureMapsEval st
  let newFragmentShader = fragmentShader tempo progTexMap prevProgram' p
  -- liftIO $ T.putStrLn $ newFragmentShader

  let prevAsync = IntMap.findWithDefault emptyAsyncProgram z (mainPrograms st)
  newAsync <- updateAsyncProgram prevAsync defaultVertexShader newFragmentShader
  let currAndPrevPrograms = IntMap.elems newCurrPrograms ++ IntMap.elems newPrevPrograms
  let st' = st {
    prevPrograms = newPrevPrograms,
    currPrograms = newCurrPrograms,
    mainPrograms = IntMap.insert z newAsync (mainPrograms st),
    textures = newTextures,
    textureMapsEval = newTextureMaps,
    firstZone = head $ IntMap.keys newCurrPrograms, -- recalculate which zone is first in drawing order so that defaultAlpha can be correct
    needsAudioInputAnalysis = elem True $ fmap programNeedsAudioInputAnalysis currAndPrevPrograms,
    needsAudioOutputAnalysis =elem True $ fmap programNeedsAudioOutputAnalysis currAndPrevPrograms
    }
  -- liftIO $ putStrLn $ "evaluatePunctualWebGL, textures: " ++ show newTextures
  -- liftIO $ putStrLn $ "evaluatePunctualWebGL, textureMapsEval: " ++ show newTextureMaps
  st'' <- liftIO $ updateAudioAnalysis st'
  setWebcamActive st''
  return (st'',newFragmentShader)
  
  
setWebcamActive :: PunctualWebGL -> GL ()
setWebcamActive st = do
  -- the webcam should be active if any current or previous programs in any zone require it, otherwise it should be inactive
  let shouldBeActive = (elem True $ fmap programNeedsWebcam $ IntMap.elems $ currPrograms st) || (elem True $ fmap programNeedsWebcam $ IntMap.elems $ prevPrograms st)
  Webcam.setActive (webcam st) shouldBeActive


drawPunctualWebGL :: Tempo -> UTCTime -> Int -> PunctualWebGL -> IO PunctualWebGL
drawPunctualWebGL tempo now z st = runGL (glContext st) $ do
  Webcam.updateTexture (webcam st)
  let mainUniforms = ["res","width","height","_fb","_cam","_fft","_ifft","tex0","tex1","tex2","tex3","tex4","tex5","tex6","tex7","tex8","tex9","tex10","tex11","tex12","lo","mid","hi","ilo","imid","ihi","_defaultAlpha","_cps","_time","_etime","_beat","_ebeat"]
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
      return $ st {
        textureMapsDraw = case IntMap.lookup z (textureMapsEval st) of
          Just x -> IntMap.insert z x (textureMapsDraw st)
          Nothing -> textureMapsDraw st,
        evalTimes = case IntMap.lookup z (currPrograms st) of
          Just x -> IntMap.insert z (evalTime x) (evalTimes st)
          Nothing -> evalTimes st
        }
  when (isJust $ activeProgram asyncProgram) $ do
    let program = fromJust $ activeProgram asyncProgram
    fftLoc <- getUniformLocation program "_fft"
    ifftLoc <- getUniformLocation program "_ifft"
    bindTex 1 (fftTexture st') fftLoc
    bindTex 2 (ifftTexture st') ifftLoc
    camLoc <- getUniformLocation program "_cam"
    bindTex 3 (Webcam._texture $ webcam st') camLoc    
    -- bind textures to uniforms representing textures in the program
    let uMap = uniformsMap asyncProgram
    let texs = IntMap.findWithDefault (Map.empty) z $ textureMapsDraw st' -- Map Text Int
    let bindTex' k a = do
          let textureSlot = a + 4
          let theTexture = textures st' ! k
          let uniformName = "tex" <> showt a
          let uniformLoc = uMap ! uniformName
          bindTex textureSlot (webGLTexture theTexture) uniformLoc
          updateTexture theTexture -- updateTexture comes after the preceding bindText because this ensures active texture slot has been set accurately
    sequence_ $ mapWithKey bindTex' texs
    uniform1fAsync asyncProgram "_cps" (realToFrac $ freq tempo)
    case IntMap.lookup z (evalTimes st') of
      Just eTime -> do
        uniform1fAsync asyncProgram "_time" (realToFrac $ diffUTCTime now $ origin tempo)
        uniform1fAsync asyncProgram "_beat" (realToFrac $ timeToCount tempo now)
        let eTime' = realToFrac $ diffUTCTime now eTime :: Double
        uniform1fAsync asyncProgram "_etime" eTime'
        uniform1fAsync asyncProgram "_ebeat" (eTime' * realToFrac (freq tempo))
      Nothing -> liftIO $ putStrLn "strange error: no eval time stored for current Punctual WebGL program"
    (lo,mid,hi) <- liftIO $ getAudioOutputAnalysis (glContext st) st'
    (ilo,imid,ihi) <- liftIO $ getAudioInputAnalysis (glContext st) st'
    uniform1fAsync asyncProgram "lo" lo
    uniform1fAsync asyncProgram "hi" hi
    uniform1fAsync asyncProgram "mid" mid
    uniform1fAsync asyncProgram "ilo" ilo
    uniform1fAsync asyncProgram "imid" imid
    uniform1fAsync asyncProgram "ihi" ihi
    let isFirstZone = z == firstZone st'
    let defaultAlpha = if isFirstZone then 1.0 else 0.0
    uniform1fAsync asyncProgram "_defaultAlpha" defaultAlpha
    let (w,h) = pixels (resolution st')
    uniform2fAsync asyncProgram "res" (fromIntegral w) (fromIntegral h)
    actualWidth <- liftIO $ getClientWidth (theCanvas st)
    uniform1fAsync asyncProgram "width" actualWidth
    actualHeight <- liftIO $ getClientHeight (theCanvas st)
    uniform1fAsync asyncProgram "height" actualHeight
    viewport 0 0 w h
    pingPongFrameBuffers (uniformsMap asyncProgram ! "_fb") st'
    -- clear before drawing only if it is the first zone:
    when isFirstZone $ do
      clearColor 0 0 0 0
      clearColorBuffer
    drawArraysTriangleStrip 0 4
  return $ st' { mainPrograms = IntMap.insert z asyncProgram (mainPrograms st') }


pingPongFrameBuffers :: WebGLUniformLocation -> PunctualWebGL -> GL ()
pingPongFrameBuffers l st = do
  let p = pingPong st
  let fb = if p then (fst $ fb0 st) else (fst $ fb1 st)
  let t = if p then (snd $ fb1 st) else (snd $ fb0 st)
  bindTex 0 t l
  bindFramebuffer fb


displayPunctualWebGL :: PunctualWebGL -> IO PunctualWebGL
displayPunctualWebGL st = if (IntMap.null $ currPrograms st) then (return st) else runGL (glContext st) $ do
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
    clearColor 0 0 0 0
    clearColorBuffer
    drawArraysTriangleStrip 0 4
    return ()
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
  "var acc=0; for(var x=0;x<8;x++) { acc=acc+$1[x] }; acc=acc/(8*256); $r = acc"
  getLo :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=8;x<80;x++) { acc=acc+$1[x] }; acc=acc/(72*256); $r = acc"
  getMid :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=80;x<512;x++) { acc=acc+$1[x] }; acc=acc/(432*256); $r = acc"
  getHi :: JSVal -> IO Double

foreign import javascript unsafe
  "$1.clientWidth"
  getClientWidth :: HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$1.clientHeight"
  getClientHeight :: HTMLCanvasElement -> IO Double
