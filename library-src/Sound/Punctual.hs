module Sound.Punctual 
  (Punctual,
  new,
  evaluate,
  render,
  postRender,
  clear,
  setTempo,
  setAudioInput,
  setAudioOutput,
  setNchnls,
  Sound.Punctual.setBrightness,
  Sound.Punctual.setResolution)
  where

import Data.Text as T
import Data.IntMap as IntMap
import Data.IORef
import Data.Time
import Sound.MusicW as MusicW
import Data.Tempo
import GHCJS.DOM.Types hiding (Text)
import Control.Exception hiding (evaluate)

import Sound.Punctual.WebGL
import Sound.Punctual.PunctualW
import Sound.Punctual.Resolution
import Sound.Punctual.GL
import Sound.Punctual.Program
import Sound.Punctual.Parser


data Punctual = Punctual {
  punctualWs :: IORef (IntMap PunctualW),
  punctualWebGL :: IORef PunctualWebGL,
  tempo :: IORef Tempo,
  audioInput :: IORef MusicW.Node,
  audioOutput :: IORef MusicW.Node,
  nchnls :: IORef Int
  }
  
new :: HTMLCanvasElement -> IO Punctual
new canvas = do
  iAudioInput <- liftAudioIO $ createConstantSource 0
  iAudioOutput <- liftAudioIO $ createDestination
  audioInput' <- newIORef iAudioInput
  audioOutput' <- newIORef iAudioOutput
  nchnls' <- newIORef $ numberOfOutputs iAudioOutput
  punctualWs' <- newIORef IntMap.empty
  glCtx' <- newGLContext canvas
  punctualWebGL' <- newPunctualWebGL (Just iAudioInput) (Just iAudioOutput) HD 1.0 canvas glCtx'
  punctualWebGL'' <- newIORef punctualWebGL'
  tNow <- getCurrentTime
  tempo' <- newIORef $ Tempo { time=tNow, freq=0.5, Data.Tempo.count=0 }
  pure $ Punctual {
    punctualWs = punctualWs',
    punctualWebGL = punctualWebGL'',
    tempo = tempo',
    audioInput = audioInput',
    audioOutput = audioOutput',
    nchnls = nchnls'
  }
  
setTempo :: Punctual -> Tempo -> IO ()
setTempo p x = writeIORef (tempo p) x

setAudioInput :: Punctual -> MusicW.Node -> IO ()
setAudioInput p x = writeIORef (audioInput p) x

setAudioOutput :: Punctual -> MusicW.Node -> IO ()
setAudioOutput p x = writeIORef (audioOutput p) x

setNchnls :: Punctual -> Int -> IO ()
setNchnls p x = writeIORef (nchnls p) x

setResolution :: Punctual -> Resolution -> IO ()
setResolution p r = do
  pwgl <- readIORef (punctualWebGL p)
  pwgl' <- Sound.Punctual.WebGL.setResolution r pwgl
  writeIORef (punctualWebGL p) pwgl'

setBrightness :: Punctual -> Double -> IO ()
setBrightness p b = do
  pwgl <- readIORef (punctualWebGL p)
  pwgl' <- Sound.Punctual.WebGL.setBrightness b pwgl
  writeIORef (punctualWebGL p) pwgl'


evaluate :: Punctual -> Int -> Text -> UTCTime -> IO (Either Text Text) -- left=error, right=shader src
evaluate p z txt eTime = do
  parseResult <- try $ return $! parse eTime txt
  case parseResult of
    Right (Right prog) -> do      
      evaluateAudio p z prog eTime
      Right <$> evaluateVideo p z prog
    Right (Left parseErr) -> pure (Left $ T.pack $ show parseErr)
    Left exception -> pure (Left $ T.pack $ show (exception :: SomeException))
  
evaluateAudio :: Punctual -> Int -> Program -> UTCTime -> IO ()
evaluateAudio p z prog eTime = do
  pws <- readIORef (punctualWs p)
  ac <- liftAudioIO $ audioContext
  pIn <- readIORef (audioInput p)
  pOut <- readIORef (audioOutput p)
  n <- readIORef (nchnls p)
  t <- readIORef (tempo p)
  let defPunctualW = emptyPunctualW ac pIn pOut n eTime
  let pw = findWithDefault defPunctualW z pws
  let pw' = setPunctualWChannels n pw
  pw'' <- do
    runAudioContextIO ac $ updatePunctualW pw' t prog
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return pw')
  writeIORef (punctualWs p) $ IntMap.insert z pw'' pws
  
evaluateVideo :: Punctual -> Int -> Program -> IO Text
evaluateVideo p z prog = do
  pwgl <- readIORef (punctualWebGL p)
  tempo' <- readIORef (tempo p)
  (pwgl',shaderSrc) <- evaluatePunctualWebGL tempo' z prog pwgl -- :: (PunctualWebGL,Text)
  writeIORef (punctualWebGL p) pwgl'
  pure shaderSrc

render :: Punctual -> Bool -> Int -> UTCTime -> IO ()
render p canDraw z tNow = do
  tempo' <- readIORef (tempo p)
  pwgl <- readIORef (punctualWebGL p)
  case canDraw of 
    True -> drawPunctualWebGL tempo' tNow z pwgl >>= writeIORef (punctualWebGL p)
    False -> pure ()
    
postRender :: Punctual -> Bool -> IO ()
postRender p canDraw = 
  case canDraw of
    True -> do
      pwgl <- readIORef (punctualWebGL p)
      pwgl' <- displayPunctualWebGL pwgl
      writeIORef (punctualWebGL p) pwgl'
    False -> pure ()

clear :: Punctual -> Int -> IO ()
clear p z = do
  -- clear Punctual audio (in a given zone)
  pws <- readIORef (punctualWs p)
  case (IntMap.lookup z pws) of
    Just x -> do
      liftAudioIO $ deletePunctualW x
      writeIORef (punctualWs p) $ IntMap.delete z pws
    Nothing -> pure ()
  -- clear Punctual video (in a given zone)
  pwgl <- readIORef (punctualWebGL p)
  pwgl' <- deletePunctualWebGL z pwgl
  writeIORef (punctualWebGL p) pwgl'

