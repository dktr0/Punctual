module Sound.Punctual where

import Data.Text
import Data.IntMap as IntMap
import Data.IORef

import Sound.Punctual.WebGL
import Sound.Punctual.PunctualW
import Sound.Punctual.Resolution
import Sound.Punctual.GL


data Punctual = Punctual {
  punctualWs :: IORef (IntMap PunctualW),
  punctualWebGL :: IORef PunctualWebGL,
  glCtx :: GLContext,
  tempo :: IORef Tempo,
  audioInput :: IORef MusicW.Node,
  audioOutput :: IORef MusicW.Node,
  nchnls :: IORef Int
  }
  
newPunctual :: Tempo -> MusicW.Node -> MusicW.Node -> Int -> HTMLCanvasElement -> IO Punctual
newPunctual iTempo iAudioInput iAudioOutput iNchnls cvs = do
  punctualWs' <- newIORef IntMap.empty
  punctualWebGL' <- newPunctualWebGL audioIn audioOut HD 1.0 cvs glCtx
  punctualWebGL'' <- newIORef punctualWebGL'
  glCtx' <- newGLContext cvs
  tempo' <- newIORef iTempo
  audioInput' <- newIORef iAudioInput
  audioOuput' <- newIORef iAudioOutput
  nchnls' <- newIORef iNchnls
  pure $ Punctual {
    punctualWs = punctualWs',
    punctualWebGL = punctualWebGL'',
    glCtx = glCtx',
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

setNchnls :: Punctual -> Tempo -> IO ()
setNchnls p x = writeIORef (nchnls p) x


evaluate :: Punctual -> Int -> Text -> UTCTime -> IO (Either Text Text) -- left error, right shader src
evaluate p z txt evalTime = do
  parseResult <- liftIO $ try $ return $! parse evalTime txt
  parseResult' <- case parseResult of
    Right (Right prog) -> do      
      programChangedAudio p z prog evalTime
      Right <$> programChangedVideo p z prog
    Right (Left parseErr) -> pure (Left $ T.pack $ show parseErr)
    Left exception -> pure (Left $ T.pack $ show (exception :: SomeException))
  
programChangedAudio :: Punctual -> Int -> Program -> UTCTime -> IO ()
programChangedAudio p z prog evalTime = do
  pws <- readIORef (punctualWs p)
  ac <- liftAudioIO $ audioContext
  pIn <- readIORef (audioInput p)
  pOut <- readIORef (audioOutput p)
  n <- readIORef (nchnls p)
  let defPunctualW = emtpyPunctualW ac pIn pOut n evalTime
  pw <- findWithDefault defPunctualW z pws
  let pw' = setPunctualWChannels n
  pw'' <- runAudioContextIO ac $ updatePunctualW pw' tempo prog `catch` (\e -> putStrLn (show (e :: SomeException)) >> return pw')
  writeIORef (punctualWs p) $ IntMap.insert z pw'' pws
  
programChangedWebGL :: Punctual -> Int -> Program -> IO Text
programChangedWebGL p z prog = do
  pwgl <- readIORef (punctualWebGL p)
  tempo' <- readIORef (tempo p)
  (pwgl',shaderSrc) <- evaluatePunctualWebGL (glCtx p) tempo' z prog pwgl -- :: (PunctualWebGL,Text)
  writeIORef (punctualWebGL p) pwgl'
  pure shaderSrc

