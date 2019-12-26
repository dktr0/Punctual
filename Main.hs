{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module Main where

import Control.Monad.Trans
import Reflex.Dom hiding (getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Map.Strict
import Control.Monad
import GHCJS.DOM.Types (HTMLCanvasElement(..),uncheckedCastTo,JSVal)
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import GHCJS.DOM.EventM
import Data.Bool

import Sound.Punctual.Types hiding ((>>))
import Sound.Punctual.Evaluation
import Sound.Punctual.Parser
import Sound.Punctual.PunctualW
import Sound.Punctual.WebGL
import Sound.MusicW
import Sound.MusicW.AudioContext
import Sound.Punctual.MovingAverage

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Punctual"
  let attrs = fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "style.css")]
  elAttr "link" attrs $ return ()

intro :: Text
intro
  ="-- Punctual, an audiovisual live coding language\n\
   \-- documentation @ https://github.com/dktr0/Punctual.git\n\
   \--\n\
   \-- Chromium/Chrome browser required for full results\n\
   \-- Press Shift-Enter to (re)evaluate/activate code\n\
   \\n\
   \sin 60m * -10db => left;\n\
   \sin 60.05m * -10db => right;\n\
   \fx => red;\n\
   \fy * -1 => green;\n\
   \sin (fx * 60m) * sin (fy * 60.05m) * fx * fy * 10db => blue;\n"

main :: IO ()
main = do
  getGlobalAudioContext >>= addWorklets
  mainWidgetWithHead headElement bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  liftIO $ putStrLn "Punctual standalone"

  let attrs = fromList [("class","canvas"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" attrs $ return ()
  initialPunctualWebGL <- liftIO $ updateRenderingContext emptyPunctualWebGL (Just canvas)
  mv <- liftIO $ newMVar initialPunctualWebGL
  -- liftIO $ forkIO $ requestAnimationFrame mv -- moved inside punctualReflex

  elClass "div" "editorAndStatus" $ do
    (parsed,statusVisible) <- do
      let textAttrs = constDyn $ fromList [("class","editorArea"){- ,("rows","999") -}]
      code <- elClass "div" "editor" $ textArea $ def & textAreaConfig_attributes .~ textAttrs & textAreaConfig_initialValue .~ intro
      let e = _textArea_element code
      e' <- wrapDomEvent (e) (onEventName Keypress) $ do
        y <- getKeyEvent
        let f ke | (keShift ke == True) && (keCtrl ke == False) && (keKeyCode ke == 13) = 1 -- shift-Enter
                 | (keShift ke == True) && (keCtrl ke == True) && (keKeyCode ke == 19) = 2 --ctrl-shift-S
                 | otherwise = 0
        if (f y /= 0) then (preventDefault >> return (f y)) else return 0
      let evaled = tagPromptlyDyn (_textArea_value code) $ ffilter (==1) e'
      shStatus <- toggle True $ ffilter (==2) e'
      return (fmap runPunctualParser evaled,shStatus)

    dFps <- punctualReflex mv $ fmapMaybe (either (const Nothing) Just) parsed
    let errorsForConsole = fmapMaybe (either (Just . show) (const Nothing)) parsed
    performEvent_ $ fmap (liftIO . putStrLn) errorsForConsole
    let errors = fmapMaybe (either (Just . show) (Just . const "")) parsed
    status <- holdDyn "" $ fmap T.pack errors

    hideableDiv statusVisible "status" $ do
      elClass "div" "errors" $ dynText status
      elClass "div" "fps" $ do
        let fpsText = fmap ((<> " FPS") . showt . (round :: Double -> Int)) dFps
        dynText fpsText


foreign import javascript unsafe
  "new Uint8Array($1.frequencyBinCount)"
  arrayForAnalysis :: Node -> IO JSVal

foreign import javascript unsafe
  "$1.getByteFrequencyData($2);"
  getByteFrequencyData :: Node -> JSVal -> IO ()

foreign import javascript unsafe
  "var acc=0; for(var x=0;x<4;x++) { acc=acc+$1[x] }; acc=acc/(4*256); $r = acc"
  getLo :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=4;x<40;x++) { acc=acc+$1[x] }; acc=acc/(36*256); $r = acc"
  getMid :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=40;x<256;x++) { acc=acc+$1[x] }; acc=acc/(216*256); $r = acc"
  getHi :: JSVal -> IO Double


punctualReflex :: MonadWidget t m => MVar PunctualWebGL -> Event t [Expression] -> m (Dynamic t Double)
punctualReflex mv exprs = mdo
  ac <- liftAudioIO $ audioContext

  -- create audio output and analysis network
  gain <- liftAudioIO $ createGain (dbamp (-10))
  comp <- liftAudioIO $ createCompressor (-20) 3 4 0.050 0.1
  analyser <- liftAudioIO $ createAnalyser 512 0.5
  array <- liftIO $ arrayForAnalysis analyser
  dest <- liftAudioIO $ createDestination
  connectNodes gain comp
  connectNodes comp analyser
  connectNodes comp dest
  liftIO $ T.putStrLn "audio output/analysis network created and connected"

  t1system <- liftIO $ getCurrentTime
  mFps <- liftIO $ newMVar $ newAverage 60
  dFps <- pollFPS mFps
  liftIO $ forkIO $ requestAnimationFrame t1system mFps mv analyser array

  t0 <- liftAudioIO $ audioTime
  let initialPunctualW = emptyPunctualW ac gain 2 t0 -- hard coded stereo for now
  evals <- performEvent $ fmap (liftIO . evaluationNow) exprs
  -- audio
  let f pW e = liftAudioIO $ updatePunctualW pW (t0,0.5) e
  newPunctualW <- performEvent $ attachPromptlyDynWith f currentPunctualW evals
  currentPunctualW <- holdDyn initialPunctualW newPunctualW
  -- video
  performEvent_ $ fmap (liftIO . evaluatePunctualWebGL' (t0,0.5) mv) evals -- *** note: tempo hard-coded here
  return dFps

evaluatePunctualWebGL' :: (AudioTime,Double) -> MVar PunctualWebGL -> Evaluation -> IO ()
evaluatePunctualWebGL' t mv e = do
  x <- readMVar mv
  y <- evaluatePunctualWebGL x t e
  void $ swapMVar mv y

evaluationNow :: [Expression] -> IO Evaluation
evaluationNow exprs = do
  t <- liftAudioIO $ audioTime
  return (exprs,t)

requestAnimationFrame :: UTCTime -> MVar MovingAverage -> MVar PunctualWebGL -> Node -> JSVal -> IO ()
requestAnimationFrame tPrev mFps mv analyser array = do
  inAnimationFrame ContinueAsync $ redrawCanvas tPrev mFps mv analyser array
  return ()

redrawCanvas :: UTCTime -> MVar MovingAverage -> MVar PunctualWebGL -> Node -> JSVal -> Double -> IO ()
redrawCanvas tPrev mFps mv analyser array _ = do
  t1system <- getCurrentTime
  let tDiff = diffUTCTime t1system tPrev
  fps <- takeMVar mFps
  putMVar mFps $ updateAverage fps $ 1 / realToFrac tDiff
  t1 <- liftAudioIO $ audioTime
  getByteFrequencyData analyser array
  lo <- getLo array
  mid <- getMid array
  hi <- getHi array
  st <- takeMVar mv
  st' <- drawFrame (t1,lo,mid,hi) st
  putMVar mv st'
  requestAnimationFrame t1system mFps mv analyser array

pollFPS :: MonadWidget t m => MVar MovingAverage -> m (Dynamic t Double)
pollFPS mFps = do
  now <- liftIO $ getCurrentTime
  ticks <- tickLossy (0.204::NominalDiffTime) now
  newFps <- performEvent $ fmap (liftIO . const (readMVar mFps)) ticks
  let newFps' = fmap getAverage newFps
  holdDyn 0 newFps'

hideableDiv :: MonadWidget t m => Dynamic t Bool -> Text -> m a -> m a
hideableDiv isVisible cssClass childWidget = do
  let attrs = fmap (bool (fromList [("hidden","true"),("class",cssClass)]) (Data.Map.Strict.singleton "class" cssClass)) isVisible
  elDynAttr "div" attrs childWidget
