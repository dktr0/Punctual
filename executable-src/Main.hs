{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI, FlexibleContexts, GADTs #-}

module Main where

import System.IO
import Control.Monad.Trans
import Control.Monad.Fix
import Reflex.Dom hiding (preventDefault)
import Data.Time
import Data.Tempo
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Data.IORef
import Control.Concurrent
import Data.Map
import Control.Monad
import GHCJS.DOM.Types hiding (Text,Event) -- (HTMLCanvasElement(..),uncheckedCastTo,JSVal,WebGLRenderingContext)
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import GHCJS.DOM.EventM
import Data.Bool
import Data.Maybe
import Data.Either

import Sound.MusicW
import Sound.MusicW.AudioContext hiding (AudioTime)
import Sound.Punctual.Resolution
import Sound.Punctual
import MovingAverage

headElement :: DomBuilder t m => m ()
headElement = do
  el "title" $ text "Punctual"
  let attrs = fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "style.css")]
  elAttr "link" attrs $ return ()

intro :: Text
intro
  ="-- Punctual, an audiovisual live coding language, version 0.4.4.7\n\
   \-- Chromium/Chrome/Edge/Opera browser required\n\
   \-- Press Shift-Enter to (re)evaluate/activate code\n\
   \-- documentation @ https://github.com/dktr0/Punctual.git\n\
   \-- help/discussion @ Estuary discord server\n\
   \\n\
   \x1 << osc $ 0.11*[1,2]; y1 << osc $ 0.08/[3,4];\n\
   \x2 << osc $ 0.06/[5,6]; y2 << osc $ 0.04*[7,8];\n\
   \lines << mono $ iline [x1,y1] [x2,y2] 0.002;\n\
   \col << hsvrgb [osc 0.11,0.5 ~~ 1 $ osc 0.12, 1];\n\
   \mask << prox 0 ** 8;\n\
   \a << fit 1 $ lines * col  * mask;\n\
   \gatep 0.1 (maxp a (fb fxy * 0.98)) >> video <> 5\n"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Punctual standalone, version 0.4.4.7"
  ctx <- getGlobalAudioContextPlayback 
  putStrLn "global audio context (playback mode) acquired"
  putStrLn "loading MusicW audio worklets..."
  addWorklets ctx
  putStrLn "audio worklets loaded."
  mainWidgetWithHead headElement bodyElement


data KeyboardShortCut = Evaluate | ToggleStatus | ToggleInfo deriving (Eq,Show)

keyboardShortCuts :: IsEvent t => Word -> Bool -> Bool -> EventM e t (Maybe KeyboardShortCut)
keyboardShortCuts 13 False True = preventDefault >> return (Just Evaluate) -- shift-Enter
keyboardShortCuts 6 True True = preventDefault >> return (Just ToggleStatus) -- ctrl-shift-F
keyboardShortCuts 17 True True = preventDefault >> return (Just ToggleInfo) -- ctrl-shift-Q
keyboardShortCuts _ _ _ = return Nothing

foreign import javascript unsafe "$1['ctrlKey']"
  _getCtrlKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1['shiftKey']"
  _getShiftKey :: JSVal -> IO Bool


bodyElement :: (MonadIO m, MonadIO (Performable m), PerformEvent t m, MonadFix m, MonadHold t m, TriggerEvent t m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => m ()
bodyElement = mdo
  (e,_) <- elAttr' "div" (Data.Map.singleton "class" "standalone") $ standalone $ fmapMaybe id ksc
  let e' = HTMLDivElement $ pToJSVal $ _element_raw e
  ksc <- wrapDomEvent e' (onEventName Keypress) $ do
    ev <- event
    y <- getKeyEvent -- :: m Word
    ctrlKey <- liftIO $ _getCtrlKey (unKeyboardEvent ev)
    shiftKey <- liftIO $ _getShiftKey (unKeyboardEvent ev)
    -- liftIO $ putStrLn $ show y ++ " " ++ show ctrlKey ++ " " ++ show shiftKey
    keyboardShortCuts y ctrlKey shiftKey
  pure ()


standalone :: (MonadIO m, MonadIO (Performable m), PerformEvent t m, MonadFix m, MonadHold t m, TriggerEvent t m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => Event t KeyboardShortCut -> m ()
standalone ksc = do
  let attrs = fromList [("class","canvas"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" attrs $ return ()
  rs <- liftIO $ launchAnimationThread canvas

  elClass "div" "editorAndStatus" $ do
    (statusVisible,status) <- mdo
      let textAttrs = constDyn $ fromList [("class","editorArea")]
      code <- elClass "div" "editor" $ textArea $ def & textAreaConfig_attributes .~ textAttrs & textAreaConfig_initialValue .~ intro
      hideableEl "pre" shInfo "info" $ dynText shader
      let evaled = tagPromptlyDyn (_textArea_value code) $ ffilter (==Evaluate) ksc
      shStatus <- toggle True $ ffilter (==ToggleStatus) ksc
      shInfo <- toggle False $ ffilter (==ToggleInfo) ksc
      (status,shader) <- performEvaluate rs evaled
      return (shStatus,status)

    hideableEl "div" statusVisible "status" $ do
      fps <- pollFPS rs
      elClass "div" "errors" $ dynText status
      elClass "div" "fps" $ do
        let fpsText = fmap ((<> " FPS") . showt . (round :: Double -> Int)) fps
        dynText fpsText
  --





performEvaluate :: (PerformEvent t m, MonadIO (Performable m), MonadHold t m) => RenderState -> Event t Text -> m (Dynamic t Text, Dynamic t Text)
performEvaluate rs e = do
  x <- performEvent $ ffor e (liftIO . doEvaluate rs)
  status <- holdDyn "" $ fmap fst x
  shader <- holdDyn "" $ fmap snd x
  return (status,shader)

doEvaluate :: RenderState -> Text -> IO (Text,Text)
doEvaluate rs txt = do
  now <- getCurrentTime
  evalResult <- evaluate (punctual rs) 0 txt now
  case evalResult of
    Left err -> do
      oldShader <- readIORef (shader rs)
      return (err,oldShader)
    Right newShader -> pure ("",newShader) -- TODO: brightness and resolution should be updated, but not here, in animationFrame instead
          

hideableEl :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Bool -> Text -> m a -> m a
hideableEl elType isVisible cssClass childWidget = do
  let attrs = fmap (bool (fromList [("hidden","true"),("class",cssClass)]) (Data.Map.singleton "class" cssClass)) isVisible
  elDynAttr elType attrs childWidget


data RenderState = RenderState {
  punctual :: Punctual,
  shader :: IORef Text,
  tPrevAnimationFrame :: IORef UTCTime,
  fps :: IORef MovingAverage
}


launchAnimationThread :: HTMLCanvasElement -> IO RenderState
launchAnimationThread canvas = do
  mic <- liftAudioIO createMicrophone
  gain <- liftAudioIO $ createGain (dbamp (-10))
  comp <- liftAudioIO $ createCompressor (-20) 3 4 0.050 0.1
  dest <- liftAudioIO $ createDestination
  connectNodes gain comp
  connectNodes comp dest
  ac <- getGlobalAudioContext
  tNow <- getCurrentTime
  p <- new canvas
  setAudioInput p mic
  setAudioOutput p comp
  setNchnls p 2
  setBrightness p 1.0
  setResolution p FHD
  shader' <- newIORef ""
  fps' <- newIORef $ newAverage 60
  tPrevAnimationFrame' <- newIORef tNow
  let rs = RenderState {
    punctual = p,
    shader = shader',
    tPrevAnimationFrame = tPrevAnimationFrame',
    fps = fps'
    }
  forkIO $ void $ animationThread rs 0
  return rs


animationThread :: RenderState -> Double -> IO ()
animationThread rs _ = do
  -- update FPS measurements 
  newTime <- getCurrentTime
  prevTime <- readIORef (tPrevAnimationFrame rs)
  let timeDiff = diffUTCTime newTime prevTime
  prevFps <- readIORef (fps rs)
  writeIORef (fps rs) $ updateAverage prevFps $ 1 / realToFrac timeDiff
  writeIORef (tPrevAnimationFrame rs) newTime
  -- update rendering
  render (punctual rs) True 0 newTime
  postRender (punctual rs) True
  void $ inAnimationFrame ContinueAsync $ animationThread rs


pollFPS :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m, MonadHold t m, MonadIO m) => RenderState -> m (Dynamic t Double)
pollFPS rs = do
  now <- liftIO $ getCurrentTime
  ticks <- tickLossy (1.004::NominalDiffTime) now
  x <- performEvent $ ffor ticks $ \_ -> liftIO $ do
    y <- readIORef (fps rs)
    pure $ getAverage y
  holdDyn 0 x
