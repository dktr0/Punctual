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
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Map.Strict
import Control.Monad
import GHCJS.DOM.Types hiding (Text,Event) -- (HTMLCanvasElement(..),uncheckedCastTo,JSVal,WebGLRenderingContext)
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import GHCJS.DOM.EventM
import Data.Bool
import Data.Maybe
import Data.Either

import Sound.Punctual.Action hiding ((>>),(<>))
import Sound.Punctual.Program
import Sound.Punctual.Parser
import Sound.Punctual.PunctualW
import Sound.Punctual.WebGL
import Sound.MusicW
import Sound.MusicW.AudioContext hiding (AudioTime)
import Sound.Punctual.GL
import Sound.Punctual.Resolution
import MovingAverage

headElement :: DomBuilder t m => m ()
headElement = do
  el "title" $ text "Punctual"
  let attrs = fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "style.css")]
  elAttr "link" attrs $ return ()

intro :: Text
intro
  ="-- Punctual, an audiovisual live coding language\n\
   \-- Chromium/Chrome/Edge/Opera browser required\n\
   \-- Press Shift-Enter to (re)evaluate/activate code\n\
   \-- documentation @ https://github.com/dktr0/Punctual.git\n\
   \-- help/discussion @ Estuary discord server\n\
   \\n\
   \x1 << sin $ 0.11*[1,2]; y1 << sin $ 0.08/[3,4];\n\
   \x2 << sin $ 0.06/[5,6]; y2 << sin $ 0.04*[7,8];\n\
   \lines << mono $ iline [x1,y1] [x2,y2] 0.002;\n\
   \col << hsvrgb [sin 0.11,0.5 ~~ 1 $ sin 0.12, 1];\n\
   \mask << prox 0 ** 8;\n\
   \fit 1 $ lines * col  * mask >> video <> 5;\n\
   \0.98 >> fdbk\n"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getGlobalAudioContextPlayback >>= addWorklets
  mainWidgetWithHead headElement bodyElement


data KeyboardShortCut = Evaluate | ToggleStatus | ToggleInfo deriving (Eq,Show)

keyboardShortCuts :: IsEvent t => Word -> Bool -> Bool -> EventM e t (Maybe KeyboardShortCut)
keyboardShortCuts 13 False True = preventDefault >> return (Just Evaluate) -- shift-Enter
keyboardShortCuts 19 True True = preventDefault >> return (Just ToggleStatus) -- ctrl-shift-S
keyboardShortCuts 9 True True = preventDefault >> return (Just ToggleInfo) -- ctrl-shift-I
keyboardShortCuts _ _ _ = return Nothing

foreign import javascript unsafe "$1['ctrlKey']"
  _getCtrlKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1['shiftKey']"
  _getShiftKey :: JSVal -> IO Bool


bodyElement :: (MonadIO m, MonadIO (Performable m), PerformEvent t m, MonadFix m, MonadHold t m, TriggerEvent t m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => m ()
bodyElement = do
  liftIO $ putStrLn "Punctual standalone"
  let attrs = fromList [("class","canvas"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" attrs $ return ()
  mv <- liftIO $ forkRenderThreads canvas

  elClass "div" "editorAndStatus" $ do
    (statusVisible,infoVisible,status,shader) <- mdo
      let textAttrs = constDyn $ fromList [("class","editorArea")]
      code <- hideableDiv shCode "editor" $ textArea $ def & textAreaConfig_attributes .~ textAttrs & textAreaConfig_initialValue .~ intro

      let e = _textArea_element code
      e' <- wrapDomEvent (e) (onEventName   Keypress) $ do
        ev <- event
        y <- getKeyEvent -- :: m Word
        ctrlKey <- liftIO $ _getCtrlKey (unKeyboardEvent ev)
        shiftKey <- liftIO $ _getShiftKey (unKeyboardEvent ev)
        keyboardShortCuts y ctrlKey shiftKey

      let evaled = tagPromptlyDyn (_textArea_value code) $ ffilter (==(Just Evaluate)) e'
      shStatus <- toggle True $ ffilter (==(Just ToggleStatus)) e'
      shCode <- toggle True $ ffilter (==(Just ToggleInfo)) e'
      shInfo <- toggle False $ ffilter (==(Just ToggleInfo)) e'
      (status,shader) <- performEvaluate mv evaled
      return (shStatus,shInfo,status,shader)

    hideableDiv statusVisible "status" $ do
      fps <- pollFPS mv
      elClass "div" "errors" $ dynText status
      elClass "div" "fps" $ do
        let fpsText = fmap ((<> " FPS") . showt . (round :: Double -> Int)) fps
        dynText fpsText




performEvaluate :: (PerformEvent t m, MonadIO (Performable m), MonadHold t m) => MVar RenderState -> Event t Text -> m (Dynamic t Text, Dynamic t Text)
performEvaluate mv e = do
  x <- performEvent $ ffor e (liftIO . doEvaluate mv)
  status <- holdDyn "" $ fmap fst x
  shader <- holdDyn "" $ fmap snd x
  return (status,shader)

doEvaluate :: MVar RenderState -> Text -> IO (Text,Text)
doEvaluate mv x = do
  now <- getCurrentTime
  case parse now x of
    Left err -> do
      let err' = T.pack err
      rs <- readMVar mv
      return (err',shader rs)
    Right p -> do
      rs <- takeMVar mv
      nW <- liftAudioIO $ updatePunctualW (punctualW rs) (tempo rs) p
      nGL <- setResolution (glCtx rs) FHD (punctualWebGL rs)
      nGL' <- setBrightness 1.0 nGL
      (nGL'',newShader) <- evaluatePunctualWebGL (glCtx rs) (tempo rs) 1 p nGL'
      putMVar mv $ rs {
        shader = newShader,
        punctualW = nW,
        punctualWebGL = nGL''
      }
      return ("",newShader)


hideableDiv :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m a -> m a
hideableDiv isVisible cssClass childWidget = do
  let attrs = fmap (bool (fromList [("hidden","true"),("class",cssClass)]) (Data.Map.Strict.singleton "class" cssClass)) isVisible
  elDynAttr "div" attrs childWidget


data RenderState = RenderState {
  glCtx :: GLContext,
  punctualW :: PunctualW,
  punctualWebGL :: PunctualWebGL,
  shader :: Text,
  fps :: MovingAverage,
  t0 :: UTCTime,
  tEval :: UTCTime,
  tempo :: Tempo,
  tPrevAnimationFrame :: UTCTime
}


forkRenderThreads :: HTMLCanvasElement -> IO (MVar RenderState)
forkRenderThreads canvas = do
  mic <- liftAudioIO createMicrophone
  gain <- liftAudioIO $ createGain (dbamp (-10))
  comp <- liftAudioIO $ createCompressor (-20) 3 4 0.050 0.1
  dest <- liftAudioIO $ createDestination
  connectNodes gain comp
  connectNodes comp dest
  ac <- getGlobalAudioContext
  tNow <- getCurrentTime
  let iW = emptyPunctualW ac mic gain 2 tNow -- hard coded stereo for now
  -- create PunctualWebGL for animation
  glc <- newGLContext canvas
  initialPunctualWebGL <- newPunctualWebGL (Just mic) (Just comp) FHD 1.0 canvas glc
  -- create an MVar for the render state, fork render threads, return the MVar
  t0system <- getCurrentTime
  mv <- newMVar $ RenderState {
    glCtx = glc,
    punctualW = iW,
    punctualWebGL = initialPunctualWebGL,
    shader = "",
    fps = newAverage 60,
    t0 = tNow,
    tEval = tNow,
    tempo = Tempo { freq=0.5, time=tNow, Data.Tempo.count=0},
    tPrevAnimationFrame = t0system
  }
  forkIO $ void $ animationThread mv 0
  return mv


animationThread :: MVar RenderState -> Double -> IO ()
animationThread mv _ = do
  rs <- takeMVar mv
  t1system <- getCurrentTime
  let tDiff = diffUTCTime t1system (tPrevAnimationFrame rs)
  let newFps = updateAverage (fps rs) $ 1 / realToFrac tDiff
  -- t1audio <- liftAudioIO $ audioTime
  let pwgl = punctualWebGL rs
  nGL <- drawPunctualWebGL (glCtx rs) (tempo rs) t1system 1 pwgl
  nGL' <- displayPunctualWebGL (glCtx rs) nGL
  putMVar mv $ rs {
    tPrevAnimationFrame = t1system,
    fps = newFps,
    punctualWebGL = nGL'
  }
  void $ inAnimationFrame ContinueAsync $ animationThread mv


pollFPS :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m, MonadHold t m, MonadIO m) => MVar RenderState -> m (Dynamic t Double)
pollFPS mv = do
  now <- liftIO $ getCurrentTime
  ticks <- tickLossy (1.004::NominalDiffTime) now
  x <- performEvent $ ffor ticks $ \_ -> liftIO $ do
    rs <- readMVar mv
    return $ getAverage $ fps rs
  holdDyn 0 x
