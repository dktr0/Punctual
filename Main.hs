{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Reflex.Dom hiding (getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Data.Time.Clock
import Data.Text as T
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Map.Strict
import Control.Monad
import GHCJS.DOM.Types (HTMLCanvasElement(..),uncheckedCastTo)
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import GHCJS.DOM.EventM

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.Parser
import Sound.Punctual.PunctualW
import Sound.Punctual.WebGL
import Sound.MusicW
import Sound.MusicW.AudioContext

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
   \-- Press Shift-Enter to (re)evaluate/activate code\n\
   \\n\
   \sin 60m * -10db => left;\n\
   \sin 60.05m * -10db => right;\n\
   \fx => red;\n\
   \fy * -1 => green;\n\
   \sin (fx * 60m) * sin (fy * 60.05m) * fx * fy * 10db => blue;\n"

main :: IO ()
main = mainWidgetWithHead headElement $ do

  let attrs = fromList [("class","canvas"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" attrs $ return ()
  initialPunctualWebGL <- liftIO $ updateRenderingContext emptyPunctualWebGL (Just canvas)
  mv <- liftIO $ newMVar initialPunctualWebGL
  liftIO $ forkIO $ requestAnimationFrame mv

  parsed <- elClass "div" "editor" $ do
    -- elClass "div" "title" $ text "Punctual" -- title just as comment in editor maybe?
    let textAttrs = constDyn $ fromList [("class","editorArea"),("rows","999")]
    code <- elClass "div" "editorDiv" $ textArea $ def & textAreaConfig_attributes .~ textAttrs & textAreaConfig_initialValue .~ intro
    let e = _textArea_element code
    e' <- wrapDomEvent (e) (onEventName Keypress) $ do
      y <- getKeyEvent
      let keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)
      if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
    let evalEvent = ffilter (==True) e'
    let evaled = tagDyn (_textArea_value code) evalEvent
    return $ fmap (runPunctualParser . unpack) evaled

  punctualReflex mv $ fmapMaybe (either (const Nothing) Just) parsed
  let errors = fmapMaybe (either (Just . show) (Just . const "")) parsed
  status <- holdDyn "" $ fmap (pack . show) errors
  dynText status

punctualReflex :: MonadWidget t m => MVar PunctualWebGL -> Event t [Expression] -> m ()
punctualReflex mv exprs = mdo
  ac <- liftAudioIO $ audioContext
  dest <- liftAudioIO $ createDestination
  t0 <- liftAudioIO $ audioUTCTime
  let initialPunctualW = emptyPunctualW ac dest t0
  evals <- performEvent $ fmap (liftIO . evaluationNow) exprs
  -- audio
  let f pW e = liftAudioIO $ updatePunctualW pW (t0,0.5) e
  newPunctualW <- performEvent $ attachDynWith f currentPunctualW evals
  currentPunctualW <- holdDyn initialPunctualW newPunctualW
  -- video
  performEvent $ fmap (liftIO . evaluatePunctualWebGL' (t0,0.5) mv) evals -- *** note: tempo hard-coded here
  return ()

evaluatePunctualWebGL' :: (UTCTime,Double) -> MVar PunctualWebGL -> Evaluation -> IO ()
evaluatePunctualWebGL' t mv e = do
  x <- takeMVar mv
  y <- evaluatePunctualWebGL x t e
  putMVar mv y

evaluationNow :: [Expression] -> IO Evaluation
evaluationNow exprs = do
  t <- liftAudioIO $ audioUTCTime
  return (exprs,t)

requestAnimationFrame :: MVar PunctualWebGL -> IO ()
requestAnimationFrame mv = do
  inAnimationFrame ThrowWouldBlock $ redrawCanvas mv
  return ()

redrawCanvas :: MVar PunctualWebGL -> Double -> IO ()
redrawCanvas mv _ = synchronously $ do
  st <- takeMVar mv
  t1 <- liftAudioIO $ audioTime
  drawFrame t1 st
  putMVar mv st
  requestAnimationFrame mv
