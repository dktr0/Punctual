{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Reflex.Dom
import Data.Time.Clock
import Data.Text

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.Parser
import Sound.Punctual.PunctualW
import Sound.MusicW
import Sound.MusicW.AudioContext

main :: IO ()
main = mainWidget $ do
  el "div" $ text "Punctual"
  evalButton <- el "div" $ button "eval"
  code <- el "div" $ textArea def
  let evaled = tagDyn (_textArea_value code) evalButton
  let parsed = fmap (runPunctualParser . unpack) evaled
  punctualReflex $ fmapMaybe (either (const Nothing) Just) parsed
  let errors = fmapMaybe (either (Just . show) (Just . show)) parsed
  status <- holdDyn "" $ fmap (pack . show) errors
  dynText status

punctualReflex :: MonadWidget t m => Event t [Expression] -> m ()
punctualReflex exprs = mdo
  ac <- liftAudioIO $ audioContext
  now <- liftAudioIO $ audioUTCTime
  dest <- liftAudioIO $ createDestination
  let initialPunctualW = emptyPunctualW ac dest now
  evals <- performEvent $ fmap (liftIO . evaluationNow) exprs
  let f pW e = liftAudioIO $ updatePunctualW pW e
  newPunctualW <- performEvent $ attachDynWith f currentPunctualW evals
  currentPunctualW <- holdDyn initialPunctualW newPunctualW
  return ()
