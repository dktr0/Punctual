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
import Sound.MusicW.AudioRoutingGraph

main :: IO ()
main = mainWidget $ do
  ac <- liftIO $ getAudioContext
  el "div" $ text "Punctual"
  evalButton <- el "div" $ button "eval"
  code <- el "div" $ textArea def
  let evaled = tagDyn (_textArea_value code) evalButton
  let parsed = fmap (runPunctualParser . unpack) evaled
  punctualReflex ac $ fmapMaybe (either (const Nothing) Just) parsed
  let errors = fmapMaybe (either (Just . show) (Just . show)) parsed
  status <- holdDyn "" $ fmap (pack . show) errors
  dynText status

punctualReflex :: MonadWidget t m => WebAudioContext -> Event t [Expression] -> m ()
punctualReflex ac exprs = mdo
  now <- liftIO $ getAudioTime ac
  evals <- performEvent $ fmap (liftIO . evaluationNow) exprs
  let punctualWAndEval = attachDyn currentPunctualW evals
  newPunctualW <- performEvent $ fmap (liftIO . (uncurry updatePunctualW)) punctualWAndEval
  currentPunctualW <- holdDyn (emptyPunctualW ac now)  newPunctualW
  return ()
