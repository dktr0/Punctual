{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Trans
import Reflex.Dom
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.PunctualW

main :: IO ()
main = mainWidget $ do
  el "div" $ text "Punctual today"
  evalButton <- el "div" $ button "eval"
  code <- el "div" $ textArea def
  let evaled = tagDyn (_textArea_value code) evalButton
  let parsed = fmap runPunctualParser evaled
  punctualReflex $ fmapMaybe (either (const Nothing) Just) parsed
  let errors = fmapMaybe (either (Just . show) (const (Just "Ok"))) parsed
  status <- holdDyn "" $ fmap show errors
  dynText status

punctualReflex :: MonadWidget t m => Event t [Expression] -> m ()
punctualReflex exprs = mdo
  evals <- performEvent $ fmap (liftIO . evaluationNow) exprs
  let punctualWAndEval = attachDyn currentPunctualW evals
  newPunctualW <- performEvent $ fmap (liftIO . (uncurry updatePunctualW)) punctualWAndEval
  currentPunctualW <- holdDyn emptyPunctualW newPunctualW
  return ()
