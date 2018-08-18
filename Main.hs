module Main where

import Reflex.Dom
import Sound.Punctual.Types
import Sound.Punctual.Evaluation

main :: IO ()
main = mainWidget $ do
  el "div" $ text "Punctual"
  evals <- el "div" $ textAreaWithEvalButton
  status <- punctual evals
  dynText status

textAreaWithEvalButton :: MonadWidget t m => m (Event t String)
textAreaWithEvalButton = do
  y <- el "div" $ button "eval"
  x <- el "div" $ textArea def
  return $ tagDyn (_textArea_value x) y

punctual :: MonadWidget t m => Event t String -> m (Dynamic t String)
punctual evals = do
  let x = fmap runPunctualParser evals
  holdDyn "" $ fmap show x
