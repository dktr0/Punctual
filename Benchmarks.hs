{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either
import Criterion
import Criterion.Types
import Criterion.Main

import Sound.Punctual.Graph
import Sound.Punctual.Parser
import Sound.Punctual.Evaluation
import Sound.Punctual.FragmentShader

text1ToParse :: IO Text
text1ToParse = return "sin 440 >> splay"

text2ToParse :: IO Text
text2ToParse = return "mono (iline (sin (0.11*1...16)) (sin (0.08/1...16)) (sin (0.06/1...16)) (sin (0.04*1...16)) 0.002) * [sin 0.11,0.5 ~~ 1 $ sin 0.12, 1] * (1 - rect 0 0.875 2 0.25) >> hsv <> 5; 0.98 * fb fx fy * (fb fx fy > 0.1) >> rgb"

evaluation :: IO Evaluation
evaluation = do
  x <- text2ToParse
  let y = runPunctualParser x
  return $! (fromRight [] y,0.0)

main :: IO ()
main =
  defaultMainWith (defaultConfig {reportFile = Just "benchmarks.html"})
  [
--  env text1ToParse (\x -> bench "parse" $ nf runPunctualParser x),
--  env text2ToParse (\x -> bench "parse" $ nf runPunctualParser x),
  env evaluation (\x -> bench "toFragShader" $ nf (fragmentShader [] (0,1)) x)
  ]
