{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either
import TextShow
import Data.Foldable
import Criterion
import Criterion.Types
import Criterion.Main
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Time

import Sound.Punctual.Graph
import Sound.Punctual.Parser
import Sound.Punctual.FragmentShader

data Thought = Yes | No deriving (Generic,NFData)

type T = Builder

toBuilder :: Thought -> T
toBuilder Yes = "ten"
toBuilder No = "five"

data Thought' = Thought' {
  toBuilder' :: T
  } -- deriving (Generic,NFData)

yes :: Thought'
yes = Thought' { toBuilder' = "ten" }

no :: Thought'
no = Thought' { toBuilder' = "five" }

thoughts :: IO [Thought]
thoughts = return $! take 10000 $ cycle [Yes,No,Yes,Yes,No,No,Yes]

thoughts' :: IO [Thought']
thoughts' = return $! take 10000 $ cycle [yes,no,yes,yes,no,no,yes]



text1ToParse :: IO Text
text1ToParse = return "sin 440 >> rgb"

text2ToParse :: IO Text
text2ToParse = return "mono (iline (sin (0.11*1...16)) (sin (0.08/1...16)) (sin (0.06/1...16)) (sin (0.04*1...16)) 0.002) * [sin 0.11,0.5 ~~ 1 $ sin 0.12, 1] * (1 - rect 0 0.875 2 0.25) >> hsv <> 5; 0.98 * fb fx fy * (fb fx fy > 0.1) >> rgb"

{-
evaluation :: IO Evaluation
evaluation = do
  x <- text2ToParse
  let y = runPunctualParser x
  return $! (fromRight [] y,0.0)
-}


main :: IO ()
main = do
  t1 <- getCurrentTime
  a <- return $! (toText $ fold $ fmap toBuilder $ take 100000 $ cycle [Yes,No,Yes,Yes,No,No,Yes])
  putStrLn $ show (T.length a)
  t2 <- getCurrentTime
  b <- return $! (toText $ fold $ fmap toBuilder' $ take 100000 $ cycle [yes,no,yes,yes,no,no,yes])
  putStrLn $ show (T.length b)
  t3 <- getCurrentTime
  putStrLn $ show (realToFrac (diffUTCTime t2 t1) :: Double)
  putStrLn $ show (realToFrac (diffUTCTime t3 t2) :: Double)


{-  defaultMainWith (defaultConfig {reportFile = Just "benchmarks.html"})
  [
  bench "thoughts" $ nf (\x -> fold $ fmap (toBuilder) $ take x $ cycle [Yes,No,Yes,Yes,No,No,Yes]) 1000,
  bench "thoughts'" $ nf (\x -> fold $ fmap (toBuilder') $ take x $ cycle [yes,no,yes,yes,no,no,yes]) 1000
--  env text1ToParse (\x -> bench "parse" $ nf runPunctualParser x),
--  env text2ToParse (\x -> bench "parse" $ nf runPunctualParser x),
--  env evaluation (\x -> bench "toFragShader" $ nf (fragmentShader [] (0,1)) x)
  ]
-}
