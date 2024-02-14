module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Data.Tempo (newTempo)
import Data.Either (Either(..))
import Data.Rational ((%))

import FragmentShader (fragmentShader,onlyVideoOutputs)
import Program (emptyProgram)
import Parser (parsePunctual)
  
test :: Boolean -> String -> Effect Unit
test webGl2 txt = do
  eTime <- nowDateTime
  case parsePunctual txt eTime of
    Left err -> log $ show err
    Right p1 -> do
      p0 <- emptyProgram
      tempo <- newTempo (1 % 1)
      log $ fragmentShader webGl2 tempo p0 p1

