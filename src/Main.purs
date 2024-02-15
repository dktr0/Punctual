module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Data.Tempo (newTempo)
import Data.Either (Either(..))
import Data.Rational ((%))
import Data.Map (Map, empty)
import Effect.Ref (Ref, new)

import FragmentShader (fragmentShader)
import Program (Program,emptyProgram)
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
      
type Punctual = {
  programs :: Ref (Map Int Program)
  }

launch :: Effect Punctual
launch = do
  log "punctual 0.5 initialization complete"
  programs <- new empty
  pure {
    programs
  }

define :: Punctual -> { zone :: Int, time :: Number, text :: String } -> Effect { success :: Boolean, info :: String, error :: String }
define _ args = do
  log $ "define: " <> show args
  placeholder <- nowDateTime
  case parsePunctual args.text placeholder of -- placeholder should be time from args instead of from nowDateTime, but Number doesn't match DateTime
    Left err -> do
      log $ "error: " <> show err
      pure { success: false, info: "", error: show err }
    Right p1 -> do
      p0 <- emptyProgram
      tempo <- newTempo (1 % 1)
      let fs = fragmentShader true tempo p0 p1
      log $ "success: " <> show fs
      pure { success: true, info: fs, error: "" }
  
clear :: Punctual -> { zone :: Int } -> Effect Unit
clear _ _ = pure unit

setTempo :: Punctual -> forall r. { | r } -> Effect Unit
setTempo _ _ = pure unit

preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
preRender _ _ = pure unit

render :: Punctual -> { zone :: Int, canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit -- later will be Effect (Array Foreign)
render _ _ = pure unit

postRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
postRender _ _ = pure unit

