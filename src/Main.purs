module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds)
import Data.DateTime (diff)
import Data.Either (Either(..))
import Data.Rational ((%))
import Data.Map (Map, empty, lookup, insert, delete)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Tempo (ForeignTempo, Tempo, fromForeignTempo, newTempo)

import Program (Program,emptyProgram,programHasVisualOutput)
import Parser (parsePunctual)
import WebGL (WebGL, newWebGL, updateWebGL, deleteWebGL, drawWebGL)
import DateTime (numberToDateTime)
import FragmentShader (fragmentShader)

  
type Punctual = {
  tempo :: Ref Tempo,
  programs :: Ref (Map Int Program),
  webGLs :: Ref (Map Int WebGL)
  }


launch :: Effect Punctual
launch = do
  tempo <- newTempo (1 % 1) >>= new
  programs <- new empty
  webGLs <- new empty
  log "punctual 0.5 initialization complete"
  pure { tempo, programs, webGLs }


define :: Punctual -> { zone :: Int, time :: Number, text :: String } -> Effect { success :: Boolean, info :: String, error :: String }
define punctual args = do
  log $ "define: " <> show args
  t0 <- nowDateTime
  let pr = parsePunctual args.text (numberToDateTime args.time)
  t1 <- nowDateTime
  log $ " parse time = " <> show (diff t1 t0 :: Milliseconds)
  case pr of
    Left err -> do
      log $ "error: " <> show err
      pure { success: false, info: "", error: show err }
    Right program -> do
      tempo <- read punctual.tempo            
{-      programs <- read punctual.programs
      oldProg <- case lookup args.zone programs of
              Just p -> pure p
              Nothing -> emptyProgram        
      write (insert args.zone program programs) punctual.programs -}
      info <- case programHasVisualOutput program of 
        true -> updateWebGLForZone punctual args.zone tempo program
        false -> do
          deleteWebGLForZone punctual args.zone
          pure ""
      pure { success: true, info, error: "" }
        
        
clear :: Punctual -> { zone :: Int } -> Effect Unit
clear punctual args = do
  programs <- read punctual.programs
  write (delete args.zone programs) punctual.programs
  deleteWebGLForZone punctual args.zone
  
setTempo :: Punctual -> ForeignTempo -> Effect Unit
setTempo punctual ft = write (fromForeignTempo ft) punctual.tempo

preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
preRender _ _ = pure unit

render :: Punctual -> { zone :: Int, canDraw :: Boolean, nowTime :: Number } -> Effect Unit -- later will be Effect (Array Foreign)
render punctual args = do
  case args.canDraw of 
    false -> pure unit
    true -> do
      webGLs <- read punctual.webGLs
      case lookup args.zone webGLs of
        Nothing -> pure unit
        Just w -> do
          tempo <- read punctual.tempo
          drawWebGL w tempo (numberToDateTime args.nowTime)
        
postRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
postRender _ _ = pure unit



-- below this line are functions that are not directly part of the exolang API

test :: Boolean -> String -> Effect Unit
test webGl2 txt = do
  eTime <- nowDateTime
  case parsePunctual txt eTime of
    Left err -> log $ show err
    Right p1 -> do
      p0 <- emptyProgram
      tempo <- newTempo (1 % 1)
      log $ fragmentShader webGl2 tempo p0 p1

updateWebGLForZone :: Punctual -> Int -> Tempo -> Program -> Effect String -- String is fragment shader code
updateWebGLForZone punctual z tempo prog = do
  webGLs <- read punctual.webGLs
  case lookup z webGLs of 
    Just w -> do
      updateWebGL w tempo prog
      read w.shaderSrc
    Nothing -> do
      w <- newWebGL tempo prog
      case w of
        Just w' -> do
          write (insert z w' webGLs) punctual.webGLs
          read w'.shaderSrc
        Nothing -> pure "unable to make canvas or WebGL context for program"

deleteWebGLForZone :: Punctual -> Int -> Effect Unit
deleteWebGLForZone punctual z = do
  webGLs <- read punctual.webGLs
  case lookup z webGLs of 
    Just w -> do
       log "punctual DEBUG: deleting WebGL"
       deleteWebGL w
       write (delete z webGLs) punctual.webGLs
    Nothing -> pure unit

