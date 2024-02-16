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

import FragmentShader (fragmentShader)
import Program (Program,emptyProgram,programHasVisualOutput)
import Parser (parsePunctual)
import WebGLCanvas (WebGLCanvas, getWebGLCanvas, deleteWebGLCanvas)
import DateTime (numberToDateTime)
  
  
type Punctual = {
  tempo :: Ref Tempo,
  programs :: Ref (Map Int Program),
  webGLCanvases :: Ref (Map Int WebGLCanvas)
  }


launch :: Effect Punctual
launch = do
  tempo <- newTempo (1 % 1) >>= new
  programs <- new empty
  webGLCanvases <- new empty
  log "punctual 0.5 initialization complete"
  pure { tempo, programs, webGLCanvases }


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
    Right p1 -> do
      programs <- read punctual.programs
      p0 <- case lookup args.zone programs of
              Just p -> pure p
              Nothing -> emptyProgram        
      write (insert args.zone p1 programs) punctual.programs
      case programHasVisualOutput p1 of 
        true -> do
          mCanvas <- getWebGLCanvasForZone punctual args.zone
          case mCanvas of
            Just canvas -> do
              log $ "webGL2: " <> show canvas.webGL2
              tempo <- read punctual.tempo
              t2 <- nowDateTime
              let fs = fragmentShader true tempo p0 p1
              t3 <- nowDateTime
              log $ " GLSL transpile time = " <> show (diff t3 t2 :: Milliseconds)
              log $ "success: " <> show fs
              pure { success: true, info: fs, error: "" }
            Nothing -> do
              log $ "punctual: program has visual output but a WebGL canvas cannot be created"
              pure { success: true, info: "", error: "" }
        false -> do
          deleteWebGLCanvasForZone punctual args.zone
          pure { success: true, info: "", error: "" }
        
        
clear :: Punctual -> { zone :: Int } -> Effect Unit
clear punctual args = do
  programs <- read punctual.programs
  write (delete args.zone programs) punctual.programs
  deleteWebGLCanvasForZone punctual args.zone
  
setTempo :: Punctual -> ForeignTempo -> Effect Unit
setTempo punctual ft = write (fromForeignTempo ft) punctual.tempo

preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
preRender _ _ = pure unit

render :: Punctual -> { zone :: Int, canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit -- later will be Effect (Array Foreign)
render _ _ = pure unit

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

getWebGLCanvasForZone :: Punctual -> Int -> Effect (Maybe WebGLCanvas)
getWebGLCanvasForZone punctual z = do
  webGLCanvases <- read punctual.webGLCanvases
  case lookup z webGLCanvases of 
    Just c -> pure (Just c)
    Nothing -> do
      c <- getWebGLCanvas
      case c of
        Just c' -> do
          write (insert z c' webGLCanvases) punctual.webGLCanvases
          pure (Just c')
        Nothing -> pure Nothing

deleteWebGLCanvasForZone :: Punctual -> Int -> Effect Unit
deleteWebGLCanvasForZone punctual z = do
  webGLCanvases <- read punctual.webGLCanvases
  case lookup z webGLCanvases of 
    Just c -> do
       log "punctual DEBUG: deleting canvas"
       deleteWebGLCanvas c
       write (delete z webGLCanvases) punctual.webGLCanvases
    Nothing -> pure unit

