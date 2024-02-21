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
import Data.Tempo (ForeignTempo, fromForeignTempo, newTempo)
import Data.Foldable (any)

import Program (Program,emptyProgram,programHasVisualOutput,programNeedsWebcam)
import Parser (parsePunctual)
import WebGL (WebGL, newWebGL, updateWebGL, deleteWebGL, drawWebGL)
import DateTime (numberToDateTime)
import FragmentShader (fragmentShader)
import SharedResources (SharedResources)
import SharedResources as SharedResources
  
type Punctual = {
  sharedResources :: SharedResources,
  programs :: Ref (Map Int Program),
  previousPrograms :: Ref (Map Int Program),
  webGLs :: Ref (Map Int WebGL)
  }


launch :: Effect Punctual
launch = do
  sharedResources <- SharedResources.newSharedResources
  programs <- new empty
  previousPrograms <- new empty
  webGLs <- new empty
  log "punctual 0.5 initialization complete"
  pure { sharedResources, programs, previousPrograms, webGLs }


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
    Right newProgram -> do
      -- update record of current and previous programs for this zone
      programs <- read punctual.programs
      previousProgram <-
        case lookup args.zone programs of
          Just x -> pure x
          Nothing -> emptyProgram
      previousPrograms <- read punctual.previousPrograms
      write (insert args.zone previousProgram previousPrograms) punctual.previousPrograms
      write (insert args.zone newProgram programs) punctual.programs
      -- update visual rendering system
      info <- case programHasVisualOutput newProgram of 
        true -> updateWebGLForZone punctual args.zone newProgram previousProgram
        false -> do
          deleteWebGLForZone punctual args.zone
          pure ""
      -- TODO: audio rendering system
      pure { success: true, info, error: "" }
        
        
clear :: Punctual -> { zone :: Int } -> Effect Unit
clear punctual args = do
  programs <- read punctual.programs
  write (delete args.zone programs) punctual.programs
  previousPrograms <- read punctual.previousPrograms
  write (delete args.zone previousPrograms) punctual.previousPrograms
  deleteWebGLForZone punctual args.zone
  
  
setTempo :: Punctual -> ForeignTempo -> Effect Unit
setTempo punctual ft = SharedResources.setTempo punctual.sharedResources (fromForeignTempo ft)


preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number, previousDrawTime :: Number } -> Effect Unit
preRender punctual args = when args.canDraw do  
  -- if any current or immediately preceding programs require the webcam, it should be active
  programsNeedWebcam <- any programNeedsWebcam <$> read punctual.programs
  previousProgramsNeedWebcam <- any programNeedsWebcam <$> read punctual.previousPrograms
  SharedResources.setWebcamActive punctual.sharedResources (programsNeedWebcam || previousProgramsNeedWebcam)
  -- TODO: here we would also update audio analysis, FFT, etc values as necessary


render :: Punctual -> { zone :: Int, canDraw :: Boolean, nowTime :: Number } -> Effect Unit -- later will be Effect (Array Foreign)
render punctual args = do
  case args.canDraw of 
    false -> pure unit
    true -> do
      webGLs <- read punctual.webGLs
      case lookup args.zone webGLs of
        Nothing -> pure unit
        Just w -> drawWebGL w (numberToDateTime args.nowTime)
        
        
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

updateWebGLForZone :: Punctual -> Int -> Program -> Program -> Effect String -- String is fragment shader code
updateWebGLForZone punctual z prog prevProg = do
  webGLs <- read punctual.webGLs
  case lookup z webGLs of 
    Just w -> do
      updateWebGL w prog prevProg
      read w.shaderSrc
    Nothing -> do
      w <- newWebGL punctual.sharedResources prog prevProg
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

