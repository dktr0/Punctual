module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds)
import Data.DateTime (diff)
import Data.Either (Either(..))
import Data.Map (Map, empty, lookup, insert, delete)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Tempo (ForeignTempo, fromForeignTempo)
import Data.Foldable (fold)
import Data.Newtype (unwrap)

import Signal (SignalInfo,emptySignalInfo)
import Program (Program,emptyProgram,programHasVisualOutput,programInfo)
import Parser (parsePunctual,parseSignal)
import WebGL (WebGL, newWebGL, updateWebGL, deleteWebGL, drawWebGL)
import DateTime (numberToDateTime)
import SharedResources (SharedResources)
import SharedResources as SharedResources
import AudioWorklet (runWorklet,stopWorklet,AudioWorklet)
import WebAudio (resumeWebAudioContext,currentTime)
  
type Punctual = {
  sharedResources :: SharedResources,
  programs :: Ref (Map Int Program),
  previousPrograms :: Ref (Map Int Program),
  programInfos :: Ref (Map Int SignalInfo),
  previousProgramInfos :: Ref (Map Int SignalInfo),
  combinedProgramInfo :: Ref SignalInfo,
  webGLs :: Ref (Map Int WebGL),
  audioWorklet :: Ref (Maybe AudioWorklet)
  }


launch :: Effect Punctual
launch = do
  sharedResources <- SharedResources.newSharedResources Nothing
  programs <- new empty
  previousPrograms <- new empty
  programInfos <- new empty
  previousProgramInfos <- new empty
  combinedProgramInfo <- new emptySignalInfo
  webGLs <- new empty
  audioWorklet <- new Nothing
  log "punctual 0.5 initialization complete"
  pure { sharedResources, programs, previousPrograms, programInfos, previousProgramInfos, combinedProgramInfo, webGLs, audioWorklet }


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
    Right newProgram -> _newProgramInZone punctual args.zone newProgram
      
_newProgramInZone :: Punctual -> Int -> Program -> Effect { success :: Boolean, info :: String, error :: String }
_newProgramInZone punctual zone newProgram = do
  programs <- read punctual.programs
  previousPrograms <- read punctual.previousPrograms
  programInfos <- read punctual.programInfos
  previousProgramInfos <- read punctual.previousProgramInfos
  previousProgram <-
    case lookup zone programs of
      Just x -> pure x
      Nothing -> emptyProgram
  previousProgramInfo <-
    case lookup zone programInfos of
      Just x -> pure x
      Nothing -> pure emptySignalInfo
  let newPrograms = insert zone newProgram programs
  let newPreviousPrograms = insert zone previousProgram previousPrograms
  let newProgramInfos = insert zone (programInfo newProgram) programInfos
  let newPreviousProgramInfos = insert zone previousProgramInfo previousProgramInfos
  write newPrograms punctual.programs
  write newPreviousPrograms punctual.previousPrograms
  write newProgramInfos punctual.programInfos
  write newPreviousProgramInfos punctual.previousProgramInfos
  _updateCombinedProgramInfo punctual
  -- update visual rendering system
  info <- case programHasVisualOutput newProgram of 
    true -> updateWebGLForZone punctual zone newProgram previousProgram
    false -> do
      deleteWebGLForZone punctual zone
      pure ""
  -- TODO: update audio rendering system
  pure { success: true, info, error: "" }
      
_updateCombinedProgramInfo :: Punctual -> Effect Unit
_updateCombinedProgramInfo punctual = do
  programsInfo <- fold <$> read punctual.programInfos
  previousProgramsInfo <- fold <$> read punctual.previousProgramInfos
  let combinedInfo = programsInfo <> previousProgramsInfo
  log $ "_updateCombinedProgramInfo: " <> show combinedInfo
  write combinedInfo punctual.combinedProgramInfo
        
clear :: Punctual -> { zone :: Int } -> Effect Unit
clear punctual args = do
  programs <- read punctual.programs
  previousPrograms <- read punctual.previousPrograms
  programInfos <- read punctual.programInfos
  previousProgramInfos <- read punctual.previousProgramInfos
  let newPrograms = delete args.zone programs
  let newPreviousPrograms = delete args.zone previousPrograms
  let newProgramInfos = delete args.zone programInfos
  let newPreviousProgramInfos = delete args.zone previousProgramInfos
  write newPrograms punctual.programs
  write newPreviousPrograms punctual.previousPrograms
  write newProgramInfos punctual.programInfos
  write newPreviousProgramInfos punctual.previousProgramInfos
  _updateCombinedProgramInfo punctual
  deleteWebGLForZone punctual args.zone
  
  
setTempo :: Punctual -> ForeignTempo -> Effect Unit
setTempo punctual ft = SharedResources.setTempo punctual.sharedResources (fromForeignTempo ft)


preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number } -> Effect Unit
preRender punctual args = when args.canDraw $ _updateSharedResources punctual

_updateSharedResources :: Punctual -> Effect Unit
_updateSharedResources punctual = do
  combinedInfo <- read punctual.combinedProgramInfo
  SharedResources.setWebcamActive punctual.sharedResources $ unwrap combinedInfo.webcam
  SharedResources.updateAudioAnalysers punctual.sharedResources combinedInfo


render :: Punctual -> { zone :: Int, canDraw :: Boolean, nowTime :: Number } -> Effect Unit -- later will be Effect (Array Foreign)
render punctual args = do
  -- log $ "render: " <> show args
  case args.canDraw of 
    false -> pure unit
    true -> do
      webGLs <- read punctual.webGLs
      case lookup args.zone webGLs of
        Nothing -> pure unit
        Just w -> drawWebGL w (numberToDateTime args.nowTime)
        
        
postRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number } -> Effect Unit
postRender _ _ = pure unit -- log $ "postRender: " <> show args




-- below this line are functions that are not directly part of the exolang API

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

  
test :: Punctual -> String -> String -> Effect Unit
test p name txt = do
  let mTestSignal = parseSignal txt
  case mTestSignal of
    Right testSignal -> do
      resumeWebAudioContext p.sharedResources.webAudioContext
      t <- currentTime p.sharedResources.webAudioContext
      mAudioWorklet <- read p.audioWorklet  
      case mAudioWorklet of
        Just audioWorklet -> stopWorklet audioWorklet (t+0.5) 5.0
        Nothing -> pure unit
      audioWorklet <- runWorklet p.sharedResources.webAudioContext p.sharedResources.audioOutputNode name testSignal (t+0.5) 5.0
      write (Just audioWorklet) p.audioWorklet
    Left _ -> log "parse error in test"





















