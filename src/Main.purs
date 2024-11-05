module Main where

import Prelude

import Effect (Effect)
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
import Control.Promise (fromAff,Promise)
import Effect.Class.Console (log)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (throwError)
import Effect.Exception (error)
import Data.Nullable (Nullable,toMaybe)

import Signal (SignalInfo,emptySignalInfo)
import Program (Program,emptyProgram,programHasVisualOutput,programHasAudioOutput,programInfo)
import Parser (parseProgram)
import DateTime (numberToDateTime)
import SharedResources (SharedResources)
import SharedResources as SharedResources
import WebGL (WebGL, newWebGL, updateWebGL, deleteWebGL, drawWebGL)
import AudioZone (AudioZone,newAudioZone,redefineAudioZone,deleteAudioZone)
import WebAudio (WebAudioNode,WebAudioContext)
  
type Punctual = {
  sharedResources :: SharedResources,
  programs :: Ref (Map Int Program),
  previousPrograms :: Ref (Map Int Program),
  programInfos :: Ref (Map Int SignalInfo),
  previousProgramInfos :: Ref (Map Int SignalInfo),
  combinedProgramInfo :: Ref SignalInfo,
  webGLs :: Ref (Map Int WebGL),
  audioZones :: Ref (Map Int AudioZone)
  }


launch :: { webAudioContext :: Nullable WebAudioContext } -> Effect Punctual
launch args = do
  let mWebAudioContext = toMaybe args.webAudioContext
  sharedResources <- SharedResources.newSharedResources mWebAudioContext
  programs <- new empty
  previousPrograms <- new empty
  programInfos <- new empty
  previousProgramInfos <- new empty
  combinedProgramInfo <- new emptySignalInfo
  webGLs <- new empty
  audioZones <- new empty
  log "punctual 0.5 initialization complete"
  pure { sharedResources, programs, previousPrograms, programInfos, previousProgramInfos, combinedProgramInfo, webGLs, audioZones }


define :: Punctual -> { zone :: Int, time :: Number, text :: String } -> Effect (Promise { info :: String })
define punctual args = fromAff $ do
  -- log $ "define: " <> show args
  t0 <- liftEffect $ nowDateTime
  pr <- parseProgram punctual.sharedResources.libraries args.text (numberToDateTime args.time)
  t1 <- liftEffect $ nowDateTime
  -- log $ " parse time = " <> show (diff t1 t0 :: Milliseconds)
  case pr of
    Left err -> do
      let eString = show err
      throwError $ error eString
    Right newProgram -> do
      info <- liftEffect $ _newProgramInZone punctual args.zone newProgram
      pure { info }

    
_newProgramInZone :: Punctual -> Int -> Program -> Effect String
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
  let newProgramDebug = "new program: " <> show newProgram <> "\n\n"
  let previousProgramDebug = "previous program: " <> show previousProgram <> "\n\n" 
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
  fragShaderDebug <- case programHasVisualOutput newProgram of 
    true -> do
      fragShaderSrc <- updateWebGLForZone punctual zone newProgram previousProgram
      pure $ "fragment shader: " <> fragShaderSrc <> "\n\n"
    false -> do
      deleteWebGLForZone punctual zone
      pure ""
  -- update audio rendering system
  case programHasAudioOutput newProgram of
    true -> updateAudioForZone punctual zone newProgram
    false -> deleteAudioForZone punctual zone
  let debugInfo = newProgramDebug <> previousProgramDebug <> fragShaderDebug
  -- log debugInfo
  pure debugInfo
      
_updateCombinedProgramInfo :: Punctual -> Effect Unit
_updateCombinedProgramInfo punctual = do
  programsInfo <- fold <$> read punctual.programInfos
  previousProgramsInfo <- fold <$> read punctual.previousProgramInfos
  let combinedInfo = programsInfo <> previousProgramsInfo
  -- log $ "_updateCombinedProgramInfo: " <> show combinedInfo
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
  deleteAudioForZone punctual args.zone
  
  
preRender :: Punctual -> { canDraw :: Boolean, nowTime :: Number } -> Effect Unit
preRender punctual args = when args.canDraw $ _updateSharedResources punctual

_updateSharedResources :: Punctual -> Effect Unit
_updateSharedResources punctual = do
  combinedInfo <- read punctual.combinedProgramInfo
  SharedResources.setWebcamActive punctual.sharedResources $ unwrap combinedInfo.webcam
  SharedResources.updateAudioInputAndAnalysers punctual.sharedResources combinedInfo


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

setTempo :: Punctual -> ForeignTempo -> Effect Unit
setTempo punctual ft = SharedResources.setTempo punctual.sharedResources (fromForeignTempo ft)

setAudioInput :: Punctual -> Effect WebAudioNode -> Effect Unit
setAudioInput punctual e = SharedResources.setAudioInput punctual.sharedResources e

setAudioOutput :: Punctual -> WebAudioNode -> Effect Unit
setAudioOutput punctual n = SharedResources.setAudioOutput punctual.sharedResources n

setBrightness :: Punctual -> Number -> Effect Unit
setBrightness punctual b = SharedResources.setBrightness punctual.sharedResources b


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

updateAudioForZone :: Punctual -> Int -> Program -> Effect Unit
updateAudioForZone punctual z prog = do
  audioZones <- read punctual.audioZones
  case lookup z audioZones of
    Just x -> redefineAudioZone x prog
    Nothing -> do
      x <- newAudioZone punctual.sharedResources prog
      write (insert z x audioZones) punctual.audioZones
      
deleteAudioForZone :: Punctual -> Int -> Effect Unit
deleteAudioForZone punctual z = do
  audioZones <- read punctual.audioZones
  case lookup z audioZones of
    Just x -> do
      log "punctual DEBUG: delete audio zone"
      deleteAudioZone x
      write (delete z audioZones) punctual.audioZones
    Nothing -> pure unit

