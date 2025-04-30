module SharedResources where

-- management of all resources (tempo, images, videos, webcam, audio analysis, audio files, etc)
-- that are shared across different Punctual zones (and between different WebGL contexts, so no WebGL types)

import Prelude (Unit, bind, discard, pure, unit, ($), (+), (-), (/), (<$>), (<<<), (<=), (>>=))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref,new,read,write)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo, newTempo)
import Data.Map (Map, empty, lookup, insert)
import Data.Monoid.Disj (Disj)
import Data.Newtype (unwrap)
import Effect.Now (now)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (unInstant,fromDateTime)
import Data.List.NonEmpty as NonEmptyList
import Data.Semigroup.Foldable (foldl1)

import WebGLCanvas (WebGLCanvas, WebGLContext, WebGLTexture)
import WebAudio
import AudioAnalyser (AudioAnalyser,newAudioAnalyser,updateAnalyser,setSourceNode)
import Value (LibraryCache) 

type URL = String

type SharedResources = {
  tempo :: Ref Tempo,
  mWebcamElementRef :: Ref (Maybe WebcamElement),
  images :: Ref (Map URL Image),
  videos :: Ref (Map URL Video),
  gdms :: Ref (Map String GDM),
  libraries :: LibraryCache,
  webAudioContext :: WebAudioContext,
  clockDiff :: Ref Number,
  clockDiffList :: Ref (NonEmptyList.NonEmptyList Number),
  audioInputGetter :: Ref (Effect WebAudioNode), -- client environment provides function to get external audio input node
  mExternalAudioInputNode :: Ref (Maybe WebAudioNode), -- cached audio input node returned by function above (dropped/Nothing when audio input not active)
  externalAudioOutputNode :: Ref WebAudioNode, -- cached audio output node 
  internalAudioInputNode :: WebAudioNode, -- a gain node that always exists, any external input will be connected to it
  internalAudioOutputNode :: WebAudioNode, -- a gain node that always exists, it will be connected to any external output
  inputAnalyser :: AudioAnalyser,
  outputAnalyser :: AudioAnalyser,
  audioWorkletCount :: Ref Int,
  brightness :: Ref Number,
  outputChannelCount :: Ref Int
  }

newSharedResources :: Maybe WebAudioContext -> Effect SharedResources
newSharedResources mWebAudioContext = do
  tempo <- newTempo (1 % 1) >>= new
  mWebcamElementRef <- new Nothing
  images <- new empty
  videos <- new empty
  gdms <- new empty
  libraries <- new empty
  webAudioContext <- case mWebAudioContext of
                       Nothing -> defaultWebAudioContext
                       Just x -> pure x
  immediateClockDiff <- _getImmediateClockDiff webAudioContext
  clockDiff <- new immediateClockDiff
  clockDiffList <- new $ NonEmptyList.singleton immediateClockDiff
  audioInputGetter <- new $ _defaultAudioInputNode webAudioContext
  mExternalAudioInputNode <- new Nothing
  defaultAudioOutputNode <- destination webAudioContext
  externalAudioOutputNode <- new defaultAudioOutputNode
  internalAudioInputNode <- gainNode webAudioContext 1.0
  internalAudioOutputNode <- gainNode webAudioContext 1.0
  connect internalAudioOutputNode defaultAudioOutputNode
  inputAnalyser <- newAudioAnalyser webAudioContext internalAudioInputNode
  outputAnalyser <- newAudioAnalyser webAudioContext internalAudioOutputNode
  audioWorkletCount <- new 0
  brightness <- new 1.0
  outputChannelCount <- new 2
  pure {
    tempo,
    mWebcamElementRef,
    images,
    videos,
    gdms,
    libraries,
    webAudioContext,
    clockDiff,
    clockDiffList,
    audioInputGetter,
    mExternalAudioInputNode,
    externalAudioOutputNode,
    internalAudioInputNode,
    internalAudioOutputNode,
    inputAnalyser,
    outputAnalyser,
    audioWorkletCount,
    brightness,
    outputChannelCount
    }
    
updateAudioInputAndAnalysers :: SharedResources -> forall r. { ain::Disj Boolean, ifft::Disj Boolean, ilo::Disj Boolean, imid::Disj Boolean, ihi::Disj Boolean, fft::Disj Boolean, lo::Disj Boolean, mid::Disj Boolean, hi::Disj Boolean | r } -> Effect Unit
updateAudioInputAndAnalysers sr needs = do
  case unwrap needs.ain of
    true -> activateAudioInput sr
    false -> disactivateAudioInput sr
  updateAnalyser sr.inputAnalyser { fft: needs.ifft, lo: needs.ilo, mid: needs.imid, hi: needs.ihi }
  updateAnalyser sr.outputAnalyser needs

-- to convert audio to POSIX, add clockdiff; to convert POSIX to audio, subtract clockdiff
_getImmediateClockDiff :: WebAudioContext -> Effect Number
_getImmediateClockDiff webAudioContext = do
  tAudio <- currentTime webAudioContext
  tNow <- ((_/1000.0) <<< unwrap <<< unInstant) <$> now -- :: Number (in POSIX 1970 seconds)
  pure $ tNow - tAudio 

updateClockDiff :: SharedResources -> Effect Unit
updateClockDiff sharedResources = do
  immediateClockDiff <- _getImmediateClockDiff sharedResources.webAudioContext
  clockDiffList <- read sharedResources.clockDiffList
  let newList = case NonEmptyList.length clockDiffList <= 9 of
                  true -> NonEmptyList.cons immediateClockDiff clockDiffList
                  false -> NonEmptyList.cons' immediateClockDiff (NonEmptyList.init clockDiffList)
  write newList sharedResources.clockDiffList
  let avg = (foldl1 (+) newList) / 10.0
  write avg sharedResources.clockDiff
  -- log $ "updateClockDiff " <> show avg


dateTimeToAudioTime :: SharedResources -> DateTime -> Effect Number
dateTimeToAudioTime sr dt = do
  clockDiff <- read sr.clockDiff
  let tPosix = (unwrap $ unInstant $ fromDateTime dt) / 1000.0
  pure $ tPosix - clockDiff


-- Tempo

setTempo :: SharedResources -> Tempo -> Effect Unit
setTempo sr t = write t sr.tempo

getTempo :: SharedResources -> Effect Tempo
getTempo sr = read sr.tempo

-- Webcam

foreign import data WebcamElement :: Type

setWebcamActive :: SharedResources -> Boolean -> Effect Unit
setWebcamActive sr true = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just _ -> pure unit
    Nothing -> do
      log "punctual: activating webcam"
      e <- _newWebcamElement
      write (Just e) sr.mWebcamElementRef   
setWebcamActive sr false = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just e -> do
      log "punctual: disactivating webcam"
      _stopWebcamElement e
      write Nothing sr.mWebcamElementRef
    Nothing -> pure unit

foreign import _newWebcamElement :: Effect WebcamElement

foreign import _stopWebcamElement :: WebcamElement -> Effect Unit

foreign import _updateWebcamTexture :: WebGLContext -> WebGLTexture -> WebcamElement -> Effect Unit

updateWebcamTexture :: SharedResources -> WebGLCanvas -> Effect Unit
updateWebcamTexture sr glc = do
  mWebcamElement <- read sr.mWebcamElementRef
  case mWebcamElement of
    Just e -> _updateWebcamTexture glc.gl glc.webcamTexture e
    Nothing -> pure unit


-- Images

foreign import data Image :: Type

getImage :: SharedResources -> String -> Effect (Maybe Image)
getImage sr url = do
  images <- read sr.images
  case lookup url images of
    Nothing -> do
      i <- _newImage url
      write (insert url i images) sr.images
      pure Nothing
    Just i -> do
      loaded <- _imageIsLoaded i
      case loaded of
        true -> pure $ Just i
        false -> pure Nothing

foreign import _newImage :: String -> Effect Image

foreign import _imageIsLoaded :: Image -> Effect Boolean
  
  
-- Videos

foreign import data Video :: Type

getVideo :: SharedResources -> String -> Effect (Maybe Video)
getVideo sr url = do
  videos <- read sr.videos
  case lookup url videos of
    Nothing -> do
      v <- _newVideo url
      write (insert url v videos) sr.videos
      pure Nothing
    Just v -> do
      isPlaying <- _videoIsPlaying v
      case isPlaying of
        true -> pure $ Just v
        false -> pure Nothing

foreign import _newVideo :: String -> Effect Video

foreign import _videoIsPlaying :: Video -> Effect Boolean


-- GDM (i.e. window/display capture via getDisplayMedia)

foreign import data GDM :: Type

getGDM :: SharedResources -> String -> Effect (Maybe GDM)
getGDM sr x = do
  gdms <- read sr.gdms
  case lookup x gdms of
    Nothing -> do
      gdm <- _newGDM
      write (insert x gdm gdms) sr.gdms
      pure Nothing
    Just gdm -> do
      isPlaying <- _gdmIsPlaying gdm
      case isPlaying of
        true -> pure $ Just gdm
        false -> pure Nothing

foreign import _newGDM :: Effect GDM

foreign import _gdmIsPlaying :: GDM -> Effect Boolean


-- Audio Input and Output

-- basically called externally to tell Punctual how to acquire audio input other than the default microphone (when necessary to acquire audio input)
setAudioInput :: SharedResources -> Effect WebAudioNode -> Effect Unit
setAudioInput sr newAudioInputGetter = do
  mExternalAudioInputNode <- read sr.mExternalAudioInputNode
  case mExternalAudioInputNode of
    Nothing -> pure unit -- no active connection to audio input
    Just prevExternalAudioInputNode -> do -- if there is an active connection to audio input, need to disconnect/reconnect
      disconnect prevExternalAudioInputNode sr.internalAudioInputNode
      newExternalAudioInputNode <- newAudioInputGetter
      connect newExternalAudioInputNode sr.internalAudioInputNode
      write (Just newExternalAudioInputNode) sr.mExternalAudioInputNode
  write newAudioInputGetter sr.audioInputGetter

-- to be called internally whenever something requires audio input, to acquire/connect them if necessary
activateAudioInput :: SharedResources -> Effect Unit
activateAudioInput sr = do
  mExternalAudioInputNode <- read sr.mExternalAudioInputNode
  case mExternalAudioInputNode of
    Nothing -> do
      audioInputGetter <- read sr.audioInputGetter
      newExternalAudioInputNode <- audioInputGetter
      connect newExternalAudioInputNode sr.internalAudioInputNode
      write (Just newExternalAudioInputNode) sr.mExternalAudioInputNode
      log "punctual audio input activated"
    Just _ -> pure unit -- already activated

-- to be called internally whenever nothing requires audio input, to release/disconnect them if necessary
disactivateAudioInput :: SharedResources -> Effect Unit
disactivateAudioInput sr = do
  mExternalAudioInputNode <- read sr.mExternalAudioInputNode
  case mExternalAudioInputNode of
    Nothing -> pure unit -- already disactivated
    Just externalAudioInputNode -> do
      disconnect externalAudioInputNode sr.internalAudioInputNode
      write Nothing sr.mExternalAudioInputNode
      log "punctual audio input disactivated"

-- called externally to use audio output other than the audio context destination
-- note: cannot be set to destination of audio context (since that can't be a source for audio analysis)
setAudioOutput :: SharedResources -> WebAudioNode -> Effect Unit
setAudioOutput sr newExternalAudioOutputNode = do
  externalAudioOutputNode <- read sr.externalAudioOutputNode
  disconnect sr.internalAudioOutputNode externalAudioOutputNode
  connect sr.internalAudioOutputNode newExternalAudioOutputNode
  write newExternalAudioOutputNode sr.externalAudioOutputNode
  setSourceNode sr.outputAnalyser newExternalAudioOutputNode


-- Brightness
-- TODO: make sure brightness is used in WebGL implementation

setBrightness :: SharedResources -> Number -> Effect Unit
setBrightness sr b = write b sr.brightness

getBrightness :: SharedResources -> Effect Number
getBrightness sr = read sr.brightness


-- number of audio output channels

setOutputChannelCount :: SharedResources -> Int -> Effect Unit
setOutputChannelCount sr b = write b sr.outputChannelCount

getOutputChannelCount :: SharedResources -> Effect Int
getOutputChannelCount sr = read sr.outputChannelCount