module AudioZone where 

import Prelude (Unit,map,bind,pure,($),discard,otherwise,(+),(<$>),(<>),show,(==),unit,max,(>=),(-),(<<<),(/),(>>>))
import Data.Maybe (Maybe(..))
import Data.List (List(..),zipWith,length,catMaybes)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Data.Foldable (fold)
import Data.Traversable (sequence,traverse,traverse_)
import Data.Unfoldable1 (replicate1)
import Data.DateTime (DateTime)
import Data.Tuple (Tuple(..))
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Data.Nullable (null,notNull)
import Effect.Class.Console (log)

import SharedResources (SharedResources, activateAudioInput, getOutputChannelCount)
import Program (Program)
import AudioWorklet (AudioWorklet,runWorklet,stopWorklet)
import Action (Action,actionTimesAsAudioTime,actionHasAudioInput)
import Output (isAudioOutput,audioOutputChannels,audioOutputOffset)
import WebAudio (resumeWebAudioContext,currentTime)

type AudioZone = {
  sharedResources :: SharedResources,
  worklets :: Ref (List (Maybe AudioWorklet)),
  clockDiff :: Ref Number
  }

newAudioZone :: SharedResources -> Program -> Effect AudioZone
newAudioZone sharedResources p = do
  resumeWebAudioContext sharedResources.webAudioContext
  clockDiff' <- calculateClockDiff sharedResources
  let actions' = map justAudioActions p.actions
  worklets' <- traverse (addOrRemoveWorklet sharedResources p.evalTime clockDiff' Nothing) actions'
  worklets <- new worklets'
  clockDiff <- new clockDiff'
  pure { sharedResources, worklets, clockDiff }

justAudioActions :: Maybe Action -> Maybe Action
justAudioActions Nothing = Nothing
justAudioActions (Just x)
  | isAudioOutput x.output = Just x
  | otherwise = Nothing

addOrRemoveWorklet :: SharedResources -> DateTime -> Number -> Maybe AudioWorklet -> Maybe Action -> Effect (Maybe AudioWorklet)
addOrRemoveWorklet _ _ _ Nothing Nothing = pure Nothing
addOrRemoveWorklet sharedResources evalTime clockDiff Nothing (Just action) = do
  tempo <- read sharedResources.tempo
  let Tuple t1 t2 = actionTimesAsAudioTime tempo evalTime clockDiff action
  Just <$> addWorklet sharedResources action t1 t2
addOrRemoveWorklet sharedResources _ _ (Just prevWorklet) Nothing = do
  t <- currentTime sharedResources.webAudioContext
  stopWorklet prevWorklet (t+0.25) 0.1
  pure Nothing
addOrRemoveWorklet sharedResources evalTime clockDiff (Just prevWorklet) (Just action) = do
  case prevWorklet.signal == action.signal of
    true -> pure (Just prevWorklet) -- no change in signal, maintain existing worklet
    false -> do
      tempo <- read sharedResources.tempo
      let Tuple t1 t2 = actionTimesAsAudioTime tempo evalTime clockDiff action
      stopWorklet prevWorklet t1 (t2-t1)
      Just <$> addWorklet sharedResources action t1 t2
      
-- TODO: need to disactivateAudioInput when NO worklet requires it any more
addWorklet :: SharedResources -> Action -> Number -> Number -> Effect AudioWorklet
addWorklet sharedResources action t1 t2 = do
  i <- read sharedResources.audioWorkletCount
  write (i+1) sharedResources.audioWorkletCount
  nAin <- case actionHasAudioInput action of
            true -> do
              log "worklet has audio input"
              activateAudioInput sharedResources
              notNull <$> pure sharedResources.internalAudioInputNode
            false -> do
              log "worklet does not have audio input"
              pure null
  maxChnls <- getOutputChannelCount sharedResources
  let nOutputChnls = audioOutputChannels maxChnls action.output
  let channelOffset = audioOutputOffset action.output
  runWorklet sharedResources.webAudioContext nAin sharedResources.internalAudioOutputNode ("W" <> show i) action.signal nOutputChnls channelOffset t1 (t2-t1)

calculateAudioZoneInfo :: AudioZone -> Effect String
calculateAudioZoneInfo z = do
  ws <- catMaybes <$> read z.worklets
  pure $ fold $ map (_.code >>> (_ <> "\n")) ws

-- to convert audio to POSIX, add clockdiff; to convert POSIX to audio, subtract clockdiff
calculateClockDiff :: SharedResources -> Effect Number
calculateClockDiff sharedResources = do
  tAudio <- currentTime sharedResources.webAudioContext
  tNow <- ((_/1000.0) <<< unwrap <<< unInstant) <$> now -- :: Number (in POSIX 1970 seconds)
  pure $ tNow - tAudio 
  
{- calculateEvalTimeAudio :: SharedResources -> DateTime -> Effect Number
calculateEvalTimeAudio sharedResources evalTime = do
  tAudio <- currentTime sharedResources.webAudioContext
  tNow <- ((_/1000.0) <<< unwrap <<< unInstant) <$> now -- :: Number (in POSIX 1970 seconds)
  let clockDiff = tNow - tAudio -- to convert audio to POSIX, add clockdiff; to convert POSIX to audio, subtract clockdiff
  let evalTime' = (unInstant $ fromDateTime evalTime) / 1000.0 -- :: Number (in POSIX 1970 seconds)
  pure $ evalTime' - clockDiff -}
  
redefineAudioZone :: AudioZone -> Program -> Effect Unit
redefineAudioZone audioZone p = do
  worklets <- read audioZone.worklets
  let n = max (length worklets) (length p.actions)
  let worklets' = extendByPadding Nothing n worklets
  let actions' = extendByPadding Nothing n p.actions 
  clockDiff <- read audioZone.clockDiff
  worklets'' <- sequence $ zipWith (addOrRemoveWorklet audioZone.sharedResources p.evalTime clockDiff) worklets' (map justAudioActions actions')
  write worklets'' audioZone.worklets

extendByPadding :: forall a. a -> Int -> List a -> List a
extendByPadding a n xs
  | length xs >= n = xs
  | otherwise = xs <> replicate1 (n - length xs) a

-- renderAudioZone :: AudioZone -> Effect Unit
-- possibly nothing to do now, but soon it will be responsible for sync updates


deleteAudioZone :: AudioZone -> Effect Unit
deleteAudioZone audioZone = do
  worklets <- read audioZone.worklets
  t <- currentTime audioZone.sharedResources.webAudioContext
  traverse_ (stopMaybeWorklet (t+0.25) 0.1) worklets
  write Nil audioZone.worklets

stopMaybeWorklet :: Number -> Number -> Maybe AudioWorklet -> Effect Unit
stopMaybeWorklet _ _ Nothing = pure unit
stopMaybeWorklet fInStart fInDur (Just w) = stopWorklet w fInStart fInDur

