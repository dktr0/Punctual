module AudioZone where 

import Prelude (Unit, bind, discard, map, max, otherwise, pure, show, unit, ($), (&&), (+), (-), (<$>), (<>), (==), (>=), (>>>), (>>=))
import Data.Maybe (Maybe(..))
import Data.List (List(..),zipWith,length,catMaybes)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Data.Foldable (fold)
import Data.Traversable (sequence,traverse,traverse_)
import Data.Unfoldable1 (replicate1)
import Data.DateTime (DateTime)
import Data.Tuple (Tuple(..))
import Data.Nullable (null,notNull)
import Effect.Class.Console (log)
import Data.Rational (toNumber)
import Data.Tempo (origin)

import SharedResources (SharedResources, activateAudioInput, getOutputChannelCount, dateTimeToAudioTime)
import Program (Program)
import AudioWorklet (AudioWorklet,runWorklet,stopWorklet,updateWorklet)
import Action (Action,actionTimesAsAudioTime,actionHasAudioInput,output,matrix)
import Output (isAudioOutput)
import WebAudio (resumeWebAudioContext,currentTime)

type AudioZone = {
  sharedResources :: SharedResources,
  evalTime :: Ref DateTime,
  worklets :: Ref (List (Maybe AudioWorklet))
  }

newAudioZone :: SharedResources -> Program -> Effect AudioZone
newAudioZone sharedResources p = do
  resumeWebAudioContext sharedResources.webAudioContext
  clockDiff <- read sharedResources.clockDiff
  -- log $ "newAudioZone clockDiff=" <> show clockDiff
  let actions' = map justAudioActions p.actions
  evalTime <- new p.evalTime
  worklets' <- traverse (addOrRemoveWorklet sharedResources p.evalTime clockDiff Nothing) actions'
  worklets <- new worklets'
  pure { sharedResources, evalTime, worklets }

justAudioActions :: Maybe Action -> Maybe Action
justAudioActions Nothing = Nothing
justAudioActions (Just x)
  | isAudioOutput (output x) = Just x
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
  case (prevWorklet.matrix == (matrix action) && prevWorklet.output == (output action)) of
    true -> pure (Just prevWorklet) -- no change in signal/output, maintain existing worklet
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
  runWorklet sharedResources.webAudioContext nAin sharedResources.internalAudioOutputNode ("W" <> show i) (matrix action) maxChnls (output action) t1 (t2-t1)

calculateAudioZoneInfo :: AudioZone -> Effect String
calculateAudioZoneInfo z = do
  ws <- catMaybes <$> read z.worklets
  pure $ fold $ map (_.code >>> (_ <> "\n")) ws

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
  clockDiff <- read audioZone.sharedResources.clockDiff
  -- log $ "redefineAudioZone clockDiff=" <> show clockDiff
  worklets'' <- sequence $ zipWith (addOrRemoveWorklet audioZone.sharedResources p.evalTime clockDiff) worklets' (map justAudioActions actions')
  write worklets'' audioZone.worklets
  write p.evalTime audioZone.evalTime

extendByPadding :: forall a. a -> Int -> List a -> List a
extendByPadding a n xs
  | length xs >= n = xs
  | otherwise = xs <> replicate1 (n - length xs) a

updateAudioZone :: AudioZone -> Effect Unit
updateAudioZone audioZone = do
  tempo <- read audioZone.sharedResources.tempo
  let cps = toNumber tempo.freq
  originAudio <- dateTimeToAudioTime audioZone.sharedResources $ origin tempo
  evalTimeAudio <- read audioZone.evalTime >>= dateTimeToAudioTime audioZone.sharedResources
  worklets <- catMaybes <$> read audioZone.worklets
  traverse_ (updateWorklet cps originAudio evalTimeAudio) worklets
  
deleteAudioZone :: AudioZone -> Effect Unit
deleteAudioZone audioZone = do
  worklets <- read audioZone.worklets
  t <- currentTime audioZone.sharedResources.webAudioContext
  traverse_ (stopMaybeWorklet (t+0.25) 0.1) worklets
  write Nil audioZone.worklets

stopMaybeWorklet :: Number -> Number -> Maybe AudioWorklet -> Effect Unit
stopMaybeWorklet _ _ Nothing = pure unit
stopMaybeWorklet fInStart fInDur (Just w) = stopWorklet w fInStart fInDur

