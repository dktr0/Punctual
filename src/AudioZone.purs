module AudioZone where 

import Prelude (Unit,map,bind,(/=),pure,($),discard,otherwise,(+),(<$>),(<>),show,(==),unit,max,(>=),(-))
import Data.Maybe (Maybe(..))
import Data.List (List(..),zipWith,length)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Data.Traversable (sequence,traverse,traverse_)
import Data.Unfoldable1 (replicate1)

import SharedResources (SharedResources)
import Program (Program)
import AudioWorklet (AudioWorklet,runWorklet,stopWorklet)
import Action (Action)
import Signal (Signal)
import Output (Output(..))
import WebAudio (resumeWebAudioContext,currentTime)

type AudioZone = {
  sharedResources :: SharedResources,
  worklets :: Ref (List (Maybe AudioWorklet))
  }

newAudioZone :: SharedResources -> Program -> Effect AudioZone
newAudioZone sharedResources p = do
  let signals' = map justAudioSignals p.actions
  worklets' <- traverse (addOrRemoveWorklet sharedResources Nothing) signals'
  worklets <- new worklets'
  pure { sharedResources, worklets }

justAudioSignals :: Maybe Action -> Maybe Signal
justAudioSignals Nothing = Nothing
justAudioSignals (Just x) 
  | x.output /= Audio = Nothing
  | otherwise = Just x.signal

addOrRemoveWorklet :: SharedResources -> Maybe AudioWorklet -> Maybe Signal -> Effect (Maybe AudioWorklet)
addOrRemoveWorklet _ Nothing Nothing = pure Nothing
addOrRemoveWorklet sharedResources Nothing (Just sig) = do
  i <- read sharedResources.audioWorkletCount
  write (i+1) sharedResources.audioWorkletCount
  resumeWebAudioContext sharedResources.webAudioContext
  t <- currentTime sharedResources.webAudioContext
  Just <$> runWorklet sharedResources.webAudioContext sharedResources.audioOutputNode ("W" <> show i) sig (t+0.5) 5.0 -- fadeIn start and duration are placeholders, obviously...
addOrRemoveWorklet sharedResources (Just prevWorklet) Nothing = do
  t <- currentTime sharedResources.webAudioContext
  stopWorklet prevWorklet (t+0.5) 5.0 -- fadeIn start and duration are placeholders, obviously...
  pure Nothing
addOrRemoveWorklet sharedResources (Just prevWorklet) (Just sig) = do
  case prevWorklet.signal == sig of
    true -> pure (Just prevWorklet) -- no change in signal, maintain existing worklet
    false -> do
      _ <- addOrRemoveWorklet sharedResources (Just prevWorklet) Nothing -- remove previous worklet
      addOrRemoveWorklet sharedResources Nothing (Just sig) -- add new worklet


redefineAudioZone :: AudioZone -> Program -> Effect Unit
redefineAudioZone audioZone p = do
  worklets <- read audioZone.worklets
  let n = max (length worklets) (length p.actions)
  let worklets' = extendByPadding Nothing n worklets
  let actions' = extendByPadding Nothing n p.actions 
  worklets'' <- sequence $ zipWith (addOrRemoveWorklet audioZone.sharedResources) worklets' (map justAudioSignals actions')
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

