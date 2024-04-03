module W where

-- A monad and associated functions for generating the code of a WebAudio audio worklet.

import Prelude (Unit, bind, discard, map, pure, show, ($), (+), (<$>), (<<<), (<>), (>>=))
import Control.Monad.State (State,get,put,runState,modify_)
import Data.List.NonEmpty (NonEmptyList,singleton,fromList,length,head,concat,zipWith,cons)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (replicate1)

import NonEmptyList (multi,extendToEqualLength)
import Signal (Signal(..))


type W = State WState

type WState = {
  allocation :: Int,
  code :: String
  }

runW :: forall a. W a -> Tuple a WState
runW x = runState x { allocation: 0, code: "" }

type Sample = Either Number String -- lefts are precalculated constants, rights are either references to built-in constants or variables (eg. Math.PI) or to items from preallocated memory heap

showSample :: Sample -> String
showSample (Left x) = show x
showSample (Right x) = x

assign :: String -> W Sample
assign x = do
  n <- allocate
  let m = "m[" <> show n <> "]"
  writeCode $ m <> "=" <> x <> ";\n"
  pure $ Right m

allocate :: W Int
allocate = do
  s <- get
  put $ s { allocation = s.allocation + 1 }
  pure s.allocation
  
writeCode :: String -> W Unit
writeCode x = modify_ $ \s -> s { code = s.code <> x } 
  

type Frame = NonEmptyList Sample

signalToFrame :: Signal -> W Frame

signalToFrame (Constant x) = pure $ singleton $ Left x

signalToFrame (SignalList xs) = 
  case fromList xs of
    Nothing -> pure $ singleton $ Left 0.0
    Just xs' -> do
      case length xs' of
        1 -> signalToFrame (head xs')
        _ -> do
          xs'' <- traverse signalToFrame xs' -- :: NonEmptyList Frame == NonEmptyList (NonEmptyList Sample)
          pure $ concat $ multi xs''

signalToFrame (Append x y) = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  pure $ xs <> ys
  
signalToFrame (Zip x y) = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  let Tuple xs' ys' = extendToEqualLength xs ys
  pure $ concat $ zipWith (\anX anY -> anX `cons` singleton anY ) xs' ys'

signalToFrame (Mono x) = do
  xs <- signalToFrame x
  y <- assign $ intercalate "+" $ map showSample xs
  pure $ singleton $ y

signalToFrame (Rep n x) = (concat <<< replicate1 n) <$> signalToFrame x

signalToFrame Pi = pure $ singleton $ Right "Math.PI"

-- Cps 
-- Time
-- Beat
-- EBeat
-- ETime
-- Rnd
-- AudioIn
-- ...

signalToFrame (Osc f) = signalToFrame f >>= traverse osc

{-  
signalToFrame (Sum mm x y) = do
  x' <- signalToFrame x 
  y' <- signalToFrame y 
  ... TODO: some function that combines (taking account of combinatorial vs. pairwise) two n-channel Frames through a provided sum function of type Sample -> Sample -> W Sample
-}

signalToFrame _ = pure $ singleton $ Left 0.0


osc :: Sample -> W Sample
osc x = assign $ "Math.sin(t * 2.0 * Math.PI * " <> showSample x <> ")"
  
sum :: Sample -> Sample -> W Sample
sum (Left x) (Left y) = pure $ Left (x+y)
sum x y = assign $ showSample x <> "+" <> showSample y


