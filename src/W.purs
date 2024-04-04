module W where

-- A monad and associated functions for generating the code of a WebAudio audio worklet.

import Prelude (Unit, bind, discard, map, pure, show, ($), (+), (<$>), (<<<), (<>), (>>=), (==), otherwise)
import Control.Monad.State (State,get,put,runState,modify_)
import Data.List.NonEmpty (NonEmptyList,singleton,fromList,length,head,concat,zipWith,cons)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (replicate1)

import NonEmptyList (multi,extendToEqualLength)
import Signal (Signal(..))
import MultiMode (MultiMode(..))


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

signalToFrame Cps = pure $ singleton $ Right "cps"

signalToFrame Time = pure $ singleton $ Right "time"

signalToFrame Beat = pure $ singleton $ Right "beat"

signalToFrame ETime = pure $ singleton $ Right "eTime"

signalToFrame EBeat = pure $ singleton $ Right "eBeat"

signalToFrame Rnd = singleton <$> assign "Math.random()*2-1"

-- AudioIn

signalToFrame (Bipolar x) = signalToFrame x >>= traverse bipolar
signalToFrame (Unipolar x) = signalToFrame x >>= traverse unipolar
signalToFrame (Osc f) = signalToFrame f >>= traverse osc

-- Tri Signal
-- Saw Signal
-- Sqr Signal
-- LFTri Signal
-- LFSaw Signal
-- LFSqr Signal

signalToFrame (Abs x) = unaryFunction "Math.abs" x
signalToFrame (Acos x) = unaryFunction "Math.acos" x
signalToFrame (Acosh x) = unaryFunction "Math.acosh" x
signalToFrame (Asin x) = unaryFunction "Math.asin" x
signalToFrame (Asinh x) = unaryFunction "Math.asinh" x
signalToFrame (Atan x) = unaryFunction "Math.atan" x
signalToFrame (Atanh x) = unaryFunction "Math.atanh" x
signalToFrame (Cbrt x) = unaryFunction "Math.cbrt" x
signalToFrame (Ceil x) = unaryFunction "Math.ceil" x
signalToFrame (Cos x) = unaryFunction "Math.cos" x
signalToFrame (Cosh x) = unaryFunction "Math.cosh" x
signalToFrame (Exp x) = unaryFunction "Math.exp" x
signalToFrame (Floor x) = unaryFunction "Math.floor" x
signalToFrame (Log x) = unaryFunction "Math.log" x
signalToFrame (Log2 x) = unaryFunction "Math.log2" x
signalToFrame (Log10 x) = unaryFunction "Math.log10" x
signalToFrame (Round x) = unaryFunction "Math.round" x
signalToFrame (Sign x) = unaryFunction "Math.sign" x
signalToFrame (Sin x) = unaryFunction "Math.sin" x
signalToFrame (Sinh x) = unaryFunction "Math.sinh" x
signalToFrame (Sqrt x) = unaryFunction "Math.sqrt" x
signalToFrame (Tan x) = unaryFunction "Math.tan" x
signalToFrame (Tanh x) = unaryFunction "Math.tanh" x
signalToFrame (Trunc x) = unaryFunction "Math.trunc" x
signalToFrame (MidiCps x) = signalToFrame x >>= traverse midicps
signalToFrame (CpsMidi x) = signalToFrame x >>= traverse cpsmidi
signalToFrame (DbAmp x) = signalToFrame x >>= traverse dbamp
signalToFrame (AmpDb x) = signalToFrame x >>= traverse ampdb
signalToFrame (Fract x) = signalToFrame x >>= traverse fract

signalToFrame (Sum mm x y) = binaryFunction sum mm x y

{-
  Difference MultiMode Signal Signal |
  Product MultiMode Signal Signal |
  Division MultiMode Signal Signal |
  Mod MultiMode Signal Signal |
  Pow MultiMode Signal Signal |
  Equal MultiMode Signal Signal |
  NotEqual MultiMode Signal Signal |
  GreaterThan MultiMode Signal Signal |
  GreaterThanEqual MultiMode Signal Signal |
  LessThan MultiMode Signal Signal |
  LessThanEqual MultiMode Signal Signal |
  Max MultiMode Signal Signal |
  Min MultiMode Signal Signal |
  Gate MultiMode Signal Signal |
  Clip MultiMode Signal Signal |
  Between MultiMode Signal Signal |
  SmoothStep MultiMode Signal Signal |
-}

{-
  Seq Signal Signal |
  Mix MultiMode Signal Signal Signal |
  ILine MultiMode Signal Signal Signal |
  Line MultiMode Signal Signal Signal |
  LinLin MultiMode Signal Signal Signal |
  LPF MultiMode Signal Signal Signal | HPF MultiMode Signal Signal Signal | BPF MultiMode Signal Signal Signal |
  Delay Number Signal Signal
-}

signalToFrame _ = pure $ singleton $ Left 0.0
  

unaryFunction :: String -> Signal -> W Frame
unaryFunction name s = do
  xs <- signalToFrame s
  traverse (\x -> assign $ name <> "(" <> showSample x <> ")") xs

binaryFunction :: (Sample -> Sample -> W Sample) -> MultiMode -> Signal -> Signal -> W Frame
binaryFunction f mm x y = do
  xs <- signalToFrame x -- NonEmptyList Sample
  ys <- signalToFrame y -- NonEmptyList Sample
  combineFrames mm f xs ys
  
combineFrames :: MultiMode -> (Sample -> Sample -> W Sample) -> Frame -> Frame -> W Frame
combineFrames Combinatorial = combineFramesCombinatorial
combineFrames Pairwise = combineFramesPairwise

combineFramesPairwise :: (Sample -> Sample -> W Sample) -> Frame -> Frame -> W Frame
combineFramesPairwise f xs ys
  | length xs == 1 = traverse (f (head xs)) ys
  | length ys == 1 = traverse (\x -> f x (head ys)) xs
  | otherwise = do -- extend xs and ys to equal length in channels
      let Tuple xs' ys' = extendToEqualLength xs ys
      sequence $ zipWith f xs' ys'

combineFramesCombinatorial :: (Sample -> Sample -> W Sample) -> Frame -> Frame -> W Frame
combineFramesCombinatorial f xs ys = do
  sequence $ do -- in NonEmptyList monad
    x <- xs
    y <- ys
    pure $ f x y


bipolar :: Sample -> W Sample
bipolar x = assign $ showSample x <> "*2-1"

unipolar :: Sample -> W Sample
unipolar x = assign $ showSample x <> "*0.5+0.5"

osc :: Sample -> W Sample
osc x = assign $ "Math.sin(t * 2.0 * Math.PI * " <> showSample x <> ")"

midicps :: Sample -> W Sample
midicps x = assign $ "440 * (2 ** ((" <> showSample x <> "-69)/12))"

cpsmidi :: Sample -> W Sample
cpsmidi x = assign $ "69 + (12 * (Math.log2(" <> showSample x <> "/440)))"

dbamp :: Sample -> W Sample
dbamp x = assign $ "10 ** (" <> showSample x <> "/20)"

ampdb :: Sample -> W Sample
ampdb x = assign $ "20 * Math.log10(" <> showSample x <> ")"

fract :: Sample -> W Sample
fract x = assign $ showSample x <> "%1"

sum :: Sample -> Sample -> W Sample
sum (Left x) (Left y) = pure $ Left (x+y)
sum x y = assign $ showSample x <> "+" <> showSample y
