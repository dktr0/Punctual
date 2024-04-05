module W where

-- A monad and associated functions for generating the code of a WebAudio audio worklet.

import Prelude (Unit, bind, discard, map, max, min, mod, pure, show, ($), (*), (+), (-), (/), (/=), (<), (<$>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (&&))
import Control.Monad.State (State,get,put,runState,modify_)
import Data.List.NonEmpty (NonEmptyList,singleton,fromList,length,head,concat,zipWith,cons,drop)
import Data.Either (Either(..))
import Data.Foldable (intercalate,indexl)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (replicate1)
import Data.Number as Number
import Data.Ord (abs)

import NonEmptyList (multi,extendToEqualLength,combineM)
import Signal (Signal(..))
import MultiMode (MultiMode)


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

signalToFrame (Abs x) = unaryFunction abs "Math.abs" x
signalToFrame (Acos x) = unaryFunction Number.acos "Math.acos" x
signalToFrame (Acosh x) = unaryFunction acosh "Math.acosh" x
signalToFrame (Asin x) = unaryFunction Number.asin "Math.asin" x
signalToFrame (Asinh x) = unaryFunction asinh "Math.asinh" x
signalToFrame (Atan x) = unaryFunction Number.atan "Math.atan" x
signalToFrame (Atanh x) = unaryFunction atanh "Math.atanh" x
signalToFrame (Cbrt x) = unaryFunction cbrt "Math.cbrt" x
signalToFrame (Ceil x) = unaryFunction Number.ceil "Math.ceil" x
signalToFrame (Cos x) = unaryFunction Number.cos "Math.cos" x
signalToFrame (Cosh x) = unaryFunction cosh "Math.cosh" x
signalToFrame (Exp x) = unaryFunction Number.exp "Math.exp" x
signalToFrame (Floor x) = unaryFunction Number.floor "Math.floor" x
signalToFrame (Log x) = unaryFunction Number.log "Math.log" x
signalToFrame (Log2 x) = unaryFunction log2 "Math.log2" x
signalToFrame (Log10 x) = unaryFunction log10 "Math.log10" x
signalToFrame (Round x) = unaryFunction Number.round "Math.round" x
signalToFrame (Sign x) = unaryFunction Number.sign "Math.sign" x
signalToFrame (Sin x) = unaryFunction Number.sin "Math.sin" x
signalToFrame (Sinh x) = unaryFunction sinh "Math.sinh" x
signalToFrame (Sqrt x) = unaryFunction Number.sqrt "Math.sqrt" x
signalToFrame (Tan x) = unaryFunction Number.tan "Math.tan" x
signalToFrame (Tanh x) = unaryFunction tanh "Math.tanh" x
signalToFrame (Trunc x) = unaryFunction Number.trunc "Math.trunc" x
signalToFrame (MidiCps x) = signalToFrame x >>= traverse midicps
signalToFrame (CpsMidi x) = signalToFrame x >>= traverse cpsmidi
signalToFrame (DbAmp x) = signalToFrame x >>= traverse dbamp
signalToFrame (AmpDb x) = signalToFrame x >>= traverse ampdb
signalToFrame (Fract x) = signalToFrame x >>= traverse fract

signalToFrame (Sum mm x y) = binaryFunction (operator (+) "+") mm x y
signalToFrame (Difference mm x y) = binaryFunction (operator (-) "-") mm x y
signalToFrame (Product mm x y) = binaryFunction (operator (*) "*") mm x y
signalToFrame (Division mm x y) = binaryFunction safeDivision mm x y
signalToFrame (Mod mm x y) = binaryFunction (operator mod "%") mm x y
signalToFrame (Pow mm x y) = binaryFunction (operator Number.pow "**") mm x y
signalToFrame (Equal mm x y) = binaryFunction (operator (\a b -> if a == b then 1.0 else 0.0) "==") mm x y
signalToFrame (NotEqual mm x y) = binaryFunction (operator (\a b -> if a /= b then 1.0 else 0.0) "!=") mm x y
signalToFrame (GreaterThan mm x y) = binaryFunction (operator (\a b -> if a > b then 1.0 else 0.0) ">") mm x y
signalToFrame (GreaterThanEqual mm x y) = binaryFunction (operator (\a b -> if a >= b then 1.0 else 0.0) ">=") mm x y
signalToFrame (LessThan mm x y) = binaryFunction (operator (\a b -> if a < b then 1.0 else 0.0) "<") mm x y
signalToFrame (LessThanEqual mm x y) = binaryFunction (operator (\a b -> if a <= b then 1.0 else 0.0) "<=") mm x y
signalToFrame (Max mm x y) = binaryFunction (function max "Math.max") mm x y
signalToFrame (Min mm x y) = binaryFunction (function min "Math.min") mm x y
signalToFrame (Gate mm x y) = binaryFunction gate mm x y
signalToFrame (Clip mm x y) = binaryFunctionWithRange clip mm x y
signalToFrame (Between mm x y) = binaryFunctionWithRange between mm x y
signalToFrame (SmoothStep mm x y) = binaryFunctionWithRange smoothStep mm x y

{-
Seq Signal Signal |
-}

{-
  Mix MultiMode Signal Signal Signal |
  LinLin MultiMode Signal Signal Signal |
  LPF MultiMode Signal Signal Signal |
  HPF MultiMode Signal Signal Signal |
  BPF MultiMode Signal Signal Signal |
  Delay Number Signal Signal
-}

signalToFrame _ = pure $ singleton $ Left 0.0
  
unaryFunction :: (Number -> Number) -> String -> Signal -> W Frame
unaryFunction f name s = do
  xs <- signalToFrame s
  let g x = case x of
              Left x' -> pure $ Left $ f x'
              Right x' -> assign $ name <> "(" <> x' <> ")"
  traverse g xs

binaryFunction :: (Sample -> Sample -> W Sample) -> MultiMode -> Signal -> Signal -> W Frame
binaryFunction f mm x y = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  combineM mm f xs ys
  
binaryFunctionWithRange :: (Tuple Sample Sample -> Sample -> W Sample) -> MultiMode -> Signal -> Signal -> W Frame
binaryFunctionWithRange f mm x y = do
  xs <- frameToRanges <$> signalToFrame x
  ys <- signalToFrame y
  combineM mm f xs ys

frameToRanges :: Frame -> NonEmptyList (Tuple Sample Sample)
frameToRanges xs = 
  let a = head xs
      b = case indexl 1 xs of
            Just x -> x
            Nothing -> a
      h = Tuple a b
  in case fromList (drop 2 xs) of
       Nothing -> singleton h
       Just t -> h `cons` frameToRanges t
    

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

operator :: (Number -> Number -> Number) -> String -> Sample -> Sample -> W Sample
operator f _ (Left x) (Left y) = pure $ Left (f x y)
operator _ op x y = assign $ showSample x <> op <> showSample y

function :: (Number -> Number -> Number) -> String -> Sample -> Sample -> W Sample
function f _ (Left x) (Left y) = pure $ Left (f x y)
function _ f x y = assign $ f <> "(" <> showSample x <> "," <> showSample y <> ")"

safeDivision :: Sample -> Sample -> W Sample
safeDivision (Left x) (Left y) = pure $ Left $ _safeDivision x y
safeDivision x y = assign $ showSample y <> "!=0? " <> showSample x <> "/" <> showSample y <> " : 0"

_safeDivision :: Number -> Number -> Number
_safeDivision _ 0.0 = 0.0
_safeDivision x y = x/y

gate :: Sample -> Sample -> W Sample
gate (Left x) (Left y) = pure $ Left $ if abs y >= x then y else 0.0
gate x y = assign $ "Math.abs(" <> showSample y <> ")>=" <> showSample x <> "?" <> showSample y <> ":0"

clip :: Tuple Sample Sample -> Sample -> W Sample
clip (Tuple (Left e0) (Left e1)) (Left x) = pure $ Left $ _clip e0 e1 x
clip (Tuple e0 e1) x = assign $ "Math.max(" <> min' <> ",Math.min(" <> max' <> "," <> showSample x <> "))"
  where
    min' = "Math.min(" <> showSample e0 <> "," <> showSample e1 <> ")"
    max' = "Math.max(" <> showSample e0 <> "," <> showSample e1 <> ")"
    
_clip :: Number -> Number -> Number -> Number
_clip e0 e1 x = _uncheckedClip min' max' x
  where
    min' = min e0 e1
    max' = max e0 e1
    
_uncheckedClip :: Number -> Number -> Number -> Number
_uncheckedClip e0 e1 x = max e0 (min e1 x)

between :: Tuple Sample Sample -> Sample -> W Sample
between (Tuple (Left e0) (Left e1)) (Left x) = pure $ Left $ if x >= min' && x <= max' then 1.0 else 0.0
  where
    min' = min e0 e1
    max' = max e0 e1
between (Tuple e0 e1) x = assign $ "(" <> showSample x <> ">=" <> min' <> "&&" <> showSample x <> "<=" <> max' <> ")?1:0"
  where
    min' = "Math.min(" <> showSample e0 <> "," <> showSample e1 <> ")"
    max' = "Math.max(" <> showSample e0 <> "," <> showSample e1 <> ")"
    
smoothStep :: Tuple Sample Sample -> Sample -> W Sample
smoothStep (Tuple (Left e0) (Left e1)) (Left x) = pure $ Left $ t * t * (3.0 - (2.0 * t))
  where t = _uncheckedClip 0.0 1.0 $ _safeDivision (x - e0) (e1 - e0)
smoothStep (Tuple e0 e1) x = do
  let a = "(" <> showSample x <> "-" <> showSample e0 <> ")/(" <> showSample e1 <> "-" <> showSample e0 <> ")"
  t <- assign $ "Math.max(0.0,Math.min(1.0," <> a <> "))"
  let t' = showSample t
  assign $ t' <> "*" <> t' <> "*(3-(2*" <> t' <> "))"

foreign import acosh :: Number -> Number
foreign import asinh :: Number -> Number
foreign import atanh :: Number -> Number
foreign import cbrt :: Number -> Number
foreign import cosh :: Number -> Number
foreign import log2 :: Number -> Number
foreign import log10 :: Number -> Number
foreign import sinh :: Number -> Number
foreign import tanh :: Number -> Number

