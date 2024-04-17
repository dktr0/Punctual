module W where

-- A monad and associated functions for generating the code of a WebAudio audio worklet.

import Prelude (Unit, bind, discard, map, pure, show, ($), (*), (+), (-), (/), (/=), (<), (<$>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (&&))
import Prelude as Prelude
import Control.Monad.State (State,get,put,runState,modify_)
import Data.List.NonEmpty (NonEmptyList,singleton,fromList,length,head,concat,zipWith,cons,drop)
import Data.Either (Either(..))
import Data.Foldable (intercalate,indexl)
import Data.Traversable (traverse,for,sequence)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (replicate1,iterateN)
import Data.Number as Number
import Data.Ord as Ord
import Data.Int (toNumber)

-- import NonEmptyList (multi,extendToEqualLength,combine,combine3)
import Signal (Signal(..))
import MultiMode (MultiMode)
import Multi (Multi,flatten,simplify,zip,rep,combine)


type W = State WState

type WState = {
  allocation :: Int,
  code :: String,
  time :: Sample,
  beat :: Sample,
  etime :: Sample,
  ebeat :: Sample
  }

runW :: forall a. W a -> Tuple a WState
runW x = runState x { allocation: 0, code: "", time: Right "time", beat: Right "beat", etime: Right "eTime", ebeat: Right "eBeat" }

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
  

type Frame = Multi Sample

signalToFrame :: Signal -> W Frame

signalToFrame (Constant x) = pure $ pure $ Left x

signalToFrame (SignalList xs) = 
  case fromList xs of
    Nothing -> pure $ pure $ Left 0.0
    Just xs' -> simplify <$> traverse signalToFrame xs' -- :: NonEmptyList Frame == NonEmptyList (Multi Sample)

signalToFrame (Append x y) = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  pure $ xs <> ys
  
signalToFrame (Zip x y) = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  pure $ zip xs ys
  
signalToFrame (Mono x) = pure <$> (signalToFrame x >>= sum)

signalToFrame (Rep n x) = rep n <$> signalToFrame x

signalToFrame Pi = pure $ pure $ Right "Math.PI"
signalToFrame Cps = pure $ pure $ Right "cps"
signalToFrame Time = pure $ pure $ Right "time"
signalToFrame Beat = pure $ pure $ Right "beat"
signalToFrame ETime = pure $ pure $ Right "eTime"
signalToFrame EBeat = pure $ pure $ Right "eBeat"
signalToFrame Rnd = pure <$> assign "Math.random()*2-1"

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

signalToFrame (Abs x) = signalToFrame x >>= traverse abs
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

{-
signalToFrame (Early x z) = do
  xs <- signalToFrame x
  s <- get  
  xs' <- for xs $ \y -> do
    time <- add s.time y
    yBeats <- product y (Right "cps")
    beat <- add s.beat yBeats
    etime <- add s.etime y
    ebeat <- add s.ebeat yBeats
    pure { time, beat, etime, ebeat }
  withAlteredTime xs' $ signalToFrame z

signalToFrame (Slow x z) = do
  xs <- signalToFrame x
  s <- get
  xs' <- for xs $ \y -> do
    time <- division s.time y
    beat <- division s.beat y
    etime <- division s.etime y
    ebeat <- division s.ebeat y
    pure { time, beat, etime, ebeat }  
  withAlteredTime xs' $ signalToFrame z
-}

signalToFrame (Addition mm x y) = binaryFunction add mm x y
signalToFrame (Difference mm x y) = binaryFunction difference mm x y
signalToFrame (Product mm x y) = binaryFunction product mm x y
signalToFrame (Division mm x y) = binaryFunction division mm x y
signalToFrame (Mod mm x y) = binaryFunction mod mm x y
signalToFrame (Pow mm x y) = binaryFunction pow mm x y
signalToFrame (Equal mm x y) = binaryFunction equal mm x y
signalToFrame (NotEqual mm x y) = binaryFunction notEqual mm x y
signalToFrame (GreaterThan mm x y) = binaryFunction greaterThan mm x y
signalToFrame (GreaterThanEqual mm x y) = binaryFunction greaterThanEqual mm x y
signalToFrame (LessThan mm x y) = binaryFunction lessThan mm x y
signalToFrame (LessThanEqual mm x y) = binaryFunction lessThanEqual mm x y
signalToFrame (Max mm x y) = binaryFunction max mm x y
signalToFrame (Min mm x y) = binaryFunction min mm x y
signalToFrame (Gate mm x y) = binaryFunction gate mm x y
-- signalToFrame (Clip mm x y) = binaryFunctionWithRange clip mm x y
-- signalToFrame (Between mm x y) = binaryFunctionWithRange between mm x y
-- signalToFrame (SmoothStep mm x y) = binaryFunctionWithRange smoothStep mm x y

{-
signalToFrame (Seq steps) = do
  steps' <- signalToFrame steps
  case length steps' of
    1 -> pure steps'
    _ -> do
      b <- (_.beat <$> get) >>= fract
      singleton <$> seq steps' b  

signalToFrame (Mix mm x y a) = do
  x' <- signalToFrame x
  y' <- signalToFrame y
  let Tuple x'' y'' = extendToEqualLength x' y'
  let xys = zip x'' y''
  a' <- signalToFrame a
  sequence $ combine mm mix xys a'
  
signalToFrame (LinLin mm r1 r2 x) = do
  r1s <- frameToRanges <$> signalToFrame r1
  r2s <- frameToRanges <$> signalToFrame r2
  xs <- signalToFrame x
  sequence $ combine3 mm linlin r1s r2s xs
-}

{-  LPF MultiMode Signal Signal Signal |
  HPF MultiMode Signal Signal Signal |
  BPF MultiMode Signal Signal Signal |
  Delay Number Signal Signal
-}

signalToFrame _ = pure $ pure $ Left 0.0

{-
withAlteredTime :: NonEmptyList { time :: Sample, beat :: Sample, etime :: Sample, ebeat :: Sample }  -> W Frame -> W Frame
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs
-}

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
  sequence $ combine f mm xs ys

{-
binaryFunctionWithRange :: (Tuple Sample Sample -> Sample -> W Sample) -> MultiMode -> Signal -> Signal -> W Frame
binaryFunctionWithRange f mm x y = do
  xs <- frameToRanges <$> signalToFrame x
  ys <- signalToFrame y
  sequence $ combine mm f xs ys

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
-}


bipolar :: Sample -> W Sample
bipolar x = assign $ showSample x <> "*2-1"

unipolar :: Sample -> W Sample
unipolar x = assign $ showSample x <> "*0.5+0.5"

osc :: Sample -> W Sample
osc x = do
  t <- _.time <$> get
  assign $ "Math.sin(" <> showSample t <> " * 2.0 * Math.PI * " <> showSample x <> ")"

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

add :: Sample -> Sample -> W Sample
add (Left 0.0) x = pure x
add x (Left 0.0) = pure x
add x y = operator (+) "+" x y

difference :: Sample -> Sample -> W Sample
difference x (Left 0.0) = pure x
difference x y = operator (-) "-" x y

product :: Sample -> Sample -> W Sample
product (Left 0.0) _ = pure $ Left 0.0
product (Left 1.0) x = pure x
product _ (Left 0.0) = pure $ Left 0.0
product x (Left 1.0) = pure x
product x y = operator (*) "*" x y

division :: Sample -> Sample -> W Sample
division (Left x) (Left y) = pure $ Left $ _division x y
division (Left 0.0) _ = pure $ Left 0.0
division _ (Left 0.0) = pure $ Left 0.0
division x (Left 1.0) = pure x
division x y = assign $ showSample y <> "!=0? " <> showSample x <> "/" <> showSample y <> " : 0"

_division :: Number -> Number -> Number
_division _ 0.0 = 0.0
_division x y = x/y

mod :: Sample -> Sample -> W Sample
mod = operator (Prelude.mod) "%"

pow :: Sample -> Sample -> W Sample
pow = operator Number.pow "**"

equal :: Sample -> Sample -> W Sample
equal = operator (\a b -> if a == b then 1.0 else 0.0) "==" 

notEqual :: Sample -> Sample -> W Sample
notEqual = operator (\a b -> if a /= b then 1.0 else 0.0) "!="

greaterThan :: Sample -> Sample -> W Sample
greaterThan = operator (\a b -> if a > b then 1.0 else 0.0) ">"

greaterThanEqual :: Sample -> Sample -> W Sample
greaterThanEqual = operator (\a b -> if a >= b then 1.0 else 0.0) ">="

lessThan :: Sample -> Sample -> W Sample
lessThan = operator (\a b -> if a < b then 1.0 else 0.0) "<"

lessThanEqual :: Sample -> Sample -> W Sample
lessThanEqual = operator (\a b -> if a <= b then 1.0 else 0.0) "<="

max :: Sample -> Sample -> W Sample
max = function Prelude.max "Math.max"

min :: Sample -> Sample -> W Sample
min = function Prelude.min "Math.min"

gate :: Sample -> Sample -> W Sample
gate (Left x) (Left y) = pure $ Left $ if Ord.abs y >= x then y else 0.0
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
    min' = Prelude.min e0 e1
    max' = Prelude.max e0 e1
    
_uncheckedClip :: Number -> Number -> Number -> Number
_uncheckedClip e0 e1 x = Prelude.max e0 (Prelude.min e1 x)

between :: Tuple Sample Sample -> Sample -> W Sample
between (Tuple (Left e0) (Left e1)) (Left x) = pure $ Left $ if x >= min' && x <= max' then 1.0 else 0.0
  where
    min' = Prelude.min e0 e1
    max' = Prelude.max e0 e1
between (Tuple e0 e1) x = assign $ "(" <> showSample x <> ">=" <> min' <> "&&" <> showSample x <> "<=" <> max' <> ")?1:0"
  where
    min' = "Math.min(" <> showSample e0 <> "," <> showSample e1 <> ")"
    max' = "Math.max(" <> showSample e0 <> "," <> showSample e1 <> ")"
    
smoothStep :: Tuple Sample Sample -> Sample -> W Sample
smoothStep (Tuple (Left e0) (Left e1)) (Left x) = pure $ Left $ t * t * (3.0 - (2.0 * t))
  where t = _uncheckedClip 0.0 1.0 $ _division (x - e0) (e1 - e0)
smoothStep (Tuple e0 e1) x = do
  let a = "(" <> showSample x <> "-" <> showSample e0 <> ")/(" <> showSample e1 <> "-" <> showSample e0 <> ")"
  t <- assign $ "Math.max(0.0,Math.min(1.0," <> a <> "))"
  let t' = showSample t
  assign $ t' <> "*" <> t' <> "*(3-(2*" <> t' <> "))"

{-
seq :: Frame -> Sample -> W Sample
seq steps x = do
  let nSteps = length steps
  let stepSize = 1.0 / toNumber nSteps
  let stepStarts = iterateN nSteps (_ + stepSize) 0.0
  let f val stepStart = between (Tuple (Left stepStart) (Left $ stepStart+stepSize)) x >>= product val
  sequence (zipWith f steps stepStarts) >>= sum
-}
  
sum :: Frame -> W Sample
sum = assign <<< intercalate "+" <<< map showSample <<< flatten

mix :: Tuple Sample Sample -> Sample -> W Sample
mix (Tuple (Left x) (Left y)) (Left a) = pure $ Left $ ((y-x)*a)+x
mix (Tuple x y) a = difference y x >>= product a >>= add x

linlin :: Tuple Sample Sample -> Tuple Sample Sample -> Sample -> W Sample
linlin (Tuple (Left r1x) (Left r1y)) (Tuple (Left r2x) (Left r2y)) (Left x) = pure $ Left $ (_division (x - r1x) (r1y - r1x)) * (r2y - r2x) + r2x
linlin (Tuple r1x r1y) (Tuple r2x r2y) x = do
  x' <- difference x r1x
  r2 <- difference r2y r2x
  difference r1y r1x >>= division x' >>= product r2 >>= add r2x

abs :: Sample -> W Sample
abs (Left x) = pure $ Left $ Ord.abs x
abs (Right x) = assign $ "Math.abs(" <> x <> ")"
  
foreign import acosh :: Number -> Number
foreign import asinh :: Number -> Number
foreign import atanh :: Number -> Number
foreign import cbrt :: Number -> Number
foreign import cosh :: Number -> Number
foreign import log2 :: Number -> Number
foreign import log10 :: Number -> Number
foreign import sinh :: Number -> Number
foreign import tanh :: Number -> Number

