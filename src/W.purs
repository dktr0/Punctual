module W where

-- A monad and associated functions for generating the code of a WebAudio audio worklet.

import Prelude (Unit, bind, discard, map, negate, otherwise, pure, show, ($), (*), (+), (-), (/), (<), (<$>), (<<<), (<=), (<>), (==), (>>=))
import Control.Monad.State (State,get,put,runState,modify_)
import Data.List.NonEmpty (NonEmptyList, head, last, length, zipWith)
import Data.Foldable (intercalate,foldM)
import Data.Traversable (traverse,for,sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (iterateN, replicate1, singleton)
import Data.Int (toNumber,round)

import MultiMode (MultiMode(..))
import Matrix (Matrix, combine, concat, flatten, fromNonEmptyList)
import Number (showNumber) as Number

type W = State WState

type WState = {
  allocatedFloats :: Int,
  allocatedInts :: Int,
  code :: String,
  time :: Sample,
  beat :: Sample,
  etime :: Sample,
  ebeat :: Sample
  }

runW :: forall a. W a -> Tuple a WState
runW x = runState x { allocatedFloats: 0, allocatedInts: 0, code: "", time: "time", beat: "beat", etime: "eTime", ebeat: "eBeat" }

type Sample = String

{- OBSOLETE
showSample :: Sample -> String
showSample (Left x) = Number.showNumber x
showSample (Right x) = x
-}

zero :: Sample
zero = "0.0"

assign :: Sample -> W Sample
assign x = do
  f <- allocateFloat
  write $ f <> "=" <> x <> ";\n"
  pure f

assignInt :: String -> W String
assignInt x = do
  i <- allocateInt
  write $ i <> "=" <> x <> ";\n"
  pure i

{-
assignIfVariable :: Sample -> W Sample
assignIfVariable (Left x) = pure $ Left x
assignIfVariable (Right x) = assign x
-}

allocateFloat :: W String
allocateFloat = do
  s <- get
  put $ s { allocatedFloats = s.allocatedFloats + 1 }
  pure $ "f[" <> show s.allocatedFloats <> "]"

allocateFloats :: Int -> W Int
allocateFloats n = do
  s <- get
  put $ s { allocatedFloats = s.allocatedFloats + n }
  pure s.allocatedFloats

allocateInt :: W String
allocateInt = do
  s <- get
  put $ s { allocatedInts = s.allocatedInts + 1 }
  pure $ "i[" <> show s.allocatedInts <> "]"

  
write :: String -> W Unit
write x = modify_ $ \s -> s { code = s.code <> x } 
  
-- single-sample delay
ssd :: Sample -> W Sample
ssd i = do
  x0 <- allocateFloat
  x1 <- allocateFloat
  write $ x1 <> "=" <> x0 <> ";\n" 
  write $ x0 <> "=" <> i <> ";\n"
  pure x1

biquad :: Sample -> Sample -> Sample -> Sample -> Sample -> Sample -> Sample -> W Sample
biquad b0 b1 b2 a0 a1 a2 i = do
  x0 <- allocateFloat
  x1 <- allocateFloat
  x2 <- allocateFloat
  y0 <- allocateFloat
  y1 <- allocateFloat
  y2 <- allocateFloat
  write $ x2 <> "=" <> x1 <> ";\n" 
  write $ x1 <> "=" <> x0 <> ";\n"
  write $ x0 <> "=" <> i <> ";\n"
  write $ y2 <> "=" <> y1 <> ";\n"
  write $ y1 <> "=" <> y0 <> ";\n"
  let b0x0 = "(" <> x0 <> "*" <> b0 <> "/" <> a0 <> ")"
  let b1x1 = "(" <> x1 <> "*" <> b1 <> "/" <> a0 <> ")"
  let b2x2 = "(" <> x2 <> "*" <> b2 <> "/" <> a0 <> ")"
  let a1y1 = "(" <> y1 <> "*" <> a1 <> "/" <> a0 <> ")"
  let a2y2 = "(" <> y2 <> "*" <> a2 <> "/" <> a0 <> ")"
  write $ y0 <> "=" <> b0x0 <> "+" <> b1x1 <> "+" <> b2x2 <> "-" <> a1y1 <> "-" <> a2y2 <> ";\n"
  pure y0

twoPi :: Sample
twoPi = "(2.0*Math.PI)" 

sampleRate :: Sample
sampleRate = "sampleRate"

lpf :: Sample -> Sample -> Sample -> W Sample
lpf f0 q i = do
  twoPiF0 <- product twoPi f0
  w0 <- divisionUnsafe twoPiF0 sampleRate
  cosW0 <- assign $ unaryFunction "Math.cos" w0
  oneMinusCosW0 <- difference (Number.showNumber 1.0) cosW0
  sinW0 <- assign $ unaryFunction "Math.sin" w0
  alpha <- product (Number.showNumber 2.0) q >>= divisionSafe sinW0
  b0 <- divisionUnsafe oneMinusCosW0 (Number.showNumber 2.0)
  let b1 = oneMinusCosW0
  let b2 = b0
  a0 <- add (Number.showNumber 1.0) alpha 
  a1 <- product (Number.showNumber (-2.0)) cosW0 
  a2 <- difference (Number.showNumber 1.0) alpha 
  biquad b0 b1 b2 a0 a1 a2 i

hpf :: Sample -> Sample -> Sample -> W Sample
hpf f0 q i = do
  twoPiF0 <- product twoPi f0
  w0 <- divisionUnsafe twoPiF0 sampleRate
  cosW0 <- assign $ unaryFunction "Math.cos" w0
  onePlusCosW0 <- add (Number.showNumber 1.0) cosW0
  sinW0 <- assign $ unaryFunction "Math.sin" w0
  alpha <- product (Number.showNumber 2.0) q >>= divisionSafe sinW0
  b0 <- divisionUnsafe onePlusCosW0 (Number.showNumber 2.0)
  b1 <- product (Number.showNumber (-1.0)) onePlusCosW0
  let b2 = b0
  a0 <- add (Number.showNumber 1.0) alpha 
  a1 <- product (Number.showNumber (-2.0)) cosW0 
  a2 <- difference (Number.showNumber 1.0) alpha 
  biquad b0 b1 b2 a0 a1 a2 i

-- following "constant 0 db peak gain" recipe from audio eq cookbook
bpf :: Sample -> Sample -> Sample -> W Sample
bpf f0 q i = do
  twoPiF0 <- product twoPi f0
  w0 <- divisionUnsafe twoPiF0 sampleRate
  cosW0 <- assign $ unaryFunction "Math.cos" w0
  -- onePlusCosW0 <- difference "1.0" cosW0
  sinW0 <- assign $ unaryFunction "Math.sin" w0
  alpha <- product "2.0" q >>= divisionSafe sinW0
  let b0 = alpha
  let b1 = zero
  b2 <- product (Number.showNumber (-1.0)) alpha
  a0 <- add (Number.showNumber 1.0) alpha 
  a1 <- product (Number.showNumber (-2.0)) cosW0 
  a2 <- difference (Number.showNumber 1.0) alpha 
  biquad b0 b1 b2 a0 a1 a2 i

delay :: Number -> Sample -> Sample -> W Sample
delay maxT t x = do
  let maxSamples = round $ maxT * 48000.0 -- TODO remove PLACEHOLDER (hard-coded sample rate at "initialization" time)
  bufferStart <- allocateFloats maxSamples
  -- write input into buffer
  inputPointer <- allocateInt
  write $ "f[" <> show bufferStart <> "+" <> inputPointer <> "]=" <> x <> ";\n"
  -- read output from buffer
  currentSamplesF <- product t sampleRate
  currentSamples <- assignInt $ "Math.round(" <> currentSamplesF <> ")"
  outputPointer <- assignInt $ inputPointer <> "-" <> currentSamples
  write $ outputPointer <> "=" <> outputPointer <> ">=0?(" <> show bufferStart <> "+" <> outputPointer <> "):(" <> show bufferStart <> "+" <> show maxSamples <> "+" <> outputPointer <> ");\n"
  y <- assign $ "f[" <> outputPointer <> "]"
  -- advance input pointer
  write $ inputPointer <> "=(" <> inputPointer <> "+1)%" <> show maxSamples <> ";\n"
  pure y

phasor :: Sample -> W Sample
phasor f = do
  inc <- divisionUnsafe f sampleRate
  y <- allocateFloat
  write $ y <> "=(" <> y <> "+" <> inc <> ")%1;\n"
  pure y
  
-- linearly interpolated wavetable lookup
wavetable :: String -> Int -> Sample -> W Sample
wavetable tableName tableLength phase = do
  nIdeal <- product phase (Number.showNumber $ toNumber tableLength)
  n0 <- assign $ "Math.floor(" <> nIdeal <> ")"
  n1 <- add n0 ("1.0")
  weight0 <- difference n1 nIdeal
  weight1 <- difference nIdeal n0
  lookup0 <- assign $ tableName <> "[" <> n0 <> "%" <> show tableLength <> "]"
  lookup1 <- assign $ tableName <> "[" <> n1 <> "%" <> show tableLength <> "]"
  y0' <- product lookup0 weight0
  y1' <- product lookup1 weight1
  add y0' y1'

osc :: Sample -> W Sample
osc f = phasor f >>= wavetable "sin" 16384

saw :: Sample -> W Sample
saw f = phasor f >>= wavetable "saw" 4096

sqr :: Sample -> W Sample
sqr f = phasor f >>= wavetable "sqr" 4096

tri :: Sample -> W Sample
tri f = phasor f >>= wavetable "tri" 4096

type Frame = Matrix Sample

{-
signalToFrame :: Signal -> W Frame

signalToFrame (Constant x) = pure $ pure $ Left x

signalToFrame (SignalList Combinatorial xs) = 
  case fromList xs of
    Nothing -> pure $ pure $ Left 0.0
    Just xs' -> fromNonEmptyListMulti <$> traverse signalToFrame xs' -- :: NonEmptyList Frame == NonEmptyList (Matrix Sample)

signalToFrame (SignalList Pairwise xs) = 
  case fromList xs of
    Nothing -> pure $ pure $ Left 0.0
    Just xs' -> do
      xs'' <- map flatten <$> traverse signalToFrame xs'
      pure $ Matrix $ pairwise xs''

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

signalToFrame (AIn n o) = do
  let ns = map ((+) o) $ range 0 (Prelude.max 0 (n-1))
  xs <- traverse (\n' -> assign $ "ain(input," <> show n' <> ")[n]" ) ns
  pure $ fromNonEmptyList xs

signalToFrame (Bipolar x) = signalToFrame x >>= traverse bipolar
signalToFrame (Unipolar x) = signalToFrame x >>= traverse unipolar

signalToFrame (Osc f) = signalToFrame f >>= traverse osc
signalToFrame (Tri f) = signalToFrame f >>= traverse tri
signalToFrame (Saw f) = signalToFrame f >>= traverse saw
signalToFrame (Sqr f) = signalToFrame f >>= traverse sqr

signalToFrame (LFTri f) = do
  xs <- signalToFrame f >>= traverse phasor
  traverse (\x -> assign $ showSample x <> "<0.5?(" <> showSample x <> "*4-1):(" <> showSample x <> "*(-4)+3)") xs

signalToFrame (LFSaw f) = signalToFrame f >>= traverse phasor >>= traverse bipolar

signalToFrame (LFSqr f) = do
  xs <- signalToFrame f >>= traverse phasor
  traverse (\x -> assign $ showSample x <> ">=0.5?1:-1") xs

signalToFrame (Abs x) = signalToFrame x >>= traverse abs
signalToFrame (Acos x) = unaryFunction Number.acos "Math.acos" x
signalToFrame (Acosh x) = unaryFunction Number.acosh "Math.acosh" x
signalToFrame (Asin x) = unaryFunction Number.asin "Math.asin" x
signalToFrame (Asinh x) = unaryFunction Number.asinh "Math.asinh" x
signalToFrame (Atan x) = unaryFunction Number.atan "Math.atan" x
signalToFrame (Atanh x) = unaryFunction Number.atanh "Math.atanh" x
signalToFrame (Cbrt x) = unaryFunction Number.cbrt "Math.cbrt" x
signalToFrame (Ceil x) = unaryFunction Number.ceil "Math.ceil" x
signalToFrame (Cos x) = unaryFunction Number.cos "Math.cos" x
signalToFrame (Cosh x) = unaryFunction Number.cosh "Math.cosh" x
signalToFrame (Exp x) = unaryFunction Number.exp "Math.exp" x
signalToFrame (Floor x) = unaryFunction Number.floor "Math.floor" x
signalToFrame (Log x) = unaryFunction Number.log "Math.log" x
signalToFrame (Log2 x) = unaryFunction Number.log2 "Math.log2" x
signalToFrame (Log10 x) = unaryFunction Number.log10 "Math.log10" x
signalToFrame (Round x) = unaryFunction Number.round "Math.round" x
signalToFrame (Sign x) = unaryFunction Number.sign "Math.sign" x
signalToFrame (Sin x) = unaryFunction Number.sin "Math.sin" x
signalToFrame (Sinh x) = unaryFunction Number.sinh "Math.sinh" x
signalToFrame (Sqrt x) = unaryFunction Number.sqrt "Math.sqrt" x
signalToFrame (Tan x) = unaryFunction Number.tan "Math.tan" x
signalToFrame (Tanh x) = unaryFunction Number.tanh "Math.tanh" x
signalToFrame (Trunc x) = unaryFunction Number.trunc "Math.trunc" x
signalToFrame (MidiCps x) = signalToFrame x >>= traverse midicps
signalToFrame (CpsMidi x) = signalToFrame x >>= traverse cpsmidi
signalToFrame (DbAmp x) = signalToFrame x >>= traverse dbamp
signalToFrame (AmpDb x) = signalToFrame x >>= traverse ampdb
signalToFrame (Fract x) = signalToFrame x >>= traverse fract

signalToFrame (Early x z) = do
  xs <- flatten <$> signalToFrame x
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
  xs <- flatten <$> signalToFrame x
  s <- get
  xs' <- for xs $ \y -> do
    time <- divisionSafe s.time y
    beat <- divisionSafe s.beat y
    etime <- divisionSafe s.etime y
    ebeat <- divisionSafe s.ebeat y
    pure { time, beat, etime, ebeat }  
  withAlteredTime xs' $ signalToFrame z

signalToFrame (Addition mm x y) = binaryFunction add mm x y
signalToFrame (Difference mm x y) = binaryFunction difference mm x y
signalToFrame (Product mm x y) = binaryFunction product mm x y
signalToFrame (Division mm x y) = binaryFunction divisionSafe mm x y
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
signalToFrame (Clip mm x y) = binaryFunctionWithRange clip mm x y
signalToFrame (Between mm x y) = binaryFunctionWithRange between mm x y
signalToFrame (SmoothStep mm x y) = binaryFunctionWithRange smoothStep mm x y

signalToFrame (Spr mm x y) = do
  xs <- signalToFrame x >>= semiFlatten >>> fromNonEmptyList >>> pure -- :: one-dimensional Matrix (NonEmptyList Sample)
  ys <- signalToFrame y >>= traverse unipolar -- :: Matrix Sample
  sequence $ combine seq mm xs ys

signalToFrame (Btw mm n x y) = do
  xs <- signalToFrame x
  ys <- signalToFrame y
  zs <- sequence $ combine (btw n) mm xs ys -- Matrix Frame = Matrix (Matrix Sample)
  pure $ fromMatrixMatrix zs

signalToFrame (Pan mm n p x) = do
  ps <- signalToFrame p >>= traverse unipolar
  xs <- signalToFrame x
  ys <- sequence $ combine (pan n) mm ps xs -- Matrix Frame = Matrix (Matrix Sample)
  pure $ fromMatrixMatrix ys

signalToFrame (Splay n x) = signalToFrame x >>= splay n >>= fromNonEmptyList >>> pure

signalToFrame (Seq s) = do
  xs <- semiFlatten <$> signalToFrame s -- :: NonEmptyList Frame
  b <- (_.beat <$> get) >>= fract
  rs <- traverse (\x -> seq x b) xs -- NonEmptyList Sample
  pure $ fromNonEmptyList rs

signalToFrame (Mix mm x y a) = do
  x' <- flatten <$> signalToFrame x
  y' <- flatten <$> signalToFrame y
  let xys = fromNonEmptyList $ zipWithEqualLength Tuple x' y'
  a' <- signalToFrame a
  sequence $ combine mix mm xys a'

signalToFrame (LinLin mm r1 r2 x) = do
  r1s <- toTuples <$> signalToFrame r1
  r2s <- toTuples <$> signalToFrame r2
  xs <- signalToFrame x
  sequence $ combine3 linlin mm r1s r2s xs

signalToFrame (LPF mm f0 q i) = do
  f0' <- signalToFrame f0
  q' <- signalToFrame q
  i' <- signalToFrame i
  sequence $ combine3 lpf mm f0' q' i'

signalToFrame (HPF mm f0 q i) = do
  f0' <- signalToFrame f0
  q' <- signalToFrame q
  i' <- signalToFrame i
  sequence $ combine3 hpf mm f0' q' i'

signalToFrame (BPF mm f0 q i) = do
  f0' <- signalToFrame f0
  q' <- signalToFrame q
  i' <- signalToFrame i
  sequence $ combine3 bpf mm f0' q' i'

signalToFrame (Delay maxT t x) = do
  t' <- signalToFrame t
  x' <- signalToFrame x
  sequence $ combine (delay maxT) Combinatorial t' x'

signalToFrame _ = pure $ pure $ Left 0.0

-}

withAlteredTime :: NonEmptyList { time :: Sample, beat :: Sample, etime :: Sample, ebeat :: Sample }  -> W Frame -> W Frame
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs

unaryFunction :: String -> Sample -> Sample
unaryFunction name x = name <> "(" <> x <> ")"

bipolar :: Sample -> W Sample
bipolar x = assign $ x <> "*2-1"

unipolar :: Sample -> W Sample
unipolar x = assign $ x <> "*0.5+0.5"

midicps :: Sample -> W Sample
midicps x = assign $ "440 * (2 ** ((" <> x <> "-69)/12))"

cpsmidi :: Sample -> W Sample
cpsmidi x = assign $ "69 + (12 * (Math.log2(" <> x <> "/440)))"

dbamp :: Sample -> W Sample
dbamp x = assign $ "10 ** (" <> x <> "/20)"

ampdb :: Sample -> W Sample
ampdb x = assign $ "20 * Math.log10(" <> x <> ")"

fract :: Sample -> W Sample
fract x = assign $ "Math.abs(" <> x <> "%1)"

abs :: Sample -> W Sample
abs x = assign $ unaryFunction "Math.abs" x

operator :: String -> Sample -> Sample -> W Sample
operator op x y = assign $ x <> op <> y

function :: String -> Sample -> Sample -> W Sample
function f x y = assign $ f <> "(" <> x <> "," <> y <> ")"

add :: Sample -> Sample -> W Sample
add = operator "+"

difference :: Sample -> Sample -> W Sample
difference = operator "-"

product :: Sample -> Sample -> W Sample
product = operator "*"

divisionSafe :: Sample -> Sample -> W Sample
divisionSafe x y = assign $ y <> "!=0? " <> x <> "/" <> y <> " : 0"

divisionUnsafe :: Sample -> Sample -> W Sample
divisionUnsafe x y = assign $ x <> "/" <> y

mod :: Sample -> Sample -> W Sample
mod = operator "%"

pow :: Sample -> Sample -> W Sample
pow = operator "**"

equal :: Sample -> Sample -> W Sample
equal = operator "==" 

notEqual :: Sample -> Sample -> W Sample
notEqual = operator "!="

greaterThan :: Sample -> Sample -> W Sample
greaterThan = operator ">"

greaterThanEqual :: Sample -> Sample -> W Sample
greaterThanEqual = operator ">="

lessThan :: Sample -> Sample -> W Sample
lessThan = operator "<"

lessThanEqual :: Sample -> Sample -> W Sample
lessThanEqual = operator "<="

max :: Sample -> Sample -> W Sample
max = function "Math.max"

min :: Sample -> Sample -> W Sample
min = function "Math.min"

gate :: Sample -> Sample -> W Sample
gate x y = assign $ "Math.abs(" <> y <> ")>=" <> x <> "?" <> y <> ":0"

clip :: Tuple Sample Sample -> Sample -> W Sample
clip (Tuple e0 e1) x = assign $ "Math.max(" <> min' <> ",Math.min(" <> max' <> "," <> x <> "))"
  where
    min' = "Math.min(" <> e0 <> "," <> e1 <> ")"
    max' = "Math.max(" <> e0 <> "," <> e1 <> ")"
    
between :: Tuple Sample Sample -> Sample -> W Sample
between (Tuple e0 e1) x = assign $ "(" <> x <> ">=" <> min' <> "&&" <> x <> "<=" <> max' <> ")?1:0"
  where
    min' = "Math.min(" <> e0 <> "," <> e1 <> ")"
    max' = "Math.max(" <> e0 <> "," <> e1 <> ")"

-- variant of between that is inclusive on left end of interval, exclusive on right end of interval (e.g. for use in seq)
between' :: Tuple Sample Sample -> Sample -> W Sample
between' (Tuple e0 e1) x = assign $ "(" <> x <> ">=" <> min' <> "&&" <> x <> "<" <> max' <> ")?1:0"
  where
    min' = "Math.min(" <> e0 <> "," <> e1 <> ")"
    max' = "Math.max(" <> e0 <> "," <> e1 <> ")"

smoothStep :: Tuple Sample Sample -> Sample -> W Sample
smoothStep (Tuple e0 e1) x = do
  let a = "(" <> x <> "-" <> e0 <> ")/(" <> e1 <> "-" <> e0 <> ")"
  t <- assign $ "Math.max(0.0,Math.min(1.0," <> a <> "))"
  assign $ t <> "*" <> t <> "*(3-(2*" <> t <> "))"

seq :: NonEmptyList Sample -> Sample -> W Sample
seq steps x = do
  let nSteps = length steps
  let stepSize = 1.0 / toNumber nSteps
  let stepStarts = iterateN nSteps (_ + stepSize) 0.0
  let f val stepStart = between' (Tuple (Number.showNumber stepStart) (Number.showNumber $ stepStart+stepSize)) x >>= product val
  sequence (zipWith f steps stepStarts) >>= (sum <<< fromNonEmptyList)
  
sum :: Frame -> W Sample
sum = assign <<< intercalate "+" <<< flatten

-- given a list of frames, sum over corresponding channels pairwise
sumChannels :: NonEmptyList Frame -> W Frame
sumChannels xs = foldM (\x y -> sequence $ combine add Pairwise x y) (pure zero) xs

mix :: Tuple Sample Sample -> Sample -> W Sample
mix (Tuple x y) a = difference y x >>= product a >>= add x

linlin :: Tuple Sample Sample -> Tuple Sample Sample -> Sample -> W Sample
linlin (Tuple r1x r1y) (Tuple r2x r2y) x = do
  x' <- difference x r1x
  r2 <- difference r2y r2x
  difference r1y r1x >>= divisionSafe x' >>= product r2 >>= add r2x
  
splay :: Int -> Matrix Sample -> W (NonEmptyList Sample)
splay nOutputChnls xs
  | nOutputChnls <= 1 = pure <$> sum xs
  | length (flatten xs) == 1 = flatten <$> pan nOutputChnls "0.5" (head $ flatten xs)
  | otherwise = do
      let xs' = flatten xs
      let nInputChnls = length xs'
      let stepSize = 1.0 / toNumber (nInputChnls - 1)
      let inputPositions = map Number.showNumber $ iterateN nInputChnls (_ + stepSize) 0.0
      xss <- sequence $ zipWith (pan nOutputChnls) inputPositions xs' -- :: NonEmptyList Frame -- one Frame per input, each Frame has nOutputChnls Samples
      flatten <$> sumChannels xss

aout :: Int -> Int -> Matrix Sample -> W (NonEmptyList Sample)
aout nOutputChnls channelOffset xs = do
  xs' <- splay nOutputChnls xs
  case channelOffset <= 0 of
    true -> pure xs'
    false -> pure $ replicate1 channelOffset zero <> xs'
      
btw :: Int -> Sample -> Sample -> W Frame
btw n x y 
  | n < 2 = do 
      z <- add x y >>= product "0.5"
      pure $ singleton z
  | otherwise = do
      yx <- difference y x
      delta <- divisionUnsafe yx (Number.showNumber $ toNumber $ n-1)
      zs <- foldM _btwFolder (singleton x :: NonEmptyList Sample) (replicate1 (n-2) delta :: NonEmptyList Sample)
      pure $ fromNonEmptyList $ zs <> singleton y

_btwFolder :: NonEmptyList Sample -> Sample -> W (NonEmptyList Sample)
_btwFolder frame delta = do
  x <- add delta (last frame)
  pure $ frame <> singleton x

pan :: Int -> Sample -> Sample -> W Frame
pan nOutputChnls pos i 
  | nOutputChnls <= 1 = pure $ pure i
  | otherwise = do
      pos' <- product pos (Number.showNumber $ toNumber $ nOutputChnls - 1)
      let outputPositions = iterateN nOutputChnls (_ + 1.0) 0.0 
      outputDistances <- traverse (\op -> difference (Number.showNumber op) pos' >>= abs >>= clip (Tuple zero "1.0") ) outputPositions
      outputGains <- traverse gainFromDistance outputDistances
      fromNonEmptyList <$> traverse (product i) outputGains

gainFromDistance :: Sample -> W Sample
gainFromDistance x = assign $ "Math.abs(" <> x <> ")>1?0:Math.cos(Math.abs(" <> x <> ")*Math.PI/2)"
