module FragmentShader where

import Prelude(($),pure,show,bind,(<>),(>>=),(<$>),(<<<),map)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (NonEmptyList,singleton,concat,fromList,zipWith,cons,head,tail)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold,intercalate,foldM)
import Data.Set as Set
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get)
import Data.Map (lookup)
import Data.FunctorWithIndex (mapWithIndex)

import NonEmptyList
import Signal (Signal(..),MultiMode(..))
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum,ternaryFunction,glslTypeToString)
import GLSL (GLSL,assign,assignForced,swizzleX,swizzleY,swizzleZ,swizzleW,alignFloat,texture2D,textureFFT,alignVec2,alignVec3,alignRGBA,runGLSL)

testCodeGen :: Boolean -> Signal -> String
testCodeGen webGl2 x = assignments <> lastExprs
  where
    (Tuple a st) = runGLSL webGl2 (signalToGLSL x)
    assignments = fold $ mapWithIndex indexedGLSLExprToString st.exprs
    lastExprs = fold $ map (\y -> y.string <> "\n") a
    

indexedGLSLExprToString :: Int -> GLSLExpr -> String
indexedGLSLExprToString n x = glslTypeToString x.glslType <> " _" <> show n <> " = " <> x.string <> ";\n"

signalToGLSL :: Signal -> GLSL (NonEmptyList GLSLExpr)

signalToGLSL (Constant x) = pure $ singleton { string: show x, glslType: Float, isSimple: true, deps: Set.empty } -- !! TODO: make sure value is shown in GLSL compatible way, ie always ending with a period

signalToGLSL (SignalList xs) = do
  case fromList xs of
    Nothing -> pure $ singleton $ zero
    Just xs' -> concat <$> traverse signalToGLSL xs'

signalToGLSL (Append x y) = do -- note: we will redo this soon with more of an "in advance" concept of alignment
  xs <- signalToGLSL x
  ys <- signalToGLSL y
  pure $ xs <> ys

signalToGLSL (Zip x y) = do
  xs <- signalToGLSL x >>= alignFloat
  ys <- signalToGLSL y >>= alignFloat
  let (Tuple xs' ys') = extendToEqualLength xs ys
  pure $ concat $ zipWith (\anX anY -> anX `cons` singleton anY ) xs' ys'

signalToGLSL (Mono x) = do
  xs <- signalToGLSL x
  let xs' = dotSum <$> xs
  let s = "(" <> intercalate " + " (_.string <$> xs') <> ")"
  pure $ singleton $ { string: s, glslType: Float, isSimple: false, deps: fold (_.deps <$> xs) }

signalToGLSL (Rep 0 _) = pure $ singleton $ zero
signalToGLSL (Rep n x) = (concat <<< replicate1 n) <$> signalToGLSL x

signalToGLSL Pi = pure $ singleton $ simpleFromString Float "PI"

signalToGLSL Px = pure $ singleton $ simpleFromString Float "(2./width)"

signalToGLSL Py = pure $ singleton $ simpleFromString Float "(2./height)"

signalToGLSL Pxy = pure $ singleton $ simpleFromString Vec2 "(2./vec2(width,height))"

signalToGLSL Aspect = pure $ singleton $ simpleFromString Float "(width/height)"

signalToGLSL Fx = do
  s <- get
  traverse swizzleX s.fxys

signalToGLSL Fy = do
  s <- get
  traverse swizzleY s.fxys

signalToGLSL Fxy = do
  s <- get
  pure s.fxys

signalToGLSL FRt = signalToGLSL $ XyRt Fxy

signalToGLSL FR = signalToGLSL $ XyR Fxy

signalToGLSL FT = signalToGLSL $ XyT Fxy

signalToGLSL Lo = pure $ singleton $ simpleFromString Float "lo"

signalToGLSL Mid = pure $ singleton $ simpleFromString Float "mid"

signalToGLSL Hi = pure $ singleton $ simpleFromString Float "hi"

signalToGLSL ILo = pure $ singleton $ simpleFromString Float "ilo"

signalToGLSL IMid = pure $ singleton $ simpleFromString Float "imid"

signalToGLSL IHi = pure $ singleton $ simpleFromString Float "ihi"

signalToGLSL Cps = pure $ singleton $ simpleFromString Float "_cps"

signalToGLSL Time = pure $ singleton $ simpleFromString Float "_time"

signalToGLSL Beat = pure $ singleton $ simpleFromString Float "_beat"

signalToGLSL ETime = pure $ singleton $ simpleFromString Float "_etime"

signalToGLSL EBeat = pure $ singleton $ simpleFromString Float "_ebeat"

signalToGLSL (FFT x) = signalToGLSL x >>= unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_fft")

signalToGLSL (IFFT x) = signalToGLSL x >>= unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_ifft")

signalToGLSL (Fb xy) = signalToGLSL xy >>= unipolar >>= traverse assignForced >>= alignVec2 >>= traverse (texture2D "_fb")

signalToGLSL Cam = do
  s <- get
  traverse (texture2D "_fb") s.fxys
  
signalToGLSL (Img url) = do
  s <- get
  case lookup url s.imgMap of
    Just n -> traverse (texture2D $ "tex" <> show n) s.fxys
    Nothing -> pure $ singleton $ zero

signalToGLSL (Vid url) = do
  s <- get
  case lookup url s.vidMap of
    Just n -> traverse (texture2D $ "tex" <> show n) s.fxys
    Nothing -> pure $ singleton $ zero


signalToGLSL (Blend x) = do
  xs <- signalToGLSL x >>= alignRGBA
  case fromList (tail xs) of
    Nothing -> pure $ singleton $ head xs
    Just t -> singleton <$> foldM blend (head xs) t

signalToGLSL (RgbHsv x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "rgbhsv"
signalToGLSL (HsvRgb x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "hsvrgb"
signalToGLSL (HsvH x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleX
signalToGLSL (HsvS x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleY
signalToGLSL (HsvV x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL (HsvR x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "hsvrgb" >>= traverse swizzleX
signalToGLSL (HsvG x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "hsvrgb" >>= traverse swizzleY
signalToGLSL (HsvB x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "hsvrgb" >>= traverse swizzleZ
signalToGLSL (RgbR x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleX
signalToGLSL (RgbG x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleY
signalToGLSL (RgbB x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL (RgbH x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "rgbhsv" >>= traverse swizzleX
signalToGLSL (RgbS x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "rgbhsv" >>= traverse swizzleY
signalToGLSL (RgbV x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction "rgbhsv" >>= traverse swizzleZ

signalToGLSL (Osc x) = signalToGLSL x >>= (unaryExpression $ \e -> e <> "*PI*2.0*_time") >>= simpleUnaryFunction "sin"
signalToGLSL (Tri x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "tri"
signalToGLSL (Saw x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "saw"
signalToGLSL (Sqr x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "sqr"
signalToGLSL (LFTri x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "tri"
signalToGLSL (LFSaw x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "saw"
signalToGLSL (LFSqr x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "sqr"

signalToGLSL (Abs x) = signalToGLSL x >>= simpleUnaryFunction "abs"
signalToGLSL (Acos x) = signalToGLSL x >>= simpleUnaryFunction "acos"
signalToGLSL (Acosh x) = signalToGLSL x >>= acosh 
signalToGLSL (AmpDb x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(20.*log(" <> s <> ")/log(10.))")  
signalToGLSL (Asin x) = signalToGLSL x >>= simpleUnaryFunction "asin"
signalToGLSL (Asinh x) = signalToGLSL x >>= asinh 
signalToGLSL (Atan x) = signalToGLSL x >>= simpleUnaryFunction "atan"
signalToGLSL (Atanh x) = signalToGLSL x >>= atanh
signalToGLSL (Bipolar x) = signalToGLSL x >>= simpleUnaryExpression (\e -> "(" <> e <> "*2.-1.)")
signalToGLSL (Cbrt x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "pow(" <> s <> ",0.3333333333)")
signalToGLSL (Ceil x) = signalToGLSL x >>= simpleUnaryFunction "ceil"
signalToGLSL (Cos x) = signalToGLSL x >>= simpleUnaryFunction "cos"
signalToGLSL (Cosh x) = signalToGLSL x >>= cosh
signalToGLSL (CpsMidi x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(69.+(12.*log2(" <> s <> "/440.)))")
signalToGLSL (DbAmp x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "pow(10.," <> s <> "/20.)")
signalToGLSL (Exp x) = signalToGLSL x >>= simpleUnaryFunction "exp"
signalToGLSL (Floor x) = signalToGLSL x >>= simpleUnaryFunction "floor"
signalToGLSL (Fract x) = signalToGLSL x >>= simpleUnaryFunction "fract"
signalToGLSL (Log x) = signalToGLSL x >>= simpleUnaryFunction "log"
signalToGLSL (Log2 x) = signalToGLSL x >>= simpleUnaryFunction "log2"
signalToGLSL (Log10 x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(log(" <> s <> ")/log(10.))")
signalToGLSL (MidiCps x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(440.*pow((" <> s <> "-69.)/12.,2.))")
signalToGLSL (Round x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(floor(" <> s <> ")+0.5)")
signalToGLSL (Sign x) = signalToGLSL x >>= simpleUnaryFunction "sign"
signalToGLSL (Sin x) = signalToGLSL x >>= simpleUnaryFunction "sin"
signalToGLSL (Sinh x) = signalToGLSL x >>= sinh
signalToGLSL (Sqrt x) = signalToGLSL x >>= simpleUnaryFunction "sqrt"
signalToGLSL (Tan x) = signalToGLSL x >>= simpleUnaryFunction "tan"
signalToGLSL (Tanh x) = signalToGLSL x >>= tanh 
signalToGLSL (Trunc x) = signalToGLSL x >>= trunc
signalToGLSL (Unipolar x) = signalToGLSL x >>= unipolar 

signalToGLSL (RtXy rt) = do
  rts <- signalToGLSL rt >>= alignVec2 
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  xs <- zipBinaryExpression (\r t -> "(" <> r <> "*cos(" <> t <> "))") rs ts
  ys <- zipBinaryExpression (\r t -> "(" <> r <> "*sin(" <> t <> "))") rs ts
  pure $ concat $ zipWith (\x y -> x `cons` singleton y) xs ys
  
signalToGLSL (RtX rt) = do
  rts <- signalToGLSL rt >>= alignVec2 
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  zipBinaryExpression (\r t -> "(" <> r <> "*cos(" <> t <> "))") rs ts

signalToGLSL (RtY rt) = do
  rts <- signalToGLSL rt >>= alignVec2 
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  zipBinaryExpression (\r t -> "(" <> r <> "*sin(" <> t <> "))") rs ts
  
signalToGLSL (XyRt xy) = do
  xys <- signalToGLSL xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  rs <- zipBinaryExpression (\x y -> "sqrt((" <> x <> "*" <> x <> ")+(" <> y <> "*" <> y <> "))") xs ys
  ts <- zipBinaryExpression (\x y -> "atan(" <> x <> "," <> y <> ")") xs ys
  pure $ concat $ zipWith (\x y -> x `cons` singleton y) rs ts

signalToGLSL (XyR xy) = do
  xys <- signalToGLSL xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  zipBinaryExpression (\x y -> "sqrt((" <> x <> "*" <> x <> ")+(" <> y <> "*" <> y <> "))") xs ys
  
signalToGLSL (XyT xy) = do
  xys <- signalToGLSL xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  zipBinaryExpression (\x y -> "atan(" <> x <> "," <> y <> ")") xs ys
  
signalToGLSL (Distance xy) = do
  fxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignVec2
  abFloatCombinatorial (\a b -> "distance(" <> a <> "," <> b <> ")") fxys xys

signalToGLSL (Prox xy) = do
  fxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignVec2
  abFloatCombinatorial (\a b -> "prox(" <> a <> "," <> b <> ")") fxys xys

{-
  SetFx Signal Signal | SetFy Signal Signal | SetFxy Signal Signal |
  Zoom Signal Signal | Move Signal Signal | Tile Signal Signal | Spin Signal Signal |

  Sum MultiMode Signal Signal |
  Difference MultiMode Signal Signal |
  Product MultiMode Signal Signal |
  Division MultiMode Signal Signal |
  Mod MultiMode Signal Signal |
  Pow MultiMode Signal Signal |
  Equal MultiMode Signal Signal |
  NotEqual MultiMode Signal Signal |
  GreaterThan MultiMode Signal Signal |
  GreaterThanOrEqual MultiMode Signal Signal |
  LessThan MultiMode Signal Signal |
  LessThanOrEqual MultiMode Signal Signal |
  Max MultiMode Signal Signal |
  Min MultiMode Signal Signal |
  Gate MultiMode Signal Signal |
  Circle Signal Signal |
  Rect Signal Signal |
  Clip Signal Signal |
  Between Signal Signal |
  VLine Signal Signal |
  HLine Signal Signal |
  Step Signal Signal |
  IfThenElse Signal Signal Signal | -- no pathways for this exist yet in PureScript port, from the AST level up
  ILine Signal Signal Signal |
  Line Signal Signal Signal |
  LinLin Signal Signal Signal |
  
  Point Signal |

-}

signalToGLSL _ = pure $ singleton $ zero


simpleUnaryFunction :: String -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
simpleUnaryFunction funcName = simpleUnaryExpression $ \x -> funcName <> "(" <> x <> ")"

simpleUnaryExpression :: (String -> String) -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
simpleUnaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

unaryExpression :: (String -> String) -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
unaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: false, deps: x.deps }

-- deduces type from type of first of each pair (which is assumed to match second of each pair)
zipBinaryExpression :: (String -> String -> String) -> NonEmptyList GLSLExpr -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
zipBinaryExpression f xs ys = pure $ zipWith (\x y -> { string: f x.string y.string, glslType:x.glslType, isSimple: false, deps: x.deps <> y.deps }) xs ys

abFloatCombinatorial :: (String -> String -> String) -> NonEmptyList GLSLExpr -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
abFloatCombinatorial f xs ys = pure $ do -- in NonEmptyList monad
  x <- xs
  y <- ys
  pure { string: f x.string y.string, glslType:Float, isSimple:false, deps: x.deps <> y.deps }

unipolar :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
unipolar = simpleUnaryExpression (\s -> "(" <> s <> "*0.5+0.5)")

blend :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- all Vec4
blend a b = do
  b' <- assign b
  alpha <- swizzleW b'
  pure $ ternaryFunction "mix" Vec4 a b' alpha

acosh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
acosh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "acosh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "-1.))")

asinh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
asinh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "asinh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "+1.))")

atanh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
atanh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "atanh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(log((1.+" <> x <> ")/(" <> "1.-" <> x <> "))/2.)") 

cosh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
cosh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "cosh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")+exp(" <> x <> "*-1.))/2.)") 

sinh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
sinh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "sinh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")-exp(" <> x <> "*-1.))/2.)") 

tanh :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
tanh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "tanh" xs
    false -> do
      xs' <- traverse assign xs
      sinhs <- sinh xs'
      coshs <- cosh xs'
      zipBinaryExpression (\x y -> "(" <> x <> "/" <> y <> ")") sinhs coshs
       
trunc :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
trunc xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "trunc" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(floor(abs(" <> x <> "))*sign(" <> x <> "))")

