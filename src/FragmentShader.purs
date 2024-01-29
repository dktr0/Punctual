module FragmentShader where

import Prelude(($),pure,show,bind,(<>),(>>=),(<$>),(<<<),map)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (NonEmptyList,singleton,concat,fromList,zipWith,cons)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold,intercalate)
import Data.Set as Set
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get)

import NonEmptyList
import Signal (Signal(..))
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum)
import GLSL (GLSL,assign,swizzleX,swizzleY,alignFloat,texture2D,textureFFT,alignVec2)


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

signalToGLSL (FFT x) = signalToGLSL x >>= traverse assign >>= alignFloat >>= traverse (textureFFT "_fft")

signalToGLSL (IFFT x) = signalToGLSL x >>= traverse assign >>= alignFloat >>= traverse (textureFFT "_ifft")

signalToGLSL (Fb xy) = signalToGLSL xy >>= traverse assign >>= alignVec2 >>= traverse (texture2D "_fb")

signalToGLSL Cam = do
  s <- get
  traverse (texture2D "_fb") s.fxys

{-
  Img String
  Vid String |
-}

signalToGLSL (Mono x) = do
  xs <- signalToGLSL x
  let xs' = dotSum <$> xs
  let s = "(" <> intercalate " + " (_.string <$> xs') <> ")"
  pure $ singleton $ { string: s, glslType: Float, isSimple: false, deps: fold (_.deps <$> xs) }

signalToGLSL (Rep 0 _) = pure $ singleton $ zero
signalToGLSL (Rep n x) = (concat <<< replicate1 n) <$> signalToGLSL x

signalToGLSL (Bipolar x) = simpleUnaryFunction x $ \s -> "(" <> s <> "*2.-1.)"

signalToGLSL (Unipolar x) = simpleUnaryFunction x $ \s -> "(" <> s <> "*0.5+0.5)"



{-
  Blend Signal |
  RgbHsv Signal | HsvRgb Signal |
  HsvH Signal | HsvS Signal | HsvV Signal | HsvR Signal | HsvG Signal | HsvB Signal |
  RgbH Signal | RgbS Signal | RgbV Signal | RgbR Signal | RgbG Signal | RgbB Signal |
  -- oscillators
  Osc Signal | Tri Signal | Saw Signal | Sqr Signal | LFTri Signal | LFSaw Signal | LFSqr Signal |
  -- unary Math functions based on (or emulating) JavaScript Math unary functions
  Abs Signal |
  Acos Signal |
  Acosh Signal |
  Asin Signal |
  Asinh Signal |
  Atan Signal |
  Atanh Signal |
  Cbrt Signal |
  Ceil Signal |
  Cos Signal |
  Cosh Signal |
  Exp Signal |
  Floor Signal |
  Log Signal |
  Log2 Signal |
  Log10 Signal |
  Round Signal |
  Sign Signal |
  Sin Signal |
  Sinh Signal |
  Sqrt Signal |
  Tan Signal |
  Tanh Signal |
  Trunc Signal |
  -- other unary functions
  RtXy Signal | -- polar to cartesian conversion
  RtX Signal | -- x = r * cos theta
  RtY Signal | -- y = r * sin theta
  XyRt Signal | -- cartesian to polar conversion
  XyR Signal | -- r = sqrt (x^2 + y ^2)
  XyT Signal | -- theta = atan2(y,x)
  Point Signal |
  Distance Signal |
  Prox Signal |
  MidiCps Signal |
  CpsMidi Signal |
  DbAmp Signal |
  AmpDb Signal |
  Fract Signal |
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
  LPF Signal Signal Signal | HPF Signal Signal Signal | BPF Signal Signal Signal |
  Delay Number Signal Signal
-}

signalToGLSL _ = pure $ singleton $ zero


simpleUnaryFunction :: Signal -> (String -> String) -> GLSL (NonEmptyList GLSLExpr)
simpleUnaryFunction x f = do
  xs <- signalToGLSL x
  pure $ map (\e -> { string: f e.string, glslType: e.glslType, isSimple: e.isSimple, deps: e.deps }) xs
