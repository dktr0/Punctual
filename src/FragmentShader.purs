module FragmentShader where

import Prelude(($),pure,show,bind,(<>),(>>=),(<$>),(<<<))
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (NonEmptyList,singleton,concat,fromList,zipWith,cons,head,tail)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold,intercalate,foldM)
import Data.Set as Set
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get)
import Data.Map (lookup)

import NonEmptyList
import Signal (Signal(..),MultiMode(..))
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum,ternaryFunction)
import GLSL (GLSL,assign,swizzleX,swizzleY,swizzleZ,swizzleW,alignFloat,texture2D,textureFFT,alignVec2,alignVec3,alignRGBA)


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

signalToGLSL (FFT x) = signalToGLSL x >>= traverse assign >>= alignFloat >>= traverse (textureFFT "_fft")

signalToGLSL (IFFT x) = signalToGLSL x >>= traverse assign >>= alignFloat >>= traverse (textureFFT "_ifft")

signalToGLSL (Fb xy) = signalToGLSL xy >>= traverse assign >>= alignVec2 >>= traverse (texture2D "_fb")

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

signalToGLSL (Osc x) = signalToGLSL x >>= (simpleUnaryExpression $ \e -> e <> "*PI*2.0*_time") >>= simpleUnaryFunction "sin"
signalToGLSL (Tri x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "tri"
signalToGLSL (Saw x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "saw"
signalToGLSL (Sqr x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "sqr"
signalToGLSL (LFTri x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "tri"
signalToGLSL (LFSaw x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "saw"
signalToGLSL (LFSqr x) = signalToGLSL x >>= alignFloat >>= simpleUnaryFunction "sqr"

signalToGLSL (Abs x) = signalToGLSL x >>= simpleUnaryFunction "abs"
signalToGLSL (Acos x) = signalToGLSL x >>= simpleUnaryFunction "acos"
signalToGLSL (Acosh x) = signalToGLSL x >>= traverse assign >>= simpleUnaryExpression (\s -> "log(" <> s <> "+sqrt(" <> s <> "*" <> s <> "-1.))") -- later: use built-in on WebGL2 
signalToGLSL (Asin x) = signalToGLSL x >>= simpleUnaryFunction "asin"
signalToGLSL (Asinh x) = signalToGLSL x >>= traverse assign >>= simpleUnaryExpression (\s -> "log(" <> s <> "+sqrt(" <> s <> "*" <> s <> "+1.))") -- later: use built-in on WebGL2 
signalToGLSL (Atan x) = signalToGLSL x >>= simpleUnaryFunction "atan"
signalToGLSL (Atanh x) = signalToGLSL x >>= traverse assign >>= simpleUnaryExpression (\s -> "(log((1.+" <> s <> ")/(" <> "1.-" <> s <> "))/2.)") -- later: use built-in on WebGL2
signalToGLSL (Bipolar x) = signalToGLSL x >>= simpleUnaryExpression (\e -> "(" <> e <> "*2.-1.)")
signalToGLSL (Cbrt x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "pow(" <> s <> ",0.3333333333)")
signalToGLSL (Ceil x) = signalToGLSL x >>= simpleUnaryFunction "ceil"
signalToGLSL (Cos x) = signalToGLSL x >>= simpleUnaryFunction "cos"
signalToGLSL (Cosh x) = signalToGLSL x >>= traverse assign >>= simpleUnaryExpression (\s -> "((exp(" <> s <> ")+exp(" <> s <> "*-1.))/2.)") -- later: use built-in on WebGL2
signalToGLSL (Exp x) = signalToGLSL x >>= simpleUnaryFunction "exp"
signalToGLSL (Floor x) = signalToGLSL x >>= simpleUnaryFunction "floor"
signalToGLSL (Log x) = signalToGLSL x >>= simpleUnaryFunction "log"
signalToGLSL (Log2 x) = signalToGLSL x >>= simpleUnaryFunction "log2"
signalToGLSL (Log10 x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(log(" <> s <> ")/log(10.))")
signalToGLSL (Round x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(floor(" <> s <> ")+0.5)")
signalToGLSL (Sign x) = signalToGLSL x >>= simpleUnaryFunction "sign"
signalToGLSL (Sin x) = signalToGLSL x >>= simpleUnaryFunction "sin"
signalToGLSL (Sinh x) = signalToGLSL x >>= traverse assign >>= simpleUnaryExpression (\s -> "((exp(" <> s <> ")-exp(" <> s <> "*-1.))/2.)") -- later: use built-in on WebGL2
signalToGLSL (Sqrt x) = signalToGLSL x >>= simpleUnaryFunction "sqrt"
signalToGLSL (Tan x) = signalToGLSL x >>= simpleUnaryFunction "tan"
signalToGLSL (Tanh x) = signalToGLSL $ Division Pairwise (Sinh x) (Cosh x) -- later: use built-in on WebGL2 and rework WebGL1 to reuse xs
signalToGLSL (Trunc x) = signalToGLSL $ Product Pairwise (Floor (Abs x)) (Sign x) -- later: use built-in on WebGL2 and rework WebGL1 to reuse xs
signalToGLSL (Unipolar x) = signalToGLSL x >>= simpleUnaryExpression (\s -> "(" <> s <> "*0.5+0.5)")

{-
  -- more unary functions
  RtXy Signal | -- polar to cartesian conversion
  RtX Signal | -- x = r * cos theta
  RtY Signal | -- y = r * sin theta
  XyRt Signal | -- cartesian to polar conversion
  XyR Signal | -- r = sqrt (x^2 + y ^2)
  XyT Signal | -- theta = atan2(y,x)
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
  
    Point Signal |

-}

signalToGLSL _ = pure $ singleton $ zero


simpleUnaryFunction :: String -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
simpleUnaryFunction funcName = simpleUnaryExpression $ \x -> funcName <> "(" <> x <> ")"

simpleUnaryExpression :: (String -> String) -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
simpleUnaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

blend :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- all Vec4
blend a b = do
  b' <- assign b
  alpha <- swizzleW b'
  pure $ ternaryFunction "mix" Vec4 a b' alpha

