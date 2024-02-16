module FragmentShader where

import Prelude(($),pure,show,bind,discard,(<>),(>>=),(<$>),(<<<),map,(==),(&&),otherwise,max)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (singleton,concat,fromList,zipWith,cons,head,tail,length)
import Data.List (List(..),(:))
import Data.List as List
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Foldable (fold,intercalate,foldM,elem)
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get,modify_)
import Data.Map (lookup)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tempo (Tempo)
import Data.DateTime (DateTime)

import NonEmptyList
import MultiMode (MultiMode(..))
import Signal (Signal(..))
import Action (Action,actionToTimes,actionHasVisualOutput)
import Output (Output(..))
import Program (Program)
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum,ternaryFunction,glslTypeToString,Exprs,exprsChannels,split,unsafeSwizzleX,unsafeSwizzleY,coerce)
import GLSLExpr as GLSLExpr
import GLSL (GLSL,align,alignNoExtend,assign,assignForced,swizzleX,swizzleY,swizzleZ,swizzleW,alignFloat,texture2D,textureFFT,alignVec2,alignVec3,alignVec4,alignRGBA,runGLSL,withFxys,extend,zipWithAAA,zipWithAAAA)


signalToGLSL :: GLSLType -> Signal -> GLSL Exprs

signalToGLSL _ (Constant x) = pure $ singleton $ GLSLExpr.float x

signalToGLSL ah (SignalList xs) = 
  case fromList xs of
    Nothing -> pure $ singleton $ zero
    Just xs' -> do
      case length xs' of
        1 -> signalToGLSL ah (head xs')
        _ -> do
          xs'' <- traverse (\x -> signalToGLSL Vec4 x >>= align Float) xs'
          alignNoExtend ah $ concat $ multi xs''

signalToGLSL ah (Append x y) = do
  xs <- signalToGLSL ah x
  ys <- signalToGLSL ah y
  pure $ xs <> ys

signalToGLSL ah (Zip x y) = do
  xs <- signalToGLSL ah x >>= alignFloat
  ys <- signalToGLSL ah y >>= alignFloat
  let (Tuple xs' ys') = extendToEqualLength xs ys
  pure $ concat $ zipWith (\anX anY -> anX `cons` singleton anY ) xs' ys'

signalToGLSL _ (Mono x) = do
  xs <- signalToGLSL Vec4 x
  let xs' = dotSum <$> xs
  let s = "(" <> intercalate " + " (_.string <$> xs') <> ")"
  pure $ singleton $ { string: s, glslType: Float, isSimple: false, deps: fold (_.deps <$> xs) }

signalToGLSL _ (Rep 0 _) = pure $ singleton $ zero
signalToGLSL ah (Rep n x) = (concat <<< replicate1 n) <$> signalToGLSL ah x

signalToGLSL _ Pi = pure $ singleton $ GLSLExpr.pi

signalToGLSL _ Px = pure $ singleton $ GLSLExpr.px

signalToGLSL _ Py = pure $ singleton $ GLSLExpr.py

signalToGLSL _ Pxy = pure $ singleton $ GLSLExpr.pxy

signalToGLSL _ Aspect = pure $ singleton $ GLSLExpr.aspect

signalToGLSL _ Fx = do
  s <- get
  pure $ singleton $ unsafeSwizzleX s.fxy

signalToGLSL _ Fy = do
  s <- get
  pure $ singleton $ unsafeSwizzleY s.fxy

signalToGLSL _ Fxy = do
  s <- get
  pure $ singleton s.fxy

signalToGLSL _ FRt = signalToGLSL Vec2 $ XyRt Fxy

signalToGLSL _ FR = signalToGLSL Float $ XyR Fxy

signalToGLSL _ FT = signalToGLSL Float $ XyT Fxy

signalToGLSL _ Lo = pure $ singleton $ simpleFromString Float "lo"

signalToGLSL _ Mid = pure $ singleton $ simpleFromString Float "mid"

signalToGLSL _ Hi = pure $ singleton $ simpleFromString Float "hi"

signalToGLSL _ ILo = pure $ singleton $ simpleFromString Float "ilo"

signalToGLSL _ IMid = pure $ singleton $ simpleFromString Float "imid"

signalToGLSL _ IHi = pure $ singleton $ simpleFromString Float "ihi"

signalToGLSL _ Cps = pure $ singleton $ simpleFromString Float "_cps"

signalToGLSL _ Time = pure $ singleton $ GLSLExpr.time

signalToGLSL _ Beat = pure $ singleton $ simpleFromString Float "_beat"

signalToGLSL _ ETime = pure $ singleton $ simpleFromString Float "_etime"

signalToGLSL _ EBeat = pure $ singleton $ simpleFromString Float "_ebeat"

signalToGLSL _ (FFT x) = signalToGLSL Float x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_fft")

signalToGLSL _ (IFFT x) = signalToGLSL Float x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_ifft")

signalToGLSL _ (Fb xy) = signalToGLSL Vec2 xy >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignVec2 >>= traverse (texture2D "_fb")

signalToGLSL _ Cam = do
  s <- get
  singleton <$> texture2D "_fb" s.fxy

signalToGLSL _ (Img url) = do
  s <- get
  case lookup url s.imgMap of
    Just n -> singleton <$> texture2D ("tex" <> show n) s.fxy
    Nothing -> pure $ singleton $ zero

signalToGLSL _ (Vid url) = do
  s <- get
  case lookup url s.vidMap of
    Just n -> singleton <$> texture2D ("tex" <> show n) s.fxy
    Nothing -> pure $ singleton $ zero


signalToGLSL _ (Blend x) = do
  xs <- signalToGLSL Vec4 x >>= alignRGBA
  case fromList (tail xs) of
    Nothing -> pure $ singleton $ head xs
    Just t -> singleton <$> foldM blend (head xs) t

signalToGLSL _ (RgbHsv x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv
signalToGLSL _ (HsvRgb x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb
signalToGLSL _ (HsvH x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleX
signalToGLSL _ (HsvS x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleY
signalToGLSL _ (HsvV x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL _ (HsvR x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleX
signalToGLSL _ (HsvG x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleY
signalToGLSL _ (HsvB x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleZ
signalToGLSL _ (RgbR x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleX
signalToGLSL _ (RgbG x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleY
signalToGLSL _ (RgbB x) = signalToGLSL Vec3 x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL _ (RgbH x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleX
signalToGLSL _ (RgbS x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleY
signalToGLSL _ (RgbV x) = signalToGLSL Vec3 x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleZ

signalToGLSL ah (Osc x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.osc
signalToGLSL ah (Tri x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.tri
signalToGLSL ah (Saw x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.saw
signalToGLSL ah (Sqr x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.sqr
signalToGLSL ah (LFTri x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.tri
signalToGLSL ah (LFSaw x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.saw
signalToGLSL ah (LFSqr x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.sqr

signalToGLSL ah (Abs x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.abs
signalToGLSL ah (Acos x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.acos
signalToGLSL ah (Acosh x) = signalToGLSL ah x >>= acosh
signalToGLSL ah (AmpDb x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.ampdb
signalToGLSL ah (Asin x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.asin
signalToGLSL ah (Asinh x) = signalToGLSL ah x >>= asinh
signalToGLSL ah (Atan x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.atan
signalToGLSL ah (Atanh x) = signalToGLSL ah x >>= atanh
signalToGLSL ah (Bipolar x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.bipolar
signalToGLSL ah (Cbrt x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.cbrt
signalToGLSL ah (Ceil x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.ceil
signalToGLSL ah (Cos x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.cos
signalToGLSL ah (Cosh x) = signalToGLSL ah x >>= cosh
signalToGLSL ah (CpsMidi x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.cpsmidi
signalToGLSL ah (DbAmp x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.dbamp
signalToGLSL ah (Exp x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.exp
signalToGLSL ah (Floor x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.floor
signalToGLSL ah (Fract x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.fract
signalToGLSL ah (Log x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.log
signalToGLSL ah (Log2 x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.log2
signalToGLSL ah (Log10 x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.log10 
signalToGLSL ah (MidiCps x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.midicps
signalToGLSL ah (Round x) = signalToGLSL ah x >>= round
signalToGLSL ah (Sign x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.sign
signalToGLSL ah (Sin x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.sin
signalToGLSL ah (Sinh x) = signalToGLSL ah x >>= sinh
signalToGLSL ah (Sqrt x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.sqrt
signalToGLSL ah (Tan x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.tan
signalToGLSL ah (Tanh x) = signalToGLSL ah x >>= tanh
signalToGLSL ah (Trunc x) = signalToGLSL ah x >>= trunc
signalToGLSL ah (Unipolar x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.unipolar

signalToGLSL _ (RtXy rt) = do
  rts <- signalToGLSL Vec2 rt >>= alignVec2
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  xs <- zipBinaryExpression (\r t -> "(" <> r <> "*cos(" <> t <> "))") rs ts
  ys <- zipBinaryExpression (\r t -> "(" <> r <> "*sin(" <> t <> "))") rs ts
  pure $ concat $ zipWith (\x y -> x `cons` singleton y) xs ys

signalToGLSL _ (RtX rt) = do
  rts <- signalToGLSL Vec2 rt >>= alignVec2
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  zipBinaryExpression (\r t -> "(" <> r <> "*cos(" <> t <> "))") rs ts

signalToGLSL _ (RtY rt) = do
  rts <- signalToGLSL Vec2 rt >>= alignVec2
  rs <- traverse swizzleX rts
  ts <- traverse swizzleY rts
  zipBinaryExpression (\r t -> "(" <> r <> "*sin(" <> t <> "))") rs ts

signalToGLSL _ (XyRt xy) = do
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  rs <- zipBinaryExpression (\x y -> "sqrt((" <> x <> "*" <> x <> ")+(" <> y <> "*" <> y <> "))") xs ys
  ts <- zipBinaryExpression (\x y -> "atan(" <> x <> "," <> y <> ")") xs ys
  pure $ concat $ zipWith (\x y -> x `cons` singleton y) rs ts

signalToGLSL _ (XyR xy) = do
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  zipBinaryExpression (\x y -> "sqrt((" <> x <> "*" <> x <> ")+(" <> y <> "*" <> y <> "))") xs ys

signalToGLSL _ (XyT xy) = do
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  xs <- traverse swizzleX xys
  ys <- traverse swizzleY xys
  zipBinaryExpression (\x y -> "atan(" <> x <> "," <> y <> ")") xs ys

signalToGLSL _ (Distance xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  pure $ map (GLSLExpr.distance fxy) xys

signalToGLSL _ (Prox xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  pure $ map (GLSLExpr.prox fxy) xys

signalToGLSL ah (SetFxy xy z) = do
  fxys <- signalToGLSL Vec2 xy >>= alignVec2 >>= traverse assignForced
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (SetFx x z) = do
  fxy <- _.fxy <$> get
  xs <- signalToGLSL Vec4 x >>= alignFloat
  let f x' = GLSLExpr.vec2binary x' (unsafeSwizzleY fxy)
  fxys <- traverse assignForced $ map f xs
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (SetFy y z) = do
  fxy <- _.fxy <$> get
  ys <- signalToGLSL Vec4 y >>= alignFloat
  let f y' = GLSLExpr.vec2binary (unsafeSwizzleX fxy) y'
  fxys <- traverse assignForced $ map f ys
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (Zoom xy z) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  fxys <- traverse assignForced $ map (GLSLExpr.division fxy) xys
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (Move xy z) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  fxys <- traverse assignForced $ map (GLSLExpr.difference fxy) xys
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (Tile xy z) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  fxys <- traverse assignForced $ map (GLSLExpr.tile fxy) xys
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (Spin x z) = do
  fxy <- _.fxy <$> get
  xs <- signalToGLSL Vec4 x >>= alignFloat
  fxys <- traverse (spin fxy) xs >>= traverse assignForced
  withFxys fxys $ signalToGLSL ah z

signalToGLSL ah (Sum mm x y) = binaryFunction ah mm GLSLExpr.sum x y
signalToGLSL ah (Difference mm x y) = binaryFunction ah mm GLSLExpr.difference x y
signalToGLSL ah (Product mm x y) = binaryFunction ah mm GLSLExpr.product x y
signalToGLSL ah (Division mm x y) = binaryFunction ah mm GLSLExpr.division x y
signalToGLSL ah (Mod mm x y) = binaryFunction ah mm GLSLExpr.mod x y
signalToGLSL ah (Pow mm x y) = binaryFunction ah mm GLSLExpr.pow x y
signalToGLSL ah (Equal mm x y) = binaryFunction ah mm GLSLExpr.equal x y
signalToGLSL ah (NotEqual mm x y) = binaryFunction ah mm GLSLExpr.notEqual x y
signalToGLSL ah (GreaterThan mm x y) = binaryFunction ah mm GLSLExpr.greaterThan x y
signalToGLSL ah (GreaterThanEqual mm x y) = binaryFunction ah mm GLSLExpr.greaterThanEqual x y
signalToGLSL ah (LessThan mm x y) = binaryFunction ah mm GLSLExpr.lessThan x y
signalToGLSL ah (LessThanEqual mm x y) = binaryFunction ah mm GLSLExpr.lessThanEqual x y
signalToGLSL ah (Max mm x y) = binaryFunction ah mm GLSLExpr.max x y
signalToGLSL ah (Min mm x y) = binaryFunction ah mm GLSLExpr.min x y

signalToGLSL ah (Gate mm x y) = do
  xs <- signalToGLSL ah x
  ys <- signalToGLSL ah y >>= traverse assign
  combineChannels mm GLSLExpr.gate xs ys

signalToGLSL ah (Clip mm r x) = clipEtcFunction ah mm GLSLExpr.clip r x
signalToGLSL ah (Between mm r x) = clipEtcFunction ah mm GLSLExpr.between r x
signalToGLSL ah (SmoothStep mm r x) = clipEtcFunction ah mm GLSLExpr.smoothstep r x

signalToGLSL ah (Circle mm xy d) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  ds <- case mm of
          Combinatorial -> signalToGLSL ah d
          Pairwise -> signalToGLSL Float d >>= alignFloat
  traverse assign $ combine mm (GLSLExpr.circle fxy) xys ds

signalToGLSL _ (Rect mm xy wh) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  whs <- signalToGLSL Vec2 wh >>= alignVec2
  rs <- sequence $ combine mm (rect fxy) xys whs
  traverse assign rs

signalToGLSL ah (VLine mm x w) = do
  fxy <- _.fxy <$> get
  xs <- signalToGLSL ah x
  ws <- signalToGLSL ah w
  combineChannels mm (GLSLExpr.vline fxy) xs ws >>= traverse assign

signalToGLSL ah (HLine mm x w) = do
  fxy <- _.fxy <$> get
  xs <- signalToGLSL ah x
  ws <- signalToGLSL ah w
  combineChannels mm (GLSLExpr.hline fxy) xs ws >>= traverse assign

signalToGLSL _ (Chain mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  ws <- signalToGLSL Vec4 w >>= alignFloat -- for now, have to align float because of monomorphic line function in shader, later maybe optimize
  let xys' = everyAdjacentPair xys -- NonEmptyList (Tuple GLSLExpr GLSLExpr) where GLSLExprs are all Vec2
  let f xyTuple theW = GLSLExpr.line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ combine mm f xys' ws

signalToGLSL _ (Lines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToGLSL Vec4 xy >>= alignVec4
  ws <- signalToGLSL Vec4 w >>= alignFloat
  let f v4 theW = GLSLExpr.line fxy (GLSLExpr.unsafeSwizzleXY v4) (GLSLExpr.unsafeSwizzleZW v4) theW
  traverse assign $ combine mm f xysVec4 ws

signalToGLSL _ (ILines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToGLSL Vec4 xy >>= alignVec4
  ws <- signalToGLSL Vec4 w >>= alignFloat
  let f v4 theW = GLSLExpr.iline fxy (GLSLExpr.unsafeSwizzleXY v4) (GLSLExpr.unsafeSwizzleZW v4) theW
  traverse assign $ combine mm f xysVec4 ws

signalToGLSL _ (Mesh mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  ws <- signalToGLSL Vec4 w >>= alignFloat
  let xys' = everyPair xys -- NonEmptyList (Tuple GLSLExpr GLSLExpr) where GLSLExprs are all Vec2
  let f xyTuple theW = GLSLExpr.line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ combine mm f xys' ws

signalToGLSL _ (ILine mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToGLSL Vec2 xy1 >>= alignVec2
  xy2s <- signalToGLSL Vec2 xy2 >>= alignVec2
  ws <- signalToGLSL Vec4 w >>= alignFloat
  traverse assign $ combine3 mm (GLSLExpr.iline fxy) xy1s xy2s ws

signalToGLSL _ (Line mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToGLSL Vec2 xy1 >>= alignVec2
  xy2s <- signalToGLSL Vec2 xy2 >>= alignVec2
  ws <- signalToGLSL Vec4 w >>= alignFloat
  traverse assign $ combine3 mm (GLSLExpr.line fxy) xy1s xy2s ws

signalToGLSL _ (Point xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToGLSL Vec2 xy >>= alignVec2
  traverse assign $ map (GLSLExpr.point fxy) xys

signalToGLSL ah (LinLin mm r1 r2 x) = do
  r1s <- signalToGLSL Vec2 r1 >>= alignVec2
  r2s <- signalToGLSL Vec2 r2 >>= alignVec2
  xs <- signalToGLSL ah x
  case mm of 
    Combinatorial -> pure $ do -- in NonEmptyList monad
      r1' <- r1s
      r2' <- r2s
      x' <- xs
      pure $ GLSLExpr.linlin r1' r2' x'
    Pairwise -> do
      case (length r1s == 1 && length r2s == 1) of
        true -> pure $ map (GLSLExpr.linlin (head r1s) (head r2s)) xs -- only one set of ranges so map onto xs as they are
        false -> do
          xs' <- alignFloat xs
          pure $ combine3Pairwise GLSLExpr.linlin r1s r2s xs'
  
signalToGLSL ah (Mix mm c t e) = do
  cs <- signalToGLSL ah c
  ts <- signalToGLSL ah t
  es <- signalToGLSL ah e
  combineChannels3 mm GLSLExpr.mix cs ts es
            
signalToGLSL _ (Seq steps y) = do
  steps' <- signalToGLSL Vec4 steps
  case exprsChannels steps' of
    1 -> pure steps'
    _ -> do
      let steps'' = concat $ map split steps' -- ? there is also splitIntoFloats in GLSL.purs which assigns, do we want to use that?
      ys <- signalToGLSL Float y >>= alignFloat
      pure $ map (GLSLExpr.seq steps'') ys
 
signalToGLSL _ _ = pure $ singleton $ zero

simpleUnaryFunction :: (GLSLExpr -> GLSLExpr) -> Exprs -> GLSL Exprs
simpleUnaryFunction f = traverse $ \x -> pure $ f x

simpleUnaryExpression :: (String -> String) -> Exprs -> GLSL Exprs
simpleUnaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

unaryExpression :: (String -> String) -> Exprs -> GLSL Exprs
unaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: false, deps: x.deps }

-- deduces type from type of first of each pair (which is assumed to match second of each pair)
zipBinaryExpression :: (String -> String -> String) -> Exprs -> Exprs -> GLSL Exprs
zipBinaryExpression f xs ys = pure $ zipWith (\x y -> { string: f x.string y.string, glslType:x.glslType, isSimple: false, deps: x.deps <> y.deps }) xs ys

binaryFunction :: GLSLType -> MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Signal -> Signal -> GLSL Exprs
binaryFunction ah mm f x y = do
  xs <- signalToGLSL ah x
  ys <- signalToGLSL ah y
  combineChannels mm f xs ys
    
combineChannels :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
combineChannels Combinatorial = combineChannelsCombinatorial
combineChannels Pairwise = combineChannelsPairwise

combineChannelsPairwise :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
combineChannelsPairwise f xs ys
  -- no need to extend if either of the inputs is a single Float...
  | length xs == 1 && (head xs).glslType == Float = combineChannelsCombinatorial f xs ys
  | length ys == 1 && (head ys).glslType == Float = combineChannelsCombinatorial f xs ys
  | otherwise = do
      -- extend xs and ys to equal length in channels
      let n = max (exprsChannels xs) (exprsChannels ys)
      xs' <- extend n xs
      ys' <- extend n ys
      zipWithAAA f xs' ys'

combineChannelsCombinatorial :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
combineChannelsCombinatorial f xs ys = do
  xs' <- alignFloat xs
  pure $ do -- in NonEmptyList monad
    x <- xs'
    y <- ys
    pure $ f x y

combineChannels3 :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> Exprs -> GLSL Exprs
combineChannels3 Combinatorial = combineChannelsCombinatorial3
combineChannels3 Pairwise = combineChannelsPairwise3

combineChannelsPairwise3 :: (GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> Exprs -> GLSL Exprs
combineChannelsPairwise3 f xs ys zs
  | length xs == 1 && (head xs).glslType == Float && length ys == 1 && (head ys).glslType == Float = pure $ map (f (head xs) (head ys)) zs
  | length xs == 1 && (head xs).glslType == Float && length zs == 1 && (head zs).glslType == Float = pure $ map (\y -> f (head xs) y (head zs)) ys
  | length ys == 1 && (head ys).glslType == Float && length zs == 1 && (head zs).glslType == Float = pure $ map (\x -> f x (head ys) (head zs)) xs
  | length xs == 1 && (head xs).glslType == Float = combineChannelsPairwise (f (head xs)) ys zs
  | length ys == 1 && (head ys).glslType == Float = combineChannelsPairwise (\x z -> f x (head ys) z) xs zs
  | length zs == 1 && (head zs).glslType == Float = combineChannelsPairwise (\x y -> f x y (head zs)) xs ys
  | otherwise = do
      let n = max (max (exprsChannels xs) (exprsChannels ys)) (exprsChannels zs)
      xs' <- extend n xs
      ys' <- extend n ys
      zs' <- extend n zs
      zipWithAAAA f xs' ys' zs'

combineChannelsCombinatorial3 :: (GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> Exprs -> GLSL Exprs
combineChannelsCombinatorial3 f xs ys zs = do
  xs' <- alignFloat xs
  ys' <- alignFloat ys
  pure $ do -- in NonEmptyList monad
    x <- xs'
    y <- ys'
    z <- zs
    pure $ f x y z
    
clipEtcFunction :: GLSLType -> MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Signal -> Signal -> GLSL Exprs
clipEtcFunction ah mm f r x = do
  rs <- signalToGLSL Vec2 r >>= alignVec2 >>= traverse assign
  xs <- signalToGLSL ah x
  case mm of
    Combinatorial -> pure $ do -- in NonEmptyList monad
      r' <- rs
      x' <- xs
      pure $ f r' x'
    Pairwise -> do
      case length rs == 1 of
        true -> pure $ map (f (head rs)) xs
        false -> do
          let n = max (length rs) (exprsChannels xs)
          let rs' = extendByRepetition n rs -- extend rs so that it has n *elements*
          xs' <- extend n xs -- extend xs so that it has n *channels*
          zipWithAAA f rs' xs'


blend :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- all Vec4
blend a b = do
  b' <- assign b
  alpha <- swizzleW b'
  pure $ ternaryFunction "mix" Vec4 a b' alpha

acosh :: Exprs -> GLSL Exprs
acosh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "acosh") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "-1.))")

asinh :: Exprs -> GLSL Exprs
asinh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "asinh") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "+1.))")

atanh :: Exprs -> GLSL Exprs
atanh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "atanh") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(log((1.+" <> x <> ")/(" <> "1.-" <> x <> "))/2.)")

cosh :: Exprs -> GLSL Exprs
cosh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "cosh") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")+exp(" <> x <> "*-1.))/2.)")

sinh :: Exprs -> GLSL Exprs
sinh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "sinh") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")-exp(" <> x <> "*-1.))/2.)")

tanh :: Exprs -> GLSL Exprs
tanh xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "tanh") xs
    false -> do
      xs' <- traverse assign xs
      sinhs <- sinh xs'
      coshs <- cosh xs'
      zipBinaryExpression (\x y -> "(" <> x <> "/" <> y <> ")") sinhs coshs

trunc :: Exprs -> GLSL Exprs
trunc xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "trunc") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(floor(abs(" <> x <> "))*sign(" <> x <> "))")

round :: Exprs -> GLSL Exprs
round xs = do
  s <- get
  case s.webGl2 of
    true -> pure $ map (GLSLExpr.simpleUnaryFunctionPure "round") xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(floor(" <> x <> ")+0.5)")

spin :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr
spin fxy a = do
  aPi <- assignForced $ GLSLExpr.product a GLSLExpr.pi
  ct <- assignForced $ GLSLExpr.cos aPi
  st <- assignForced $ GLSLExpr.sin aPi
  let x = GLSLExpr.difference (GLSLExpr.product (unsafeSwizzleX fxy) ct) (GLSLExpr.product (unsafeSwizzleY fxy) st)
  let y = GLSLExpr.sum (GLSLExpr.product (unsafeSwizzleY fxy) ct) (GLSLExpr.product (unsafeSwizzleX fxy) st)
  assignForced $ GLSLExpr.vec2binary x y
  
  
rect :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- Vec2 Vec2 Vec2 -> Float
rect fxy xy wh = do
  let a = GLSLExpr.product GLSLExpr.pxy (GLSLExpr.float 1.5)
  let b = GLSLExpr.abs $ GLSLExpr.difference fxy xy 
  let c = GLSLExpr.abs $ GLSLExpr.product wh (GLSLExpr.float 0.5)
  let d = GLSLExpr.difference b c
  let e = { string: "smoothstep(vec2(0.)," <> a.string <> "," <> d.string <> ")", glslType: Vec2, isSimple: false, deps: fxy.deps <> xy.deps <> wh.deps }
  f <- assign $ GLSLExpr.difference (GLSLExpr.float 1.0) e
  g <- swizzleX f
  h <- swizzleY f
  pure $ GLSLExpr.product g h
 
 
header :: String    
header = """precision mediump float;
#define PI 3.1415926535897932384626433832795
uniform lowp vec2 res;
uniform sampler2D _fb,_cam,_fft,_ifft;
uniform sampler2D tex0,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12;
uniform float lo,mid,hi,ilo,imid,ihi;
uniform float _defaultAlpha,_cps,_time,_etime,_beat,_ebeat;
vec3 hsvrgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}
vec3 rgbhsv(vec3 c){
  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);}
float iline(vec2 xy1,vec2 xy2,float w,vec2 fxy) {
  fxy -= xy1, xy2 -= xy1;
  float h = dot(fxy,xy2)/dot(xy2,xy2);
  float aa = min(((1.5/res.x)+(1.5/res.y))*0.5,w);
  return smoothstep(aa,0.,length(fxy - xy2 * h)-(w*0.5));}
float line(vec2 xy1,vec2 xy2,float w,vec2 fxy) {
  fxy -= xy1, xy2 -= xy1;
  float h = clamp(dot(fxy,xy2)/dot(xy2,xy2),0.,1.);
  float aa = min(((1.5/res.x)+(1.5/res.y))*0.5,w);
  return smoothstep(aa,0.,length(fxy - xy2 * h)-(w*0.5));}
void main() {
"""
-- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the HSV-RGB conversion algorithms above!


programsToGLSL :: Tempo -> {- Map TextureRef Int -> -} Program -> Program -> GLSL GLSLExpr
programsToGLSL tempo oldProgram newProgram = do
  let oldActions = map onlyVideoOutputs oldProgram.actions
  let newActions = map onlyVideoOutputs newProgram.actions
  fxy <- assignForced GLSLExpr.defaultFxy
  modify_ $ \s -> s { fxy = fxy }
  rgbas <- traverseActions tempo newProgram.evalTime oldActions newActions -- List GLSLExpr
  case List.head rgbas of
    Nothing -> pure $ coerce Vec4 zero
    Just h -> do
      case List.tail rgbas of
        Nothing -> pure h
        Just t -> foldM blend h t

onlyVideoOutputs :: Maybe Action -> Maybe Action
onlyVideoOutputs Nothing = Nothing
onlyVideoOutputs (Just x) = if actionHasVisualOutput x then Just x else Nothing

traverseActions :: Tempo -> DateTime -> List (Maybe Action) -> List (Maybe Action) -> GLSL (List GLSLExpr)
traverseActions _ _ Nil Nil = pure List.Nil
traverseActions tempo eTime (x:xs) Nil = do
  mh <- actionsToGLSL tempo eTime x Nothing 
  t <- traverseActions tempo eTime xs Nil
  case mh of
    Just h -> pure (h : t)
    Nothing -> pure t
traverseActions tempo eTime Nil (y:ys) = do
  mh <- actionsToGLSL tempo eTime Nothing y 
  t <- traverseActions tempo eTime Nil ys
  case mh of
    Just h -> pure (h : t)
    Nothing -> pure t
traverseActions tempo eTime (x:xs) (y:ys) = do
  mh <- actionsToGLSL tempo eTime x y
  t <- traverseActions tempo eTime xs ys
  case mh of
    Just h -> pure (h : t)
    Nothing -> pure t
     
actionsToGLSL :: Tempo -> DateTime -> Maybe Action -> Maybe Action -> GLSL (Maybe GLSLExpr)
actionsToGLSL _ _ Nothing Nothing = pure $ Just (GLSLExpr.float 3.4)
actionsToGLSL tempo eTime Nothing (Just new) = do
  rgba <- actionToGLSL new
  let Tuple t0 t1 = actionToTimes tempo eTime new
  Just <$> assignForced (GLSLExpr.product rgba $ GLSLExpr.fadeIn t0 t1)
actionsToGLSL tempo eTime (Just old) Nothing = do
  rgba <- actionToGLSL old
  let Tuple t0 t1 = actionToTimes tempo eTime old
  Just <$> assignForced (GLSLExpr.product rgba $ GLSLExpr.fadeOut t0 t1)
actionsToGLSL tempo eTime (Just old) (Just new) = do
  case old == new of
    true -> Just <$> (actionToGLSL new >>= assignForced)
    false -> do
      rgbaOld <- actionToGLSL old
      rgbaNew <- actionToGLSL new
      let Tuple t0 t1 = actionToTimes tempo eTime new
      rgbaOld' <- assignForced (GLSLExpr.product rgbaOld $ GLSLExpr.fadeOut t0 t1)
      rgbaNew' <- assignForced (GLSLExpr.product rgbaNew $ GLSLExpr.fadeIn t0 t1)
      Just <$> assignForced (GLSLExpr.sum rgbaOld' rgbaNew')
    
actionToGLSL :: Action -> GLSL GLSLExpr
actionToGLSL x = do
  case elem RGBA x.outputs of
    true -> signalToGLSL Vec4 x.signal >>= exprsRGBAToRGBA
    false -> signalToGLSL Vec3 x.signal >>= exprsRGBToRGBA
    
exprsRGBToRGBA :: Exprs -> GLSL GLSLExpr
exprsRGBToRGBA xs = do
  xs' <- alignVec3 xs
  rgb <- foldM (\b a -> assignForced $ GLSLExpr.sum b a) (head xs') (tail xs')
  assignForced $ GLSLExpr.vec4binary rgb (GLSLExpr.float 1.0)
  
exprsRGBAToRGBA :: Exprs -> GLSL GLSLExpr
exprsRGBAToRGBA xs = do
  xs' <- alignRGBA xs
  case fromList (tail xs') of
    Nothing -> pure $ head xs'
    Just t -> foldM blend (head xs') t

fragmentShader :: Boolean -> Tempo -> {- Map TextureRef Int -> -} Program -> Program -> String
fragmentShader webGl2 tempo oldProgram newProgram = header <> assignments <> gl_FragColor <> "}"
  where
    (Tuple a st) = runGLSL webGl2 $ programsToGLSL tempo oldProgram newProgram
    assignments = fold $ mapWithIndex indexedGLSLExprToString st.exprs
    gl_FragColor = "gl_FragColor = " <> a.string <> ";\n"

indexedGLSLExprToString :: Int -> GLSLExpr -> String
indexedGLSLExprToString n x = glslTypeToString x.glslType <> " _" <> show n <> " = " <> x.string <> ";\n"

{-
testCodeGen :: Boolean -> Signal -> String
testCodeGen webGl2 x = assignments <> lastExprs
  where
    (Tuple a st) = runGLSL webGl2 (signalToGLSL x)
    assignments = fold $ mapWithIndex indexedGLSLExprToString st.exprs
    lastExprs = fold $ map (\y -> y.string <> " :: " <> show y.glslType <> "\n") a
-}

