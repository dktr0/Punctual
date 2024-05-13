module FragmentShader where

import Prelude(($),pure,show,bind,discard,(<>),(>>=),(<$>),(<<<),map,(==),(&&),otherwise,max)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (singleton,concat,fromList,zipWith,cons,head,tail,length)
import Data.List (List(..),(:))
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Foldable (fold,intercalate,foldM)
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get,modify_)
import Data.Map (Map,lookup)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tempo (Tempo)
import Data.DateTime (DateTime)

import NonEmptyList
import MultiMode (MultiMode(..))
import Signal (Signal(..))
import Action (Action,actionTimesAsSecondsSinceEval)
import Output (Output)
import Output as Output
import Program (Program)
import Expr
import G
import Multi (Multi,fromNonEmptyListMulti)


signalToExprs :: forall a. Expr a => Signal -> G (Multi a)

signalToExprs (Constant x) = pure $ pure $ constant x

signalToExprs (SignalList xs) = 
  case fromList xs of
    Nothing -> pure $ pure $ constant 0.0
    Just xs' -> do
      xs'' <- fromNonEmptyListMulti <$> traverse signalToExprs xs' -- :: Multi Float, with list already combinatorially expanded
      pure $ floatsToExprs xs''

signalToExprs (Append x y) = do
  xs <- signalToExprs x
  ys <- signalToExprs y
  pure $ xs <> ys

{-
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

signalToGLSL ah (Rep n x) = (concat <<< replicate1 n) <$> signalToGLSL ah x
-}

signalToExprs Pi = pure $ pure $ fromFloat $ FloatExpr "PI"

signalToExprs Px = pure $ pure $ fromFloat $ FloatExpr "(2./res.x)"

signalToExprs Py = pure $ pure $ fromFloat $ FloatExpr "(2./res.y)"

{-
signalToExprs Pxy = pure $ pure $ ... $ Vec2Expr "(2./res)"
-- has to be realigned somehow
-}

signalToExprs Aspect = pure $ pure $ fromFloat $ FloatExpr "(res.x/res.y)"

signalToExprs Fx = (pure <<< fromFloat <<< swizzleX <<< _.fxy) <$> get

signalToExprs Fy = (pure <<< fromFloat <<< swizzleY <<< _.fxy) <$> get

{-
signalToGLSL _ Fxy = (singleton <<< _.fxy) <$> get
-- has to be realigned from Vec2 to whatever requested type is
-}

signalToExprs FRt = signalToExprs $ XyRt Fxy

signalToExprs FR = signalToExprs $ XyR Fxy

signalToExprs FT = signalToExprs $ XyT Fxy

signalToExprs Lo = pure $ pure $ fromFloat $ FloatExpr "lo"

signalToExprs Mid = pure $ pure $ fromFloat $ FloatExpr "mid"

signalToExprs Hi = pure $ pure $ fromFloat $ FloatExpr "hi"

signalToExprs ILo = pure $ pure $ fromFloat $ FloatExpr "ilo"

signalToExprs IMid = pure $ pure $ fromFloat $ FloatExpr "imid"

signalToExprs IHi = pure $ pure $ fromFloat $ FloatExpr "ihi"

signalToExprs Cps = pure $ pure $ fromFloat $ FloatExpr "_cps"

signalToExprs Time = (pure <<< fromFloat <<< _.time) <$> get

signalToExprs Beat = (pure <<< fromFloat <<< _.beat) <$> get

signalToExprs ETime = (pure <<< fromFloat <<< _.etime) <$> get

signalToExprs EBeat = (pure <<< fromFloat <<< _.ebeat) <$> get

{-
signalToGLSL _ (FFT x) = signalToGLSL Float x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "o")

signalToGLSL _ (IFFT x) = signalToGLSL Float x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "i")

signalToGLSL _ (Fb xy) = signalToGLSL Vec2 xy >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignVec2 >>= traverse (texture2D "f")

signalToGLSL _ Cam = do
  s <- get
  t <- texture2D "w" (GLSLExpr.unipolar s.fxy)
  r <- assignForced $ GLSLExpr.lessThanEqual (GLSLExpr.abs s.fxy) (GLSLExpr.float 1.0)  
  pure $ singleton $ GLSLExpr.product t (GLSLExpr.product (unsafeSwizzleX r) (unsafeSwizzleY r))

signalToGLSL _ (Img url) = do
  s <- get
  case lookup url s.imgMap of
    Just n -> do
      t <- texture2D ("t" <> show n) (GLSLExpr.unipolar s.fxy)
      r <- assignForced $ GLSLExpr.lessThanEqual (GLSLExpr.abs s.fxy) (GLSLExpr.float 1.0)
      pure $ singleton $ GLSLExpr.product t (GLSLExpr.product (unsafeSwizzleX r) (unsafeSwizzleY r))
    Nothing -> pure $ singleton $ zero

signalToGLSL _ (Vid url) = do
  s <- get
  case lookup url s.vidMap of
    Just n -> do
      t <- texture2D ("t" <> show n) (GLSLExpr.unipolar s.fxy)
      r <- assignForced $ GLSLExpr.lessThanEqual (GLSLExpr.abs s.fxy) (GLSLExpr.float 1.0)
      pure $ singleton $ GLSLExpr.product t (GLSLExpr.product (unsafeSwizzleX r) (unsafeSwizzleY r))
    Nothing -> pure $ singleton $ zero

signalToGLSL _ (Blend x) = do
  xs <- signalToGLSL Vec4 x >>= alignRGBA
  case fromList (tail xs) of
    Nothing -> pure $ singleton $ head xs
    Just t -> singleton <$> foldM blend (head xs) t
    
signalToGLSL _ (Add x) = do
  xs <- signalToGLSL Vec3 x >>= alignVec3
  case fromList (tail xs) of
    Nothing -> pure $ singleton $ head xs
    Just t -> singleton <$> foldM add (head xs) t
    
signalToGLSL _ (Mul x) = do
  xs <- signalToGLSL Vec3 x >>= alignVec3
  case fromList (tail xs) of
    Nothing -> pure $ singleton $ head xs
    Just t -> singleton <$> foldM mul (head xs) t

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

signalToGLSL ah (Osc x) = signalToGLSL ah x >>= traverse osc
signalToGLSL ah (Tri x) = signalToGLSL ah x >>= traverse tri
signalToGLSL ah (Saw x) = signalToGLSL ah x >>= traverse saw
signalToGLSL ah (Sqr x) = signalToGLSL ah x >>= traverse sqr
signalToGLSL ah (LFTri x) = signalToGLSL ah x >>= traverse tri
signalToGLSL ah (LFSaw x) = signalToGLSL ah x >>= traverse saw
signalToGLSL ah (LFSqr x) = signalToGLSL ah x >>= traverse sqr

signalToGLSL ah (Abs x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.abs
signalToGLSL ah (Acos x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.acos
signalToGLSL ah (Acosh x) = signalToGLSL ah x >>= acosh
signalToGLSL ah (AmpDb x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.ampdb
signalToGLSL ah (Asin x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.asin
signalToGLSL ah (Asinh x) = signalToGLSL ah x >>= asinh
signalToGLSL ah (Atan x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.atan
signalToGLSL ah (Atanh x) = signalToGLSL ah x >>= atanh
signalToGLSL ah (Bipolar x) = signalToGLSL ah x >>= simpleUnaryFunction GLSLExpr.bipolar
signalToGLSL ah (Cbrt x) = signalToGLSL ah x >>= cbrt
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
  ts <- zipBinaryExpression (\x y -> "atan(" <> y <> "," <> x <> ")") xs ys
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
  zipBinaryExpression (\x y -> "atan(" <> y <> "," <> x <> ")") xs ys

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

signalToGLSL ah (Early x z) = do
  xs <- signalToGLSL Float x >>= alignFloat
  s <- get  
  let xs' = mapFlipped xs $ \y -> {
    time: GLSLExpr.add s.time y,
    beat: GLSLExpr.add s.beat $ GLSLExpr.product y $ simpleFromString Float "_cps",
    etime: GLSLExpr.add s.etime y,
    ebeat: GLSLExpr.add s.ebeat $ GLSLExpr.product y $ simpleFromString Float "_cps"
    }
  withAlteredTime xs' $ signalToGLSL ah z
  
signalToGLSL ah (Slow x z) = do
  xs <- signalToGLSL Float x >>= alignFloat
  s <- get
  let xs' = mapFlipped xs $ \y -> {
    time: GLSLExpr.division s.time y,
    beat: GLSLExpr.division s.beat y,
    etime: GLSLExpr.division s.etime y,
    ebeat: GLSLExpr.division s.ebeat y
    }
  withAlteredTime xs' $ signalToGLSL ah z

signalToGLSL ah (Addition mm x y) = binaryFunction ah mm GLSLExpr.add x y
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
  
signalToGLSL ah (Mix mm x y a) = do
  x' <- signalToGLSL ah x
  y' <- signalToGLSL ah y
  xys <- extendAligned x' y' -- :: NonEmptyList (Tuple GLSLExpr GLSLExpr)
  a' <- signalToGLSL ah a >>= alignFloat
  sequence $ combine mm (\(Tuple xx yy) aa -> pure $ GLSLExpr.mix xx yy aa) xys a'
            
signalToGLSL _ (Seq steps) = do
  steps' <- signalToGLSL Float steps >>= alignFloat
  case length steps' of
    1 -> pure steps'
    _ -> do
      b <- _.beat <$> get
      b' <- assignForced $ GLSLExpr.fract b
      singleton <$> assignForced (GLSLExpr.seq steps' b')
 
-} 

signalToExprs _ = pure $ pure $ constant 0.0

{-
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
  xs <- signalToGLSL ah x >>= traverse assignForced
  ys <- signalToGLSL ah y >>= traverse assignForced
  combineChannels mm f xs ys
    
combineChannels :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
combineChannels Combinatorial = combineChannelsCombinatorial
combineChannels Pairwise = combineChannelsPairwise

combineChannelsPairwise :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
combineChannelsPairwise f xs ys
  -- no need to extend if either of the inputs is a single Float...
  | length xs == 1 && (head xs).glslType == Float = pure $ map (f (head xs)) ys
  | length ys == 1 && (head ys).glslType == Float = pure $ map (\x -> f x (head ys)) xs
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
          xs' <- extend n xs >>= alignFloat -- extend xs so that it has n *channels*, and align to floats so that each channel pairs with an element from rs'
          sequence $ zipWith (\rr xx -> assignForced $ GLSLExpr.smoothstep rr xx) rs' xs'


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
    
cbrt :: Exprs -> GLSL Exprs
cbrt xs = traverse assign xs >>= simpleUnaryExpression (\x -> "(exp(log(abs(" <> x <> "))/3.)*sign(" <> x <> "))")
  
spin :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr
spin fxy a = do
  aPi <- assignForced $ GLSLExpr.product a GLSLExpr.pi
  ct <- assignForced $ GLSLExpr.cos aPi
  st <- assignForced $ GLSLExpr.sin aPi
  let x = GLSLExpr.difference (GLSLExpr.product (unsafeSwizzleX fxy) ct) (GLSLExpr.product (unsafeSwizzleY fxy) st)
  let y = GLSLExpr.add (GLSLExpr.product (unsafeSwizzleY fxy) ct) (GLSLExpr.product (unsafeSwizzleX fxy) st)
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
-} 
 
header :: Boolean -> String    
header true = webGL2HeaderPrefix <> commonHeader
header false = webGL1HeaderPrefix <> commonHeader
    
webGL2HeaderPrefix :: String
webGL2HeaderPrefix = """#version 300 es
precision mediump float;
layout(location=0) out vec4 fragColor;
"""

webGL1HeaderPrefix :: String
webGL1HeaderPrefix = """precision mediump float;
"""

commonHeader :: String
commonHeader = """uniform lowp vec2 res;
uniform sampler2D f,o,i,w,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15;
uniform float lo,mid,hi,ilo,imid,ihi,_defaultAlpha,_cps,_time,_etime,_beat,_ebeat;
#define PI 3.1415926535897932384626433832795
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

programsToGLSL :: Tempo -> Program -> Program -> G Vec4
programsToGLSL tempo oldProgram newProgram = do
  fxy <- (_.fxy <$> get) >>= assign 
  modify_ $ \s -> s { fxy = fxy }
  pure $ constant 0.0 -- PLACEHOLDER 
  {-
  mExpr <- foldActions tempo newProgram.evalTime Nothing oldProgram.actions newProgram.actions
  case mExpr of
    Nothing -> pure $ constant 0.0
    Just (Left v3) -> pure 
    Just (Right v4) -> pure v4 -}

{-
foldActions :: Tempo -> DateTime -> Maybe (Either Vec3 Vec4) -> List (Maybe Action) -> List (Maybe Action) -> G (Maybe (Either Vec3 Vec4))
foldActions _ _ prevOutputExpr _ Nil = pure prevOutputExpr
foldActions tempo eTime prevOutputExpr Nil (y:ys) = do
  mExpr <- appendActions tempo eTime prevOutputExpr Nothing y 
  foldActions tempo eTime mExpr Nil ys
foldActions tempo eTime prevOutputExpr (x:xs) (y:ys) = do
  mExpr <- appendActions tempo eTime prevOutputExpr x y
  foldActions tempo eTime mExpr xs ys
     
appendActions :: Tempo -> DateTime -> Maybe (Either Vec3 Vec4) -> Maybe Action -> Maybe Action -> G (Maybe (Either Vec3 Vec4))
appendActions _ _ prevOutputExpr _ Nothing = pure prevOutputExpr
appendActions tempo eTime prevOutputExpr mOldAction (Just newAction) = do
  mNewExpr <- actionToGLSL newAction.output newAction
  case mNewExpr of 
    Nothing -> pure prevOutputExpr
    Just newExpr -> do
      let Tuple t0 t1 = actionTimesAsSecondsSinceEval tempo eTime newAction
      newExpr' <- assignForced $ GLSLExpr.product newExpr $ GLSLExpr.fadeIn t0 t1
      case mOldAction of
        Nothing -> appendExpr newAction.output prevOutputExpr newExpr'
        Just oldAction -> do
          mOldExpr <- actionToGLSL newAction.output oldAction
          case mOldExpr of
            Nothing -> appendExpr newAction.output prevOutputExpr newExpr'
            Just oldExpr -> do
              oldExpr' <- assignForced $ GLSLExpr.product oldExpr $ GLSLExpr.fadeOut t0 t1
              expr <- assignForced (GLSLExpr.add newExpr' oldExpr')
              appendExpr newAction.output prevOutputExpr expr
-}

{-
actionToGLSL :: Output -> Action -> G (Maybe (Either Vec3 Vec4))
actionToGLSL Output.Audio _ = pure Nothing
actionToGLSL Output.Blend a = do
  xs <- flatten <$> (signalToExprs a.signal :: G Vec4)
  Just <$> foldM (\x y -> blend x y >>= assign) (head xs) (tail xs)
actionToGLSL Output.RGBA a = do
  xs <- flatten <$> (signalToExprs a.signal :: G Vec4)
  Just <$> foldM (\x y -> blend x y >>= assign) (head xs) (tail xs)
actionToGLSL _ a = do
  xs <- flatten <$> (signalToExprs a.signal :: G Vec3)
  Just <$> foldM (\x y -> assign $ add x y) (head xs) (tail xs)
-}

{-   
appendExpr :: Output -> Maybe GLSLExpr -> GLSLExpr -> GLSL (Maybe GLSLExpr)
appendExpr Output.Audio x _ = pure x
appendExpr _ Nothing x = pure $ Just x
appendExpr Output.RGBA (Just _) x = pure $ Just x
appendExpr Output.RGB (Just _) x = pure $ Just x
appendExpr Output.Blend (Just prevExpr) x = do
  let prevRGBA = case GLSLExpr.exprChannels prevExpr of
                   3 -> GLSLExpr.vec4binary prevExpr (GLSLExpr.float 1.0)
                   _ -> prevExpr
  Just <$> (blend prevRGBA x >>= assignForced)
appendExpr Output.Add (Just prevExpr) x = do
  let prevRGB = GLSLExpr.coerceVec3 prevExpr -- discards previous alpha channel if there was one
  Just <$> (assignForced $ GLSLExpr.add prevRGB x)
appendExpr Output.Mul (Just prevExpr) x = do
  let prevRGB = GLSLExpr.coerceVec3 prevExpr -- discards previous alpha channel if there was one
  Just <$> (assignForced $ GLSLExpr.product prevRGB x)
-}
    
fragmentShader :: Boolean -> Tempo -> Map String Int -> Map String Int -> Program -> Program -> String
fragmentShader webGl2 tempo imgMap vidMap oldProgram newProgram = header webGl2 <> st.code <> gl_FragColor <> "}"
  where
    (Tuple v4 st) = runG webGl2 imgMap vidMap $ programsToGLSL tempo oldProgram newProgram
    fragColorVarName = if webGl2 then "fragColor" else "gl_FragColor"
    gl_FragColor = fragColorVarName <> " = " <> toExpr v4 <> ";\n"
