module FragmentShader where

import Prelude(($),pure,show,bind,(<>),(>>=),(<$>),(<<<),map,(==),(&&),otherwise,max)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (singleton,concat,fromList,zipWith,cons,head,tail,length)
import Data.Traversable (traverse,for,sequence)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Foldable (fold,intercalate,foldM)
import Data.Set as Set
import Data.Unfoldable1 (replicate1)
import Control.Monad.State (get)
import Data.Map (lookup)
import Data.FunctorWithIndex (mapWithIndex)

import NonEmptyList
import MultiMode (MultiMode(..))
import Signal (Signal(..))
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum,ternaryFunction,glslTypeToString,Exprs,exprsChannels,split,unsafeSwizzleX,unsafeSwizzleY)
import GLSLExpr as GLSLExpr
import GLSL (GLSL,assign,assignForced,swizzleX,swizzleY,swizzleZ,swizzleW,alignFloat,texture2D,textureFFT,alignVec2,alignVec3,alignVec4,alignRGBA,runGLSL,withFxys,extend,zipWithAAA,zipWithAAAA)

testCodeGen :: Boolean -> Signal -> String
testCodeGen webGl2 x = assignments <> lastExprs
  where
    (Tuple a st) = runGLSL webGl2 (signalToGLSL x)
    assignments = fold $ mapWithIndex indexedGLSLExprToString st.exprs
    lastExprs = fold $ map (\y -> y.string <> " :: " <> show y.glslType <> "\n") a


indexedGLSLExprToString :: Int -> GLSLExpr -> String
indexedGLSLExprToString n x = glslTypeToString x.glslType <> " _" <> show n <> " = " <> x.string <> ";\n"

signalToGLSL :: Signal -> GLSL Exprs

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

signalToGLSL Pi = pure $ singleton $ GLSLExpr.pi

signalToGLSL Px = pure $ singleton $ GLSLExpr.px

signalToGLSL Py = pure $ singleton $ GLSLExpr.py

signalToGLSL Pxy = pure $ singleton $ GLSLExpr.pxy

signalToGLSL Aspect = pure $ singleton $ GLSLExpr.aspect

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

signalToGLSL Time = pure $ singleton $ GLSLExpr.time

signalToGLSL Beat = pure $ singleton $ simpleFromString Float "_beat"

signalToGLSL ETime = pure $ singleton $ simpleFromString Float "_etime"

signalToGLSL EBeat = pure $ singleton $ simpleFromString Float "_ebeat"

signalToGLSL (FFT x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_fft")

signalToGLSL (IFFT x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignFloat >>= traverse (textureFFT "_ifft")

signalToGLSL (Fb xy) = signalToGLSL xy >>= simpleUnaryFunction GLSLExpr.unipolar >>= traverse assignForced >>= alignVec2 >>= traverse (texture2D "_fb")

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

signalToGLSL (RgbHsv x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv
signalToGLSL (HsvRgb x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb
signalToGLSL (HsvH x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleX
signalToGLSL (HsvS x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleY
signalToGLSL (HsvV x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL (HsvR x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleX
signalToGLSL (HsvG x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleY
signalToGLSL (HsvB x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.hsvrgb >>= traverse swizzleZ
signalToGLSL (RgbR x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleX
signalToGLSL (RgbG x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleY
signalToGLSL (RgbB x) = signalToGLSL x >>= alignVec3 >>= traverse swizzleZ
signalToGLSL (RgbH x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleX
signalToGLSL (RgbS x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleY
signalToGLSL (RgbV x) = signalToGLSL x >>= alignVec3 >>= simpleUnaryFunction GLSLExpr.rgbhsv >>= traverse swizzleZ

signalToGLSL (Osc x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.osc
signalToGLSL (Tri x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.tri
signalToGLSL (Saw x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.saw
signalToGLSL (Sqr x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.sqr
signalToGLSL (LFTri x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.tri
signalToGLSL (LFSaw x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.saw
signalToGLSL (LFSqr x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.sqr

signalToGLSL (Abs x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.abs
signalToGLSL (Acos x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.acos
signalToGLSL (Acosh x) = signalToGLSL x >>= acosh
signalToGLSL (AmpDb x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.ampdb
signalToGLSL (Asin x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.asin
signalToGLSL (Asinh x) = signalToGLSL x >>= asinh
signalToGLSL (Atan x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.atan
signalToGLSL (Atanh x) = signalToGLSL x >>= atanh
signalToGLSL (Bipolar x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.bipolar
signalToGLSL (Cbrt x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.cbrt
signalToGLSL (Ceil x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.ceil
signalToGLSL (Cos x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.cos
signalToGLSL (Cosh x) = signalToGLSL x >>= cosh
signalToGLSL (CpsMidi x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.cpsmidi
signalToGLSL (DbAmp x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.dbamp
signalToGLSL (Exp x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.exp
signalToGLSL (Floor x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.floor
signalToGLSL (Fract x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.fract
signalToGLSL (Log x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.log
signalToGLSL (Log2 x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.log2
signalToGLSL (Log10 x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.log10 
signalToGLSL (MidiCps x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.midicps
signalToGLSL (Round x) = signalToGLSL x >>= round
signalToGLSL (Sign x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.sign
signalToGLSL (Sin x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.sin
signalToGLSL (Sinh x) = signalToGLSL x >>= sinh
signalToGLSL (Sqrt x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.sqrt
signalToGLSL (Tan x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.tan
signalToGLSL (Tanh x) = signalToGLSL x >>= tanh
signalToGLSL (Trunc x) = signalToGLSL x >>= trunc
signalToGLSL (Unipolar x) = signalToGLSL x >>= simpleUnaryFunction GLSLExpr.unipolar

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
  abmCombinatorial (\a b -> pure $ GLSLExpr.distance a b) fxys xys

signalToGLSL (Prox xy) = do
  fxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignVec2
  abmCombinatorial (\a b -> pure $ GLSLExpr.prox a b) fxys xys

signalToGLSL (SetFxy xy z) = do
  fxys <- signalToGLSL xy >>= alignVec2 >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (SetFx x z) = do
  prevFxys <- _.fxys <$> get
  xs <- signalToGLSL x >>= alignFloat
  let f fxy x' = pure $ GLSLExpr.vec2binary x' (unsafeSwizzleY fxy)
  fxys <- abmCombinatorial f prevFxys xs >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (SetFy y z) = do
  prevFxys <- _.fxys <$> get
  ys <- signalToGLSL y >>= alignFloat
  let f fxy y' = pure $ GLSLExpr.vec2binary (unsafeSwizzleX fxy) y'
  fxys <- abmCombinatorial f prevFxys ys >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (Zoom xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignVec2
  fxys <- abmCombinatorial (\fxy xy' -> pure $ GLSLExpr.division fxy xy') prevFxys xys >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (Move xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignFloat
  fxys <- abmCombinatorial (\fxy xy' -> pure $ GLSLExpr.difference fxy xy') prevFxys xys >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (Tile xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignFloat
  fxys <- abmCombinatorial (\fxy xy' -> pure $ GLSLExpr.tile fxy xy') prevFxys xys >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (Spin x z) = do
  prevFxys <- _.fxys <$> get
  xs <- signalToGLSL x >>= alignFloat
  fxys <- abmCombinatorial spin prevFxys xs >>= traverse assignForced
  withFxys fxys $ signalToGLSL z

signalToGLSL (Sum mm x y) = binaryFunction mm GLSLExpr.sum x y
signalToGLSL (Difference mm x y) = binaryFunction mm GLSLExpr.difference x y
signalToGLSL (Product mm x y) = binaryFunction mm GLSLExpr.product x y
signalToGLSL (Division mm x y) = binaryFunction mm GLSLExpr.division x y
signalToGLSL (Mod mm x y) = binaryFunction mm GLSLExpr.mod x y
signalToGLSL (Pow mm x y) = binaryFunction mm GLSLExpr.pow x y
signalToGLSL (Equal mm x y) = binaryFunction mm GLSLExpr.equal x y
signalToGLSL (NotEqual mm x y) = binaryFunction mm GLSLExpr.notEqual x y
signalToGLSL (GreaterThan mm x y) = binaryFunction mm GLSLExpr.greaterThan x y
signalToGLSL (GreaterThanEqual mm x y) = binaryFunction mm GLSLExpr.greaterThanEqual x y
signalToGLSL (LessThan mm x y) = binaryFunction mm GLSLExpr.lessThan x y
signalToGLSL (LessThanEqual mm x y) = binaryFunction mm GLSLExpr.lessThanEqual x y
signalToGLSL (Max mm x y) = binaryFunction mm GLSLExpr.max x y
signalToGLSL (Min mm x y) = binaryFunction mm GLSLExpr.min x y

signalToGLSL (Gate mm x y) = do
  xs <- signalToGLSL x
  ys <- signalToGLSL y >>= traverse assign
  combineChannels mm GLSLExpr.gate xs ys

signalToGLSL (Clip mm r x) = clipEtcFunction mm GLSLExpr.clip r x
signalToGLSL (Between mm r x) = clipEtcFunction mm GLSLExpr.between r x
signalToGLSL (SmoothStep mm r x) = clipEtcFunction mm GLSLExpr.smoothstep r x

signalToGLSL (Circle mm xy d) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xys <- signalToGLSL xy >>= alignVec2
    ds <- case mm of
            Combinatorial -> signalToGLSL d
            Pairwise -> signalToGLSL d >>= alignFloat
    pure $ combine mm (GLSLExpr.circle fxy) xys ds
  traverse assign $ concat exprss

signalToGLSL (Rect mm xy wh) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xys <- signalToGLSL xy >>= alignVec2
    whs <- signalToGLSL wh >>= alignVec2
    sequence $ combine mm (rect fxy) xys whs
  traverse assign $ concat exprss

signalToGLSL (VLine mm x w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xs <- signalToGLSL x
    ws <- signalToGLSL w
    combineChannels mm (GLSLExpr.vline fxy) xs ws
  traverse assign $ concat exprss

signalToGLSL (HLine mm x w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xs <- signalToGLSL x
    ws <- signalToGLSL w
    combineChannels mm (GLSLExpr.hline fxy) xs ws
  traverse assign $ concat exprss

signalToGLSL (Chain mm xy w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xys <- signalToGLSL xy >>= alignVec2
    ws <- signalToGLSL w >>= alignFloat -- for now, have to align float because of monomorphic line function in shader, later maybe optimize
    let xys' = everyAdjacentPair xys -- NonEmptyList (Tuple GLSLExpr GLSLExpr) where GLSLExprs are all Vec2
    let f xyTuple theW = GLSLExpr.line fxy (fst xyTuple) (snd xyTuple) theW
    pure $ combine mm f xys' ws
  traverse assign $ concat exprss

signalToGLSL (Lines mm xy w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xysVec4 <- signalToGLSL xy >>= alignVec4
    ws <- signalToGLSL w >>= alignFloat
    let f v4 theW = GLSLExpr.line fxy (GLSLExpr.unsafeSwizzleXY v4) (GLSLExpr.unsafeSwizzleZW v4) theW
    pure $ combine mm f xysVec4 ws
  traverse assign $ concat exprss

signalToGLSL (ILines mm xy w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xysVec4 <- signalToGLSL xy >>= alignVec4
    ws <- signalToGLSL w >>= alignFloat
    let f v4 theW = GLSLExpr.iline fxy (GLSLExpr.unsafeSwizzleXY v4) (GLSLExpr.unsafeSwizzleZW v4) theW
    pure $ combine mm f xysVec4 ws
  traverse assign $ concat exprss 

signalToGLSL (Mesh mm xy w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xys <- signalToGLSL xy >>= alignVec2
    ws <- signalToGLSL w >>= alignFloat
    let xys' = everyPair xys -- NonEmptyList (Tuple GLSLExpr GLSLExpr) where GLSLExprs are all Vec2
    let f xyTuple theW = GLSLExpr.line fxy (fst xyTuple) (snd xyTuple) theW
    pure $ combine mm f xys' ws
  traverse assign $ concat exprss 

signalToGLSL (ILine mm xy1 xy2 w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xy1s <- signalToGLSL xy1 >>= alignVec2
    xy2s <- signalToGLSL xy2 >>= alignVec2
    ws <- signalToGLSL w >>= alignFloat
    pure $ combine3 mm (GLSLExpr.iline fxy) xy1s xy2s ws
  traverse assign $ concat exprss

signalToGLSL (Line mm xy1 xy2 w) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xy1s <- signalToGLSL xy1 >>= alignVec2
    xy2s <- signalToGLSL xy2 >>= alignVec2
    ws <- signalToGLSL w >>= alignFloat
    pure $ combine3 mm (GLSLExpr.line fxy) xy1s xy2s ws
  traverse assign $ concat exprss

signalToGLSL (Point xy) = do
  fxys <- _.fxys <$> get
  exprss <- for fxys $ \fxy -> do
    xys <- signalToGLSL xy >>= alignVec2
    pure $ map (GLSLExpr.point fxy) xys
  traverse assign $ concat exprss

signalToGLSL (LinLin mm r1 r2 x) = do
  r1s <- signalToGLSL r1 >>= alignVec2
  r2s <- signalToGLSL r2 >>= alignVec2
  xs <- signalToGLSL x
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
  
signalToGLSL (Mix mm c t e) = do
  cs <- signalToGLSL c
  ts <- signalToGLSL t
  es <- signalToGLSL e
  combineChannels3 mm GLSLExpr.mix cs ts es
            
signalToGLSL (Seq steps y) = do
  steps' <- signalToGLSL steps
  case exprsChannels steps' of
    1 -> pure steps'
    _ -> do
      let steps'' = concat $ map split steps' -- ? there is also splitIntoFloats in GLSL.purs which assigns, do we want to use that?
      ys <- signalToGLSL y >>= alignFloat
      pure $ map (GLSLExpr.seq steps'') ys
 
signalToGLSL _ = pure $ singleton $ zero

simpleUnaryFunction :: (GLSLExpr -> GLSLExpr) -> Exprs -> GLSL Exprs
simpleUnaryFunction f = traverse $ \x -> pure $ f x

simpleUnaryExpression :: (String -> String) -> Exprs -> GLSL Exprs
simpleUnaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

unaryExpression :: (String -> String) -> Exprs -> GLSL Exprs
unaryExpression f = traverse $ \x -> pure { string: f x.string, glslType: x.glslType, isSimple: false, deps: x.deps }

-- deduces type from type of first of each pair (which is assumed to match second of each pair)
zipBinaryExpression :: (String -> String -> String) -> Exprs -> Exprs -> GLSL Exprs
zipBinaryExpression f xs ys = pure $ zipWith (\x y -> { string: f x.string y.string, glslType:x.glslType, isSimple: false, deps: x.deps <> y.deps }) xs ys

binaryFunction :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Signal -> Signal -> GLSL Exprs
binaryFunction mm f x y = do
  xs <- signalToGLSL x
  ys <- signalToGLSL y
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
    
clipEtcFunction :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> Signal -> Signal -> GLSL Exprs
clipEtcFunction mm f r x = do
  rs <- signalToGLSL r >>= alignVec2 >>= traverse assign
  xs <- signalToGLSL x
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
            

abmCombinatorial :: (GLSLExpr -> GLSLExpr -> GLSL GLSLExpr) -> Exprs -> Exprs -> GLSL Exprs
abmCombinatorial f xs ys = sequence $ do -- in NonEmptyList monad
    x <- xs
    y <- ys
    pure $ f x y


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
  return smoothstep(aa,0.,length(fxy - xy2 * h)-(w*0.5));}"""
-- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the HSV-RGB conversion algorithms above!

