module FragmentShader where

import Prelude(($),pure,show,bind,(<>),(>>=),(<$>),(<<<),map,(==),(&&),otherwise,max)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (singleton,concat,fromList,zipWith,cons,head,tail,length)
import Data.Traversable (traverse,for)
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
import GLSLExpr (GLSLExpr,GLSLType(..),simpleFromString,zero,dotSum,ternaryFunction,glslTypeToString,Exprs,exprsChannels,split)
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

signalToGLSL Pi = pure $ singleton $ simpleFromString Float "PI"

signalToGLSL Px = pure $ singleton $ simpleFromString Float "(2./res.x)"

signalToGLSL Py = pure $ singleton $ simpleFromString Float "(2./res.y)"

signalToGLSL Pxy = pure $ singleton $ simpleFromString Vec2 "(2./res)"

signalToGLSL Aspect = pure $ singleton $ simpleFromString Float "(res.x/res.y)"

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

signalToGLSL (SetFxy xy z) = do
  fxys <- signalToGLSL xy >>= alignVec2
  withFxys fxys $ signalToGLSL z

signalToGLSL (SetFx x z) = do
  prevFxys <- _.fxys <$> get
  xs <- signalToGLSL x >>= alignFloat
  fxys <- abVec2Combinatorial (\fxy x' -> "vec2(" <> x' <> "," <> fxy <> ".y)") prevFxys xs
  withFxys fxys $ signalToGLSL z

signalToGLSL (SetFy y z) = do
  prevFxys <- _.fxys <$> get
  ys <- signalToGLSL y >>= alignFloat
  fxys <- abVec2Combinatorial (\fxy y' -> "vec2(" <> fxy <> ".x," <> y' <> ")") prevFxys ys
  withFxys fxys $ signalToGLSL z

signalToGLSL (Zoom xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignVec2
  fxys <- abVec2Combinatorial (\fxy xy' -> "(" <> fxy <> "/" <> xy' <> ")") prevFxys xys
  withFxys fxys $ signalToGLSL z

signalToGLSL (Move xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignFloat
  fxys <- abVec2Combinatorial (\fxy xy' -> "(" <> fxy <> "-" <> xy' <> ")") prevFxys xys
  withFxys fxys $ signalToGLSL z

signalToGLSL (Tile xy z) = do
  prevFxys <- _.fxys <$> get
  xys <- signalToGLSL xy >>= alignFloat
  fxys <- abVec2Combinatorial (\fxy xy' -> "tile(" <> xy' <> "," <> fxy <> ")") prevFxys xys
  withFxys fxys $ signalToGLSL z

signalToGLSL (Spin x z) = do
  prevFxys <- _.fxys <$> get
  xs <- signalToGLSL x >>= alignFloat
  fxys <- abVec2Combinatorial (\fxy x' -> "spin(" <> x' <> "," <> fxy <> ")") prevFxys xs
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
    pure $ combine mm (GLSLExpr.rect fxy) xys whs
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

simpleUnaryFunction :: String -> Exprs -> GLSL Exprs
simpleUnaryFunction funcName = simpleUnaryExpression $ \x -> funcName <> "(" <> x <> ")"

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
            
abFloatCombinatorial :: (String -> String -> String) -> Exprs -> Exprs -> GLSL Exprs
abFloatCombinatorial f xs ys = pure $ do -- in NonEmptyList monad
  x <- xs
  y <- ys
  pure { string: f x.string y.string, glslType:Float, isSimple:false, deps: x.deps <> y.deps }

abVec2Combinatorial :: (String -> String -> String) -> Exprs -> Exprs -> GLSL Exprs
abVec2Combinatorial f xs ys = pure $ do -- in NonEmptyList monad
  x <- xs
  y <- ys
  pure { string: f x.string y.string, glslType:Vec2, isSimple:false, deps: x.deps <> y.deps }

unipolar :: Exprs -> GLSL Exprs
unipolar = simpleUnaryExpression (\s -> "(" <> s <> "*0.5+0.5)")

blend :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- all Vec4
blend a b = do
  b' <- assign b
  alpha <- swizzleW b'
  pure $ ternaryFunction "mix" Vec4 a b' alpha

acosh :: Exprs -> GLSL Exprs
acosh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "acosh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "-1.))")

asinh :: Exprs -> GLSL Exprs
asinh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "asinh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "log(" <> x <> "+sqrt(" <> x <> "*" <> x <> "+1.))")

atanh :: Exprs -> GLSL Exprs
atanh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "atanh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(log((1.+" <> x <> ")/(" <> "1.-" <> x <> "))/2.)")

cosh :: Exprs -> GLSL Exprs
cosh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "cosh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")+exp(" <> x <> "*-1.))/2.)")

sinh :: Exprs -> GLSL Exprs
sinh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "sinh" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "((exp(" <> x <> ")-exp(" <> x <> "*-1.))/2.)")

tanh :: Exprs -> GLSL Exprs
tanh xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "tanh" xs
    false -> do
      xs' <- traverse assign xs
      sinhs <- sinh xs'
      coshs <- cosh xs'
      zipBinaryExpression (\x y -> "(" <> x <> "/" <> y <> ")") sinhs coshs

trunc :: Exprs -> GLSL Exprs
trunc xs = do
  s <- get
  case s.webGl2 of
    true -> simpleUnaryFunction "trunc" xs
    false -> traverse assign xs >>= simpleUnaryExpression (\x -> "(floor(abs(" <> x <> "))*sign(" <> x <> "))")
    
{-
-- TODO: reimplement as inline expressions 
vec2 _fxy() { return (gl_FragCoord.xy/res) * 2. - 1.; }
float xfNew(float t1,float t2){return clamp((_etime-t1)/(t2-t1),0.,1.);}
float xfOld(float t1,float t2){return 1.-xFadeNew(t1,t2);}
float prox(vec2 x,vec2 y){return clamp((2.828427-distance(x,y))/2.828427,0.,1.);}
float phasor(float f) { return (_time*f - floor(_time*f));}
float tri(float f) { float p = phasor(f); return p < 0.5 ? p*4.-1. : 1.-((p-0.5)*4.) ;}
float saw(float f) { return phasor(f)*2.-1.;}
float sqr(float f) { float p = phasor(f); return p < 0.5 ? -1. : 1.;}
vec2 tile(vec2 ab,vec2 fxy) { return fract(((fxy*0.5)+0.5)*ab)*2.-1.;}
vec2 spin(float a,vec2 fxy) {
 float ct = cos(a*3.1415926538); float st = sin(a*3.1415926538);
 return vec2(fxy.x*ct-fxy.y*st,fxy.y*ct+fxy.x*st);}
-}

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

