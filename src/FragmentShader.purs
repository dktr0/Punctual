module FragmentShader where

import Prelude (bind, discard, flip, map, negate, pure, show, ($), (-), (<$>), (<*>), (<<<), (<>), (==), (>), (>>=), (>>>))
import Prelude as Prelude
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (NonEmptyList, drop, fromList, head, last, length, tail, zipWith)
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Foldable (foldM)
import Data.Unfoldable (replicate)
import Control.Monad.State (get,modify_)
import Data.Map (Map,lookup)
import Data.Tempo (Tempo)
import Data.DateTime (DateTime)
import Data.Number (round, trunc) as Number
import Data.List as List

import NonEmptyList (zipWithEqualLength,everyPair,everyAdjacentPair,pairwise)
import MultiMode (MultiMode(..))
import Signal (Signal(..))
import Action (Action,actionTimesAsSecondsSinceEval)
import Output (Output)
import Output as Output
import Program (Program)
import Expr (class Expr, Float(..), Vec2(..), Vec3, Vec4, abs, acos, add, ampdb, asin, atan, between, bipolar, blend, castExprs, cbrt, ceil, circle, clip, constant, cos, cpsmidi, dbamp, difference, distance, division, divisionExprFloat, dotSum, equal, exp, expr, fadeIn, fadeOut, floatFloatToVec2, floor, fract, fromFloat, fromFloats, fromVec2s, function1, gate, greaterThan, greaterThanEqual, hline, hsvrgb, iline, lessThan, lessThanEqual, line, linlin, log, log10, log2, max, midicps, min, mix, mixFloat, mod, notEqual, pi, point, pow, product, productExprFloat, productFloatExpr, prox, pxy, rgbaFade, rgbhsv, rtx, rtxy, rty, seq, setX, setY, sign, sin, smoothStep, sqrt, sum, swizzleW, swizzleX, swizzleXY, swizzleXYZ, swizzleY, swizzleZ, swizzleZW, tan, tile, toExpr, unaryFunction, unipolar, vec3FloatToVec4, vline, xyr, xyrt, xyt, zero)
import G (G, assign, runG, texture2D, textureFFT, withAlteredTime, withFxys)
import Matrix (Matrix(..),fromNonEmptyListMulti,mapRows,flatten,fromNonEmptyList,rep,combine,combine3,semiFlatten)
import Number (acosh, asinh, atanh, cosh, sinh, tanh) as Number

exprToExprs :: forall a b. Expr a => Expr b => a -> Matrix b
exprToExprs x = mapRows castExprs $ pure x

signalToExprs :: forall a. Expr a => Signal -> G (Matrix a)

signalToExprs (Constant x) = pure $ exprToExprs $ FloatConstant x

signalToExprs (SignalList Combinatorial xs) =
  case fromList xs of
    Nothing -> pure $ exprToExprs $ FloatConstant 0.0
    Just xs' -> do
      case length xs' of
        1 -> signalToExprs (head xs')
        _ -> do
          xs'' <- fromNonEmptyListMulti <$> traverse signalToExprs xs' -- :: Matrix Float, with list already combinatorially expanded
          pure $ mapRows fromFloats xs''

signalToExprs (SignalList Pairwise xs) = 
  case fromList xs of
    Nothing -> pure $ exprToExprs $ FloatConstant 0.0
    Just xs' -> do
      case length xs' of
        1 -> signalToExprs (head xs')
        _ -> do
          xs'' <- map flatten <$> traverse signalToExprs xs' :: G (NonEmptyList (NonEmptyList Float))
          pure $ mapRows fromFloats $ Matrix $ pairwise xs''

signalToExprs (Append x y) = do
  xs <- flatten <$> (signalToExprs x :: G (Matrix Float))
  ys <- flatten <$> (signalToExprs y :: G (Matrix Float))
  pure $ fromNonEmptyList $ fromFloats (xs <> ys)

signalToExprs (Zip x y) = do
  xs <- flatten <$> (signalToExprs x :: G (Matrix Float))
  ys <- flatten <$> (signalToExprs y :: G (Matrix Float))
  pure $ fromNonEmptyList $ fromVec2s $ zipWithEqualLength floatFloatToVec2 xs ys

signalToExprs (Mono x) = do
  xs <- (signalToExprs x :: G (Matrix Float)) -- MAYBE LATER: determine an optimal type for realization of x based on its number of channels or the nature of the underlying signal (eg. whether it naturally produces vec2s 3s or 4s)
  let f = sum (dotSum <$> flatten xs)
  pure $ pure $ fromFloat f

signalToExprs (Rep n x) = do
  xs <- signalToExprs x :: G (Matrix Float)
  pure $ mapRows castExprs $ rep n xs

signalToExprs Pi = pure $ exprToExprs $ FloatExpr "PI"

signalToExprs Px = pure $ exprToExprs $ FloatExpr "(2./res.x)"

signalToExprs Py = pure $ exprToExprs $ FloatExpr "(2./res.y)"

signalToExprs Pxy = pure $ exprToExprs $ Vec2Expr "(2./res)"

signalToExprs Aspect = pure $ exprToExprs $ FloatExpr "(res.x/res.y)"

signalToExprs Fx = (exprToExprs <<< swizzleX <<< _.fxy) <$> get

signalToExprs Fy = (exprToExprs <<< swizzleY <<< _.fxy) <$> get

signalToExprs Fxy = (exprToExprs <<< _.fxy) <$> get

signalToExprs FRt = signalToExprs $ XyRt Fxy

signalToExprs FR = signalToExprs $ XyR Fxy

signalToExprs FT = signalToExprs $ XyT Fxy

signalToExprs Lo = pure $ exprToExprs $ FloatExpr "lo"

signalToExprs Mid = pure $ exprToExprs $ FloatExpr "mid"

signalToExprs Hi = pure $ exprToExprs $ FloatExpr "hi"

signalToExprs ILo = pure $ exprToExprs $ FloatExpr "ilo"

signalToExprs IMid = pure $ exprToExprs $ FloatExpr "imid"

signalToExprs IHi = pure $ exprToExprs $ FloatExpr "ihi"

signalToExprs Cps = pure $ exprToExprs $ FloatExpr "_cps"

signalToExprs Time = (exprToExprs <<< _.time) <$> get

signalToExprs Beat = (exprToExprs <<< _.beat) <$> get

signalToExprs ETime = (exprToExprs <<< _.etime) <$> get

signalToExprs EBeat = (exprToExprs <<< _.ebeat) <$> get

signalToExprs FFT = get >>= _.fxy >>> unipolar >>> swizzleX >>> textureFFT "o" >>= exprToExprs >>> maskUnitSquare

signalToExprs IFFT = get >>= _.fxy >>> unipolar >>> swizzleX >>> textureFFT "i" >>= exprToExprs >>> maskUnitSquare

signalToExprs Fb = get >>= _.fxy >>> unipolar >>> texture2D "f" >>= exprToExprs >>> maskUnitSquare

signalToExprs Cam = get >>= _.fxy >>> unipolar >>> texture2D "w" >>= exprToExprs >>> maskUnitSquare

signalToExprs (Img url) = do
  s <- get
  case lookup url s.imgMap of
    Just n -> do
      t <- texture2D ("t" <> show n) (unipolar s.fxy) -- :: Vec3
      maskUnitSquare $ exprToExprs t
    Nothing -> pure $ pure $ zero

signalToExprs (Vid url) = do
  s <- get
  case lookup url s.vidMap of
    Just n -> do
      t <- texture2D ("t" <> show n) (unipolar s.fxy) -- :: Vec3
      maskUnitSquare $ exprToExprs t
    Nothing -> pure $ pure $ zero

signalToExprs (Blend x) = do
  xs <- flatten <$> (signalToExprs x :: G (Matrix Vec4))
  y <- (foldM (\a b -> assign $ blend a b) (head xs) (tail xs) :: G Vec4)
  pure $ exprToExprs y

signalToExprs (Add x) = do
  xs <- flatten <$> (signalToExprs x :: G (Matrix Vec3))
  y <- (foldM (\a b -> assign $ add a b) (head xs) (tail xs) :: G Vec3)
  pure $ exprToExprs y

signalToExprs (Mul x) = do
  xs <- flatten <$> (signalToExprs x :: G (Matrix Vec3))
  y <- (foldM (\a b -> assign $ product a b) (head xs) (tail xs) :: G Vec3)
  pure $ exprToExprs y

signalToExprs (RgbHsv x) = signalToExprs x >>= map rgbhsv >>> mapRows castExprs >>> traverse assign
signalToExprs (HsvRgb x) = signalToExprs x >>= map hsvrgb >>> mapRows castExprs >>> traverse assign
signalToExprs (HsvR x) = signalToExprs x >>= map hsvrgb >>> map swizzleX >>> mapRows castExprs >>> traverse assign
signalToExprs (HsvG x) = signalToExprs x >>= map hsvrgb >>> map swizzleY >>> mapRows castExprs >>> traverse assign
signalToExprs (HsvB x) = signalToExprs x >>= map hsvrgb >>> map swizzleZ >>> mapRows castExprs >>> traverse assign
signalToExprs (RgbR x) = (signalToExprs x :: G (Matrix Vec3)) >>= map swizzleX >>> mapRows castExprs >>> pure
signalToExprs (RgbG x) = (signalToExprs x :: G (Matrix Vec3)) >>= map swizzleY >>> mapRows castExprs >>> pure
signalToExprs (RgbB x) = (signalToExprs x :: G (Matrix Vec3)) >>= map swizzleZ >>> mapRows castExprs >>> pure
signalToExprs (RgbH x) = signalToExprs x >>= map rgbhsv >>> map swizzleX >>> mapRows castExprs >>> traverse assign
signalToExprs (RgbS x) = signalToExprs x >>= map rgbhsv >>> map swizzleY >>> mapRows castExprs >>> traverse assign
signalToExprs (RgbV x) = signalToExprs x >>= map rgbhsv >>> map swizzleZ >>> mapRows castExprs >>> traverse assign

signalToExprs (Osc x) = signalToExprs x >>= traverse osc
signalToExprs (Tri x) = signalToExprs x >>= traverse tri
signalToExprs (Saw x) = signalToExprs x >>= traverse saw
signalToExprs (Sqr x) = signalToExprs x >>= traverse sqr
signalToExprs (LFTri x) = signalToExprs x >>= traverse tri
signalToExprs (LFSaw x) = signalToExprs x >>= traverse saw
signalToExprs (LFSqr x) = signalToExprs x >>= traverse sqr

signalToExprs (Abs x) = signalToExprs x >>= map abs >>> traverse assign
signalToExprs (Acos x) = signalToExprs x >>= map acos >>> traverse assign
signalToExprs (Acosh x) = signalToExprs x >>= traverse acosh
signalToExprs (AmpDb x) = signalToExprs x >>= map ampdb >>> traverse assign
signalToExprs (Asin x) = signalToExprs x >>= map asin >>> traverse assign
signalToExprs (Asinh x) = signalToExprs x >>= traverse asinh
signalToExprs (Atan x) = signalToExprs x >>= map atan >>> traverse assign
signalToExprs (Atanh x) = signalToExprs x >>= traverse atanh
signalToExprs (Bipolar x) = signalToExprs x >>= map bipolar >>> traverse assign
signalToExprs (Cbrt x) = signalToExprs x >>= map cbrt >>> traverse assign
signalToExprs (Ceil x) = signalToExprs x >>= map ceil >>> traverse assign
signalToExprs (Cos x) = signalToExprs x >>= map cos >>> traverse assign
signalToExprs (Cosh x) = signalToExprs x >>= traverse cosh
signalToExprs (CpsMidi x) = signalToExprs x >>= map cpsmidi >>> traverse assign
signalToExprs (DbAmp x) = signalToExprs x >>= map dbamp >>> traverse assign
signalToExprs (Exp x) = signalToExprs x >>= map exp >>> traverse assign
signalToExprs (Floor x) = signalToExprs x >>= map floor >>> traverse assign
signalToExprs (Fract x) = signalToExprs x >>= map fract >>> traverse assign
signalToExprs (Log x) = signalToExprs x >>= map log >>> traverse assign
signalToExprs (Log2 x) = signalToExprs x >>= map log2 >>> traverse assign
signalToExprs (Log10 x) = signalToExprs x >>= map log10 >>> traverse assign
signalToExprs (MidiCps x) = signalToExprs x >>= map midicps >>> traverse assign
signalToExprs (Round x) = signalToExprs x >>= traverse round
signalToExprs (Sign x) = signalToExprs x >>= map sign >>> traverse assign
signalToExprs (Sin x) = signalToExprs x >>= map sin >>> traverse assign
signalToExprs (Sinh x) = signalToExprs x >>= traverse sinh
signalToExprs (Sqrt x) = signalToExprs x >>= map sqrt >>> traverse assign
signalToExprs (Tan x) = signalToExprs x >>= map tan >>> traverse assign
signalToExprs (Tanh x) = signalToExprs x >>= traverse tanh
signalToExprs (Trunc x) = signalToExprs x >>= traverse trunc
signalToExprs (Unipolar x) = signalToExprs x >>= map unipolar >>> traverse assign

signalToExprs (RtXy rt) = signalToExprs rt >>= traverse assign >>= map rtxy >>> traverse assign >>= mapRows castExprs >>> pure
signalToExprs (RtX rt) = signalToExprs rt >>= traverse assign >>= map rtx >>> traverse assign >>= mapRows castExprs >>> pure
signalToExprs (RtY rt) = signalToExprs rt >>= traverse assign >>= map rty >>> traverse assign >>= mapRows castExprs >>> pure
signalToExprs (XyRt xy) = signalToExprs xy >>= traverse assign >>= map xyrt >>> traverse assign >>= mapRows castExprs >>> pure
signalToExprs (XyR xy) = signalToExprs xy >>= traverse assign >>= map xyr >>> traverse assign >>= mapRows castExprs >>> pure
signalToExprs (XyT xy) = signalToExprs xy >>= traverse assign >>= map xyt >>> traverse assign >>= mapRows castExprs >>> pure

signalToExprs (Dist xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows castExprs $ map (distance fxy) xys

signalToExprs (Prox xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows castExprs $ map (prox fxy) xys

signalToExprs (SetFxy xy z) = do
  fxys <- signalToExprs xy >>= traverse assign
  withFxys (flatten fxys) $ signalToExprs z

signalToExprs (SetFx x z) = do
  fxy <- _.fxy <$> get
  xs <- signalToExprs x
  fxys <- traverse assign $ map (setX fxy) $ flatten xs
  withFxys fxys $ signalToExprs z

signalToExprs (SetFy y z) = do
  fxy <- _.fxy <$> get
  ys <- signalToExprs y
  fxys <- traverse assign $ map (setY fxy) $ flatten ys
  withFxys fxys $ signalToExprs z

signalToExprs (Zoom q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (divisionExprFloat fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomXy q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Vec2)
  fxys <- traverse assign $ map (division fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomX q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (setX fxy <<< division (swizzleX fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomY q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (setY fxy <<< division (swizzleY fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Move q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Vec2)
  fxys <- traverse assign $ map (difference fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Tile q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (tile fxy <<< fromFloat) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (TileXy q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Vec2)
  fxys <- traverse assign $ map (tile fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (TileX q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (setX fxy <<< tile (swizzleX fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (TileY q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse assign $ map (setY fxy <<< tile (swizzleY fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Spin q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Matrix Float)
  fxys <- traverse (spin fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Early q z) = do
  qs <- flatten <$> signalToExprs q -- :: NonEmptyList Float
  s <- get
  let qs' = mapFlipped qs $ \y -> {
    time: add s.time y,
    beat: add s.beat $ product y (expr "_cps"),
    etime: add s.etime y,
    ebeat: add s.ebeat $ product y (expr "_cps")
    }
  withAlteredTime qs' $ signalToExprs z

signalToExprs (Slow q z) = do
  qs <- flatten <$> signalToExprs q -- :: NonEmptyList Float
  s <- get
  let qs' = mapFlipped qs $ \y -> {
    time: division s.time y,
    beat: division s.beat y,
    etime: division s.etime y,
    ebeat: division s.ebeat y
    }
  withAlteredTime qs' $ signalToExprs z

signalToExprs (Addition mm x y) = arithmeticModel add mm x y
signalToExprs (Difference mm x y) = arithmeticModel difference mm x y
signalToExprs (Product mm x y) = arithmeticModel product mm x y
signalToExprs (Division mm x y) = arithmeticModel division mm x y
signalToExprs (Mod mm x y) = arithmeticModel mod mm x y
signalToExprs (Pow mm x y) = arithmeticModel pow mm x y
signalToExprs (Equal mm x y) = arithmeticModel equal mm x y
signalToExprs (NotEqual mm x y) = arithmeticModel notEqual mm x y
signalToExprs (GreaterThan mm x y) = arithmeticModel greaterThan mm x y
signalToExprs (GreaterThanEqual mm x y) = arithmeticModel greaterThanEqual mm x y
signalToExprs (LessThan mm x y) = arithmeticModel lessThan mm x y
signalToExprs (LessThanEqual mm x y) = arithmeticModel lessThanEqual mm x y
signalToExprs (Max mm x y) = arithmeticModel max mm x y
signalToExprs (Min mm x y) = arithmeticModel min mm x y
signalToExprs (Gate mm x y) = arithmeticModel gate mm x y

signalToExprs (Clip mm r x) = clipModel clip mm r x
signalToExprs (Between mm r x) = clipModel between mm r x
signalToExprs (SmoothStep mm r x) = clipModel smoothStep mm r x
signalToExprs (Circle mm xy d) = do 
  fxy <- _.fxy <$> get
  clipModel (circle fxy) mm xy d

signalToExprs (Rect mm xy wh) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  whs <- signalToExprs wh
  rs <- sequence $ combine (rect fxy) mm xys whs
  traverse assign $ mapRows castExprs rs

signalToExprs (VLine mm x w) = vlineModel vline mm x w
signalToExprs (HLine mm y h) = vlineModel hline mm y h

signalToExprs (Chain mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  ws <- signalToExprs w
  let xys' = fromNonEmptyList $ everyAdjacentPair $ flatten xys -- Matrix (Tuple Vec2 Vec2)
  let f xyTuple theW = line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ mapRows castExprs $ combine f mm xys' ws

signalToExprs (Lines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToExprs xy >>= traverse assign
  ws <- signalToExprs w
  let f v4 theW = line fxy (swizzleXY v4) (swizzleZW v4) theW
  traverse assign $ mapRows castExprs $ combine f mm xysVec4 ws

signalToExprs (ILines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToExprs xy >>= traverse assign
  ws <- signalToExprs w
  let f v4 theW = iline fxy (swizzleXY v4) (swizzleZW v4) theW
  traverse assign $ mapRows castExprs $ combine f mm xysVec4 ws

signalToExprs (Mesh mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- flatten <$> signalToExprs xy
  ws <- signalToExprs w
  let xys' = fromNonEmptyList $ everyPair $ xys -- Matrix (Tuple Vec2 Vec2)
  let f xyTuple theW = line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ mapRows castExprs $ combine f mm xys' ws

signalToExprs (ILine mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToExprs xy1
  xy2s <- signalToExprs xy2
  ws <- signalToExprs w
  traverse assign $ mapRows castExprs $ combine3 (iline fxy) mm xy1s xy2s ws

signalToExprs (Line mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToExprs xy1
  xy2s <- signalToExprs xy2
  ws <- signalToExprs w
  traverse assign $ mapRows castExprs $ combine3 (line fxy) mm xy1s xy2s ws

signalToExprs (Point xy) = do 
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows castExprs $ map (point fxy) xys

signalToExprs (LinLin mm r1 r2 x) = do
  r1s <- signalToExprs r1
  r2s <- signalToExprs r2
  xs <- signalToExprs x
  traverse assign $ combine3 linlin mm r1s r2s xs

signalToExprs (Mix Pairwise x y a) = do
  xs <- signalToExprs x
  ys <- signalToExprs y
  let xys = fromNonEmptyList $ zipWithEqualLength Tuple (flatten xs) (flatten ys)
  a' <- signalToExprs a
  let f (Tuple xx yy) aa = mix xx yy aa
  traverse assign $ combine f Pairwise xys a'

signalToExprs (Mix Combinatorial x y a) = do
  xs <- signalToExprs x
  ys <- signalToExprs y
  let xys = fromNonEmptyList $ zipWithEqualLength Tuple (flatten xs) (flatten ys)
  a' <- signalToExprs a -- :: Matrix Float
  let f (Tuple xx yy) aa = mixFloat xx yy aa
  traverse assign $ combine f Combinatorial xys a'

signalToExprs (Spr mm x y) = do
  xs <- (signalToExprs x :: G (Matrix Float)) >>= semiFlatten >>> fromNonEmptyList >>> pure-- :: one-dimensional Matrix (NonEmptyList Float)
  ys <- (signalToExprs y :: G (Matrix Float)) >>= map unipolar >>> pure -- :: Matrix Float
  traverse assign $ mapRows castExprs $ combine (flip seq) mm xs ys

signalToExprs (Seq steps) = do
  steps' <- semiFlatten <$> signalToExprs steps -- :: NonEmptyList (NonEmptyList Float)
  b <- get >>= _.beat >>> fract >>> pure
  chs <- traverse assign $ map (seq b) steps' -- :: NonEmptyList Float  ; seq :: Float -> NonEmptyList Float -> Float
  traverse assign $ fromNonEmptyList $ castExprs chs

signalToExprs _ = pure $ exprToExprs $ FloatConstant 0.0

{-
-- for preserving alignment requests through intermediate calculations that are Float -> G Float
-- created during matrix refactor but currently unused???
splitFloatsApplyReassemble :: forall a. Expr a => (Float -> G Float) -> Matrix a -> G (Matrix a)
splitFloatsApplyReassemble f xs = do
  xs'' <- traverse f $ mapRows toFloats xs
  pure $ mapRows fromFloats xs''
-}

-- underlying functions exist in many variations, restricting it to float to prioritize semantics over optimization
arithmeticModel :: forall a. Expr a => (Float -> Float -> Float) -> MultiMode -> Signal -> Signal -> G (Matrix a)
arithmeticModel = combineG

-- underlying functions are Vec2 -> a -> a, keeping it to float to prioritize semantics over optimization
clipModel :: forall a. Expr a => (Vec2 -> Float -> Float) -> MultiMode -> Signal -> Signal -> G (Matrix a)
clipModel = combineG

vlineModel :: forall a. Expr a => (Vec2 -> Float -> Float -> Float) -> MultiMode -> Signal -> Signal -> G (Matrix a)
vlineModel f mm x y = do
  fxy <- _.fxy <$> get
  combineG (f fxy) mm x y

combineG :: forall a b c d. Expr a => Expr b => Expr c => Expr d => (a -> b -> c) -> MultiMode -> Signal -> Signal -> G (Matrix d)
combineG f mm x y = do
  xs <- signalToExprs x -- :: Matrix a
  ys <- signalToExprs y -- :: Matrix b
  zs <- traverse assign $ combine f mm xs ys -- :: Matrix c
  traverse assign $ mapRows castExprs zs -- TODO later: optimize to avoid redundant assignment

maskUnitSquare :: forall a. Expr a => Matrix a -> G (Matrix a)
maskUnitSquare xs = do
  fxy <- _.fxy <$> get
  mask <- assign $ lessThanEqual (abs fxy) (constant 1.0) -- :: Vec2
  mask' <- assign $ product (swizzleX mask) (swizzleY mask) -- :: Float
  traverse (assign <<< productFloatExpr mask') xs

osc :: forall a. Expr a => a -> G a
osc f = do
  t <- _.time <$> get
  assign $ sin $ productFloatExpr (product (product pi (constant 2.0)) t) f

phasor :: forall a. Expr a => a -> G a
phasor f = do
  t <- _.time <$> get
  pure $ fract $ productFloatExpr t f

tri :: forall a. Expr a => a -> G a
tri f = phasor f >>= flip difference (constant 0.5) >>> abs >>> product (constant 4.0) >>> difference (constant 1.0) >>> assign

saw :: forall a. Expr a => a -> G a
saw f = phasor f >>= bipolar >>> assign

sqr :: forall a. Expr a => a -> G a
sqr f = phasor f >>= greaterThanEqual (constant 0.5) >>> bipolar >>> assign

acosh :: forall a. Expr a => a -> G a
acosh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.acosh (function1 "acosh") x
    false -> assign $ log $ add x $ sqrt $ difference (product x x) (constant (1.0))

asinh :: forall a. Expr a => a -> G a
asinh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.asinh (function1 "asinh") x
    false -> assign $ log $ add x $ sqrt $ add (product x x) (constant (1.0))

atanh :: forall a. Expr a => a -> G a
atanh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.atanh (function1 "atanh") x
    false -> assign $ flip division (constant 2.0) $ log $ division (add (constant 1.0) x) (difference (constant 1.0) x)

cosh :: forall a. Expr a => a -> G a
cosh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.cosh (function1 "cosh") x
    false -> assign $ flip division (constant 2.0) $ add (exp x) (exp (product x (constant (-1.0))))

sinh :: forall a. Expr a => a -> G a
sinh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.sinh (function1 "sinh") x
    false -> assign $ flip division (constant 2.0) $ difference (exp x) (exp (product x (constant (-1.0))))

tanh :: forall a. Expr a => a -> G a
tanh x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.tanh (function1 "tanh") x
    false -> (division <$> sinh x <*> cosh x) >>= assign

trunc :: forall a. Expr a => a -> G a
trunc x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.trunc (function1 "trunc") x
    false -> assign $ product (sign x) (floor (abs x))

round :: forall a. Expr a => a -> G a
round x = do
  s <- get
  case s.webGl2 of
    true -> assign $ unaryFunction Number.round (function1 "round") x
    false -> assign $ add (floor x) (constant 0.5)

spin :: Vec2 -> Float -> G Vec2
spin fxy a = do
  aPi <- assign $ product a pi
  ct <- assign $ cos aPi
  st <- assign $ sin aPi
  let x = difference (product (swizzleX fxy) ct) (product (swizzleY fxy) st)
  let y = add (product (swizzleY fxy) ct) (product (swizzleX fxy) st)
  assign $ floatFloatToVec2 x y

rect :: Vec2 -> Vec2 -> Vec2 -> G Float
rect fxy xy wh = do
  let a = product pxy (constant 1.5)
  let b = abs (difference fxy xy)
  let c = abs (product wh (constant 0.5))
  let d = difference b c
  let e = (expr $ "smoothstep(vec2(0.)," <> toExpr a <> "," <> toExpr d <> ")") :: Vec2
  f <- assign $ difference (constant 1.0) e
  assign $ product (swizzleX f) (swizzleY f)


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
float div(float x,float y) { return (y != 0. ? x/y : 0.); }
vec2 div(vec2 x,vec2 y) { return vec2(div(x.x,y.x),div(x.y,y.y)); }
vec3 div(vec3 x,vec3 y) { return vec3(div(x.x,y.x),div(x.y,y.y),div(x.z,y.z)); }
vec4 div(vec4 x,vec4 y) { return vec4(div(x.x,y.x),div(x.y,y.y),div(x.z,y.z),div(x.w,y.w)); }
vec2 div(float x,vec2 y) { return vec2(div(x,y.x),div(x,y.y)); }
vec3 div(float x,vec3 y) { return vec3(div(x,y.x),div(x,y.y),div(x,y.z)); }
vec4 div(float x,vec4 y) { return vec4(div(x,y.x),div(x,y.y),div(x,y.z),div(x,y.w)); }
vec2 div(vec2 x,float y) { return vec2(div(x.x,y),div(x.y,y)); }
vec3 div(vec3 x,float y) { return vec3(div(x.x,y),div(x.y,y),div(x.z,y)); }
vec4 div(vec4 x,float y) { return vec4(div(x.x,y),div(x.y,y),div(x.z,y),div(x.w,y)); }
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
  mExpr <- do
    let n = Prelude.max 0 (Prelude.max (List.length oldProgram.actions) (List.length newProgram.actions) - List.length oldProgram.actions)
    let oldActions = oldProgram.actions <> replicate n Nothing
    foldM (appendActions tempo newProgram.evalTime) Nothing (List.zip oldActions newProgram.actions)
  case mExpr of
    Nothing -> pure $ constant 0.0
    Just v4 -> pure v4


appendActions :: Tempo -> DateTime -> Maybe Vec4 -> Tuple (Maybe Action) (Maybe Action) -> G (Maybe Vec4)
appendActions _ _ mPrevOutput (Tuple _ Nothing) = pure mPrevOutput
appendActions tempo eTime mPrevOutput (Tuple mPrevAction (Just newAction)) = do
  let Tuple t0 t1 = actionTimesAsSecondsSinceEval tempo eTime newAction
  appendSignals newAction.output t0 t1 mPrevOutput (_.signal <$> mPrevAction) newAction.signal


appendSignals :: Output -> Number -> Number -> Maybe Vec4 -> Maybe Signal -> Signal -> G (Maybe Vec4)

appendSignals Output.Blend t0 t1 mPrevOutput mPrevSignal newSignal = do
  newRGBAs <- temp1 newSignal
  rgbas <- case mPrevSignal of
           Just prevSignal -> do
             case prevSignal == newSignal of
               true -> traverse assign newRGBAs
               false -> do
                 prevRGBAs <- temp1 prevSignal
                 crossFadeRGBAsBlend t0 t1 prevRGBAs newRGBAs
           Nothing -> do
             fIn <- assign $ fadeIn t0 t1
             traverse (assign <<< rgbaFade fIn) newRGBAs
  case mPrevOutput of
    Nothing -> Just <$> foldM (\x y -> assign $ blend x y) (head rgbas) (tail rgbas)
    Just prevOutput -> Just <$> foldM (\x y -> assign $ blend x y) prevOutput rgbas

appendSignals Output.RGBA t0 t1 _ mPrevSignal newSignal = do
  newRGBA <- temp2 newSignal
  case mPrevSignal of
    Just prevSignal -> do
      case prevSignal == newSignal of
        true -> Just <$> assign newRGBA
        false -> do
          prevRGBA <- temp2 prevSignal
          fIn <- assign $ fadeIn t0 t1
          Just <$> (assign $ mixFloat prevRGBA newRGBA fIn)
    Nothing -> Just <$> assign newRGBA

appendSignals Output.Add t0 t1 mPrevOutput mPrevSignal newSignal = do
  newRGBs <- temp1 newSignal
  rgbs <- case mPrevSignal of
    Just prevSignal -> do
      case prevSignal == newSignal of
        true -> traverse assign newRGBs
        false -> do
          prevRGBs <- temp1 prevSignal
          crossFadeRGBsAdd t0 t1 prevRGBs newRGBs
    Nothing -> do
      fIn <- assign $ fadeIn t0 t1
      traverse (assign <<< productFloatExpr fIn) newRGBs
  case mPrevOutput of
    Nothing -> (Just <<< flip vec3FloatToVec4 (constant 1.0)) <$> foldM (\x y -> assign $ add x y) (head rgbs) (tail rgbs)
    Just prevOutput -> (Just <<< flip vec3FloatToVec4 (constant 1.0)) <$> foldM (\x y -> assign $ add x y) (swizzleXYZ prevOutput) rgbs

appendSignals Output.Mul t0 t1 mPrevOutput mPrevSignal newSignal = do
  newRGBs <- temp1 newSignal
  rgbs <- case mPrevSignal of
    Just prevSignal -> do
      case prevSignal == newSignal of
        true -> traverse assign newRGBs
        false -> do
          prevRGBs <- temp1 prevSignal
          crossFadeRGBsMul t0 t1 prevRGBs newRGBs
    Nothing -> do
      fIn <- assign $ fadeIn t0 t1
      traverse (\x -> assign $ mixFloat (constant 1.0) x fIn) newRGBs
  case mPrevOutput of
    Nothing -> (Just <<< flip vec3FloatToVec4 (constant 1.0)) <$> foldM (\x y -> assign $ product x y) (head rgbs) (tail rgbs)
    Just prevOutput -> (Just <<< flip vec3FloatToVec4 (constant 1.0)) <$> foldM (\x y -> assign $ product x y) (swizzleXYZ prevOutput) rgbs

appendSignals Output.RGB t0 t1 _ mPrevSignal newSignal = do
  newRGB <- temp2 newSignal
  case mPrevSignal of
    Just prevSignal -> do
      case prevSignal == newSignal of
        true -> Just <$> assign (vec3FloatToVec4 newRGB (constant 1.0))
        false -> do
          prevRGB <- temp2 prevSignal
          fIn <- assign $ fadeIn t0 t1
          Just <$> (assign $ vec3FloatToVec4 (mixFloat prevRGB newRGB fIn) (constant 1.0))
    Nothing -> Just <$> assign (vec3FloatToVec4 newRGB (constant 1.0))

appendSignals _ _ _ mPrevOutput _ _ = pure mPrevOutput


temp1 :: forall a. Expr a => Signal -> G (NonEmptyList a)
temp1 x = castExprs <$> flatten <$> (signalToExprs x :: G (Matrix Float))

temp2 :: forall a. Expr a => Signal -> G a
temp2 x = last <$> castExprs <$> flatten <$> (signalToExprs x :: G (Matrix Float))


crossFadeRGBAsBlend :: Number -> Number -> NonEmptyList Vec4 -> NonEmptyList Vec4 -> G (NonEmptyList Vec4)
crossFadeRGBAsBlend t0 t1 prevRGBAs newRGBAs = do
  fIn <- assign $ fadeIn t0 t1
  rgbasInCommon <- sequence $ zipWith (\x y -> assign $ mixFloat x y fIn) prevRGBAs newRGBAs -- when we have both a previous and a new signal, rgba goes from prevSignal to newSignal over course of fade time
  let nInCommon = Prelude.min (length prevRGBAs) (length newRGBAs)
  additionalRGBAs <- case length prevRGBAs > length newRGBAs of
    true -> do
      fOut <- assign $ fadeOut t0 t1
      traverse (assign <<< rgbaFade fOut) $ drop nInCommon prevRGBAs -- when we have an old signal but no previous signal, a goes from (a*1) to 0 over course of fade time
    false -> traverse (assign <<< rgbaFade fIn) $ drop nInCommon newRGBAs  -- when we have a new signal but no previous signal, a goes from 0 to (a*1) over course of fade time
  case fromList additionalRGBAs of
    Just xs -> pure $ rgbasInCommon <> xs
    Nothing -> pure $ rgbasInCommon

crossFadeRGBsAdd :: Number -> Number -> NonEmptyList Vec3 -> NonEmptyList Vec3 -> G (NonEmptyList Vec3)
crossFadeRGBsAdd t0 t1 prevRGBs newRGBs = do
  fIn <- assign $ fadeIn t0 t1
  rgbsInCommon <- sequence $ zipWith (\x y -> assign $ mixFloat x y fIn) prevRGBs newRGBs
  let nInCommon = Prelude.min (length prevRGBs) (length newRGBs)
  additionalRGBs <- case length prevRGBs > length newRGBs of
    true -> traverse (\x -> assign $ mixFloat x (constant 0.0) fIn) $ drop nInCommon prevRGBs
    false -> traverse (\x -> assign $ mixFloat (constant 0.0) x fIn) $ drop nInCommon newRGBs
  case fromList additionalRGBs of
    Just xs -> pure $ rgbsInCommon <> xs
    Nothing -> pure rgbsInCommon

crossFadeRGBsMul :: Number -> Number -> NonEmptyList Vec3 -> NonEmptyList Vec3 -> G (NonEmptyList Vec3)
crossFadeRGBsMul t0 t1 prevRGBs newRGBs = do
  fIn <- assign $ fadeIn t0 t1
  rgbsInCommon <- sequence $ zipWith (\x y -> assign $ mixFloat x y fIn) prevRGBs newRGBs
  let nInCommon = Prelude.min (length prevRGBs) (length newRGBs)
  additionalRGBs <- case length prevRGBs > length newRGBs of
    true -> traverse (\x -> assign $ mixFloat x (constant 1.0) fIn) $ drop nInCommon prevRGBs
    false -> traverse (\x -> assign $ mixFloat (constant 1.0) x fIn) $ drop nInCommon newRGBs
  case fromList additionalRGBs of
    Just xs -> pure $ rgbsInCommon <> xs
    Nothing -> pure rgbsInCommon

fragmentShader :: Boolean -> Tempo -> Map String Int -> Map String Int -> Program -> Program -> String
fragmentShader webGl2 tempo imgMap vidMap oldProgram newProgram = header webGl2 <> st.code <> gl_FragColor <> "}"
  where
    (Tuple v4 st) = runG webGl2 imgMap vidMap $ programsToGLSL tempo oldProgram newProgram
    fragColorVarName = if webGl2 then "fragColor" else "gl_FragColor"
    preMulAlpha = vec3FloatToVec4 (productExprFloat (swizzleXYZ v4) (swizzleW v4)) (swizzleW v4)
    gl_FragColor = fragColorVarName <> " = " <> toExpr preMulAlpha <> ";\n"
