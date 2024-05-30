module FragmentShader where

import Prelude(($),pure,show,bind,discard,(<>),(>>=),(<$>),(<<<),map,(==),(&&),otherwise,(>>>),(<*>),flip,negate,(>))
import Prelude as Prelude
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
import Data.Number (acos, asin, atan, ceil, cos, exp, floor, log, pow, round, sign, sin, sqrt, tan, trunc) as Number
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList,drop)
import Data.List (zip)

import NonEmptyList (zipWithEqualLength,everyPair,everyAdjacentPair)
import MultiMode (MultiMode(..))
import Signal (Signal(..))
import Action (Action,actionTimesAsSecondsSinceEval)
import Output (Output)
import Output as Output
import Program (Program)
import Expr
import G
import Multi (Multi,fromNonEmptyListMulti,mapRows,flatten,fromNonEmptyList,rep,combine,combine3,semiFlatten)
import Number (acosh, asinh, atanh, between, cbrt, clip, cosh, log10, log2, sinh, tanh, division, smoothStep) as Number


signalToExprs :: forall a. Expr a => Signal -> G (Multi a)

signalToExprs (Constant x) = pure $ pure $ constant x

signalToExprs (SignalList xs) = 
  case fromList xs of
    Nothing -> pure $ pure $ constant 0.0
    Just xs' -> do
      xs'' <- fromNonEmptyListMulti <$> traverse signalToExprs xs' -- :: Multi Float, with list already combinatorially expanded
      pure $ mapRows fromFloats xs''

signalToExprs (Append x y) = (<>) <$> signalToExprs x <*> signalToExprs y

signalToExprs (Zip x y) = do
  xs <- flatten <$> (signalToExprs x :: G (Multi Float))
  ys <- flatten <$> (signalToExprs y :: G (Multi Float))
  let v2s = fromNonEmptyList $ zipWithEqualLength floatFloatToVec2 xs ys
  pure $ mapRows fromVec2s v2s

signalToExprs (Mono x) = do
  xs <- (signalToExprs x :: G (Multi Vec3)) -- MAYBE LATER: determine an optimal type for realization of x based on its number of channels or the nature of the underlying signal (eg. whether it naturally produces vec2s 3s or 4s)
  let f = sum (dotSum <$> flatten xs)
  pure $ pure $ fromFloat f
  
signalToExprs (Rep n x) = rep n <$> signalToExprs x

signalToExprs Pi = pure $ pure $ fromFloat $ FloatExpr "PI"

signalToExprs Px = pure $ pure $ fromFloat $ FloatExpr "(2./res.x)"

signalToExprs Py = pure $ pure $ fromFloat $ FloatExpr "(2./res.y)"

signalToExprs Pxy = pure $ fromNonEmptyList $ fromVec2s $ singleton $ Vec2Expr "(2./res)"

signalToExprs Aspect = pure $ pure $ fromFloat $ FloatExpr "(res.x/res.y)"

signalToExprs Fx = (pure <<< fromFloat <<< swizzleX <<< _.fxy) <$> get

signalToExprs Fy = (pure <<< fromFloat <<< swizzleY <<< _.fxy) <$> get

signalToExprs Fxy = (fromNonEmptyList <<< fromVec2s <<< singleton <<< _.fxy) <$> get

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

signalToExprs FFT = get >>= (_.fxy >>> unipolar >>> textureFFT "o" >>> fromFloat >>> pure >>> maskUnitSquare)

signalToExprs IFFT = get >>= (_.fxy >>> unipolar >>> textureFFT "i" >>> fromFloat >>> pure >>> maskUnitSquare)

signalToExprs Fb = get >>= (_.fxy >>> unipolar >>> texture2D "f" >>> singleton >>> fromVec3s >>> fromNonEmptyList >>> maskUnitSquare)

signalToExprs Cam = get >>= (_.fxy >>> unipolar >>> texture2D "w" >>> singleton >>> fromVec3s >>> fromNonEmptyList >>> maskUnitSquare)

signalToExprs (Img url) = do
  s <- get
  case lookup url s.imgMap of
    Just n -> do
      t <- assign $ texture2D ("t" <> show n) (unipolar s.fxy) -- :: Vec3
      maskUnitSquare $ fromNonEmptyList $ fromVec3s $ singleton t
    Nothing -> pure $ pure $ zero

signalToExprs (Vid url) = do
  s <- get
  case lookup url s.vidMap of
    Just n -> do
      t <- assign $ texture2D ("t" <> show n) (unipolar s.fxy) -- :: Vec3
      maskUnitSquare $ fromNonEmptyList $ fromVec3s $ singleton t
    Nothing -> pure $ pure $ zero

signalToExprs (Blend x) = do
  xs <- flatten <$> (signalToExprs x :: G (Multi Vec4))
  y <- (foldM (\a b -> assign $ blend a b) (head xs) (tail xs) :: G Vec4)
  pure $ fromNonEmptyList $ fromVec4s $ singleton y

signalToExprs (Add x) = do
  xs <- flatten <$> (signalToExprs x :: G (Multi Vec3))
  y <- (foldM (\a b -> assign $ add a b) (head xs) (tail xs) :: G Vec3)
  pure $ fromNonEmptyList $ fromVec3s $ singleton y

signalToExprs (Mul x) = do
  xs <- flatten <$> (signalToExprs x :: G (Multi Vec3))
  y <- (foldM (\a b -> assign $ product a b) (head xs) (tail xs) :: G Vec3)
  pure $ fromNonEmptyList $ fromVec3s $ singleton y
  
signalToExprs (RgbHsv x) = signalToExprs x >>= map rgbhsv >>> mapRows fromVec3s >>> traverse assign
signalToExprs (HsvRgb x) = signalToExprs x >>= map hsvrgb >>> mapRows fromVec3s >>> traverse assign
signalToExprs (HsvR x) = signalToExprs x >>= map hsvrgb >>> map swizzleX >>> mapRows fromFloats >>> traverse assign
signalToExprs (HsvG x) = signalToExprs x >>= map hsvrgb >>> map swizzleY >>> mapRows fromFloats >>> traverse assign
signalToExprs (HsvB x) = signalToExprs x >>= map hsvrgb >>> map swizzleZ >>> mapRows fromFloats >>> traverse assign
signalToExprs (RgbR x) = (signalToExprs x :: G (Multi Vec3)) >>= map swizzleX >>> mapRows fromFloats >>> pure
signalToExprs (RgbG x) = (signalToExprs x :: G (Multi Vec3)) >>= map swizzleY >>> mapRows fromFloats >>> pure
signalToExprs (RgbB x) = (signalToExprs x :: G (Multi Vec3)) >>= map swizzleZ >>> mapRows fromFloats >>> pure
signalToExprs (RgbH x) = signalToExprs x >>= map rgbhsv >>> map swizzleX >>> mapRows fromFloats >>> traverse assign
signalToExprs (RgbS x) = signalToExprs x >>= map rgbhsv >>> map swizzleY >>> mapRows fromFloats >>> traverse assign
signalToExprs (RgbV x) = signalToExprs x >>= map rgbhsv >>> map swizzleZ >>> mapRows fromFloats >>> traverse assign

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

signalToExprs (RtXy rt) = signalToExprs rt >>= traverse assign >>= map rtxy >>> traverse assign >>= mapRows fromVec2s >>> pure
signalToExprs (RtX rt) = signalToExprs rt >>= traverse assign >>= map rtx >>> traverse assign >>= mapRows fromFloats >>> pure
signalToExprs (RtY rt) = signalToExprs rt >>= traverse assign >>= map rty >>> traverse assign >>= mapRows fromFloats >>> pure
signalToExprs (XyRt xy) = signalToExprs xy >>= traverse assign >>= map xyrt >>> traverse assign >>= mapRows fromVec2s >>> pure
signalToExprs (XyR xy) = signalToExprs xy >>= traverse assign >>= map xyr >>> traverse assign >>= mapRows fromFloats >>> pure
signalToExprs (XyT xy) = signalToExprs xy >>= traverse assign >>= map xyt >>> traverse assign >>= mapRows fromFloats >>> pure
  
signalToExprs (Distance xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows fromFloats $ map (distance fxy) xys

signalToExprs (Prox xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows fromFloats $ map (prox fxy) xys
  
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
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (divisionExprFloat fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomXy q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Vec2)
  fxys <- traverse assign $ map (division fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomX q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (setX fxy <<< division (swizzleX fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (ZoomY q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (setY fxy <<< division (swizzleY fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z
 
signalToExprs (Move q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Vec2)
  fxys <- traverse assign $ map (difference fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Tile q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (tile fxy <<< fromFloat) $ flatten qs
  withFxys fxys $ signalToExprs z
  
signalToExprs (TileXy q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Vec2)
  fxys <- traverse assign $ map (tile fxy) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (TileX q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (setX fxy <<< tile (swizzleX fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (TileY q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
  fxys <- traverse assign $ map (setY fxy <<< tile (swizzleY fxy)) $ flatten qs
  withFxys fxys $ signalToExprs z

signalToExprs (Spin q z) = do
  fxy <- _.fxy <$> get
  qs <- signalToExprs q -- :: G (Multi Float)
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
  
signalToExprs (Addition mm x y) = binaryFunctionToExprs mm add x y
signalToExprs (Difference mm x y) = binaryFunctionToExprs mm difference x y
signalToExprs (Product mm x y) = binaryFunctionToExprs mm product x y
signalToExprs (Division mm x y) = binaryFunctionToExprs mm division x y
signalToExprs (Mod mm x y) = binaryFunctionToExprs mm mod x y
signalToExprs (Pow mm x y) = binaryFunctionToExprs mm pow x y
signalToExprs (Equal mm x y) = binaryFunctionToExprs mm equal x y
signalToExprs (NotEqual mm x y) = binaryFunctionToExprs mm notEqual x y
signalToExprs (GreaterThan mm x y) = binaryFunctionToExprs mm greaterThan x y
signalToExprs (GreaterThanEqual mm x y) = binaryFunctionToExprs mm greaterThanEqual x y
signalToExprs (LessThan mm x y) = binaryFunctionToExprs mm lessThan x y
signalToExprs (LessThanEqual mm x y) = binaryFunctionToExprs mm lessThanEqual x y
signalToExprs (Max mm x y) = binaryFunctionToExprs mm max x y
signalToExprs (Min mm x y) = binaryFunctionToExprs mm min x y

signalToExprs (Gate mm x y) = do
  xs <- signalToExprs x
  ys <- signalToExprs y >>= traverse assign
  traverse assign $ combine gate mm xs ys

signalToExprs (Clip mm r x) = do
  rs <- signalToExprs r >>= traverse assign
  xs <- signalToExprs x
  traverse assign $ combine clip mm rs xs

signalToExprs (Between mm r x) = do
  rs <- signalToExprs r >>= traverse assign
  xs <- signalToExprs x >>= traverse assign
  traverse assign $ combine between mm rs xs

signalToExprs (SmoothStep mm r x) = do
  rs <- signalToExprs r >>= traverse assign
  xs <- signalToExprs x
  traverse assign $ combine smoothStep mm rs xs

signalToExprs (Circle mm xy d) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  ds <- signalToExprs d
  traverse assign $ combine (circle fxy) mm xys ds

signalToExprs (Rect mm xy wh) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  whs <- signalToExprs wh
  rs <- sequence $ combine (rect fxy) mm xys whs
  pure $ mapRows fromFloats rs

signalToExprs (VLine mm x w) = do
  fxy <- _.fxy <$> get
  xs <- signalToExprs x
  ws <- signalToExprs w >>= traverse assign
  traverse assign $ combine (vline fxy) mm xs ws

signalToExprs (HLine mm y h) = do
  fxy <- _.fxy <$> get
  ys <- signalToExprs y
  hs <- signalToExprs h >>= traverse assign
  traverse assign $ combine (hline fxy) mm ys hs

signalToExprs (Chain mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  ws <- signalToExprs w
  let xys' = fromNonEmptyList $ everyAdjacentPair $ flatten xys -- Multi (Tuple Vec2 Vec2)
  let f xyTuple theW = line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ mapRows fromFloats $ combine f mm xys' ws

signalToExprs (Lines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToExprs xy >>= traverse assign
  ws <- signalToExprs w 
  let f v4 theW = line fxy (swizzleXY v4) (swizzleZW v4) theW
  traverse assign $ mapRows fromFloats $ combine f mm xysVec4 ws

signalToExprs (ILines mm xy w) = do
  fxy <- _.fxy <$> get
  xysVec4 <- signalToExprs xy >>= traverse assign
  ws <- signalToExprs w 
  let f v4 theW = iline fxy (swizzleXY v4) (swizzleZW v4) theW
  traverse assign $ mapRows fromFloats $ combine f mm xysVec4 ws

signalToExprs (Mesh mm xy w) = do
  fxy <- _.fxy <$> get
  xys <- flatten <$> signalToExprs xy
  ws <- signalToExprs w
  let xys' = fromNonEmptyList $ everyPair $ xys -- Multi (Tuple Vec2 Vec2)
  let f xyTuple theW = line fxy (fst xyTuple) (snd xyTuple) theW
  traverse assign $ mapRows fromFloats $ combine f mm xys' ws

signalToExprs (ILine mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToExprs xy1
  xy2s <- signalToExprs xy2
  ws <- signalToExprs w
  traverse assign $ mapRows fromFloats $ combine3 (iline fxy) mm xy1s xy2s ws

signalToExprs (Line mm xy1 xy2 w) = do
  fxy <- _.fxy <$> get
  xy1s <- signalToExprs xy1
  xy2s <- signalToExprs xy2
  ws <- signalToExprs w
  traverse assign $ mapRows fromFloats $ combine3 (line fxy) mm xy1s xy2s ws

signalToExprs (Point xy) = do
  fxy <- _.fxy <$> get
  xys <- signalToExprs xy
  traverse assign $ mapRows fromFloats $ map (point fxy) xys
  
signalToExprs (LinLin mm r1 r2 x) = do
  r1s <- signalToExprs r1
  r2s <- signalToExprs r2
  xs <- signalToExprs x
  traverse assign $ combine3 linlin mm r1s r2s xs
  
signalToExprs (Mix mm x y a) = do
  x' <- signalToExprs x
  y' <- signalToExprs y
  let xys = fromNonEmptyList $ zipWithEqualLength Tuple (flatten x') (flatten y')
  a' <- signalToExprs a
  let f (Tuple xx yy) aa = mix xx yy aa
  traverse assign $ combine f mm xys a'
  
signalToExprs (Seq steps) = do
  steps' <- semiFlatten <$> signalToExprs steps -- :: NonEmptyList (NonEmptyList Float)
  b <- get >>= _.beat >>> fract >>> pure
  chs <- traverse assign $ map (seq b) steps' -- :: NonEmptyList Float  ; seq :: Float -> NonEmptyList Float -> Float
  traverse assign $ fromNonEmptyList $ fromFloats chs

signalToExprs _ = pure $ pure $ constant 0.0


{-
-- for preserving alignment requests through intermediate calculations that are Float -> G Float
-- created during matrix refactor but currently unused???
splitFloatsApplyReassemble :: forall a. Expr a => (Float -> G Float) -> Multi a -> G (Multi a)
splitFloatsApplyReassemble f xs = do
  xs'' <- traverse f $ mapRows toFloats xs 
  pure $ mapRows fromFloats xs''
-}

maskUnitSquare :: forall a. Expr a => Multi a -> G (Multi a)
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
sqr f = phasor f >>= bipolar >>> greaterThanEqual (constant 0.5) >>> assign


binaryFunctionToExprs :: forall a. Expr a => MultiMode -> (a -> a -> a) -> Signal -> Signal -> G (Multi a)
binaryFunctionToExprs Combinatorial f x y = combine f Combinatorial <$> (map fromFloat <$> signalToExprs x) <*> signalToExprs y
binaryFunctionToExprs Pairwise f x y = combine f Pairwise <$> signalToExprs x <*> signalToExprs y

  
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
    let oldActions = oldProgram.actions -- PLACEHOLDER!!! TODO ...pad xs to make it as long as ys...
    foldM (appendActions tempo newProgram.evalTime) Nothing (zip oldActions newProgram.actions) 
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
  newRGBAs <- flatten <$> signalToExprs newSignal
  rgbas <- case mPrevSignal of
           Just prevSignal -> do
             prevRGBAs <- flatten <$> signalToExprs prevSignal 
             crossFadeRGBAs t0 t1 prevRGBAs newRGBAs             
           Nothing -> do
             fIn <- assign $ fadeIn t0 t1
             traverse (assign <<< rgbaFade fIn) newRGBAs
  case mPrevOutput of
    Nothing -> Just <$> foldM (\x y -> assign $ blend x y) (head rgbas) (tail rgbas)
    Just prevOutput -> Just <$> foldM (\x y -> assign $ blend x y) prevOutput rgbas
  
{-
appendSignals Output.RGBA t0 t1 _ mPrevSignal newSignal = do
-- note: maybe this is not quite the same as blend because of the way only "last" signals count when there is more than one vec4

appendSignals Output.Add t0 t1 mPrevOutput mPrevSignal newSignal = do

appendSignals Output.Mul t0 t1 mPrevOutput mPrevSignal newSignal = do

appendSignals Output.RGB t0 t1 mPrevOutput mPrevSignal newSignal = do
-}

appendSignals _ _ _ mPrevOutput _ _ = pure mPrevOutput
  
    
crossFadeRGBAs :: Number -> Number -> NonEmptyList Vec4 -> NonEmptyList Vec4 -> G (NonEmptyList Vec4)
crossFadeRGBAs t0 t1 prevRGBAs newRGBAs = do
  fIn <- assign $ fadeIn t0 t1
  rgbasInCommon <- sequence $ zipWith (\x y -> assign $ mix x y fIn) prevRGBAs newRGBAs -- when we have both a previous and a new signal, rgba goes from prevSignal to newSignal over course of fade time
  let nInCommon = Prelude.min (length prevRGBAs) (length newRGBAs)
  additionalRGBAs <- case length prevRGBAs > length newRGBAs of
    true -> do
      fOut <- assign $ fadeOut t0 t1
      traverse (assign <<< rgbaFade fOut) $ drop nInCommon prevRGBAs -- when we have an old signal but no previous signal, a goes from (a*1) to 0 over course of fade time           
    false -> traverse (assign <<< rgbaFade fIn) $ drop nInCommon newRGBAs  -- when we have a new signal but no previous signal, a goes from 0 to (a*1) over course of fade time
  case fromList additionalRGBAs of
    Just xs -> pure $ rgbasInCommon <> xs
    Nothing -> pure $ rgbasInCommon
      
      
    
    
fragmentShader :: Boolean -> Tempo -> Map String Int -> Map String Int -> Program -> Program -> String
fragmentShader webGl2 tempo imgMap vidMap oldProgram newProgram = header webGl2 <> st.code <> gl_FragColor <> "}"
  where
    (Tuple v4 st) = runG webGl2 imgMap vidMap $ programsToGLSL tempo oldProgram newProgram
    fragColorVarName = if webGl2 then "fragColor" else "gl_FragColor"
    gl_FragColor = fragColorVarName <> " = " <> toExpr v4 <> ";\n"
