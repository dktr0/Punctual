{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Sound.Punctual.FragmentShader where

import Data.IntMap.Strict as IntMap
import Data.Text (Text)
import Data.Semigroup ((<>))
import TextShow
import Data.Map as Map
import Data.Foldable as Foldable
import Data.Maybe
import Data.List.Split
import Data.Time
import Data.Tempo
import Control.Monad


import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((<>),(>>))
import Sound.Punctual.Program
import Sound.Punctual.GLSLExpr
import Sound.Punctual.GLSL


type GraphEnv = (Map TextureRef Int, [GLSLExpr]) -- texture map, fxy expressions

testGraphEnv :: GraphEnv
testGraphEnv = (Map.empty,[defaultFxy] )


-- graphToGLSL MUST return a non-empty list, so
-- generally speaking, in cases where there is no meaningful return value,
-- the return value will be a one-item list containing a 0 :: GLFloat


graphToGLSL :: AlignHint -> GraphEnv -> Graph -> GLSL [GLSLExpr]

-- constants and uniforms

graphToGLSL _ _ (Constant x) = return [ constantFloat x ]
graphToGLSL _ _ Px = return [px]
graphToGLSL _ _ Py = return [py]
graphToGLSL _ _ Aspect = return [glFloat "(width/height)"]
graphToGLSL _ _ Lo = return [glFloat "lo"]
graphToGLSL _ _ Mid = return [glFloat "mid"]
graphToGLSL _ _ Hi = return [glFloat "hi"]
graphToGLSL _ _ ILo = return [glFloat "ilo"]
graphToGLSL _ _ IMid = return [glFloat "imid"]
graphToGLSL _ _ IHi = return [glFloat "ihi"]
graphToGLSL _ _ Cps = return [glFloat "_cps"]
graphToGLSL _ _ Time = return [glFloat "_time"]
graphToGLSL _ _ Beat = return [glFloat "_beat"]
graphToGLSL _ _ ETime = return [glFloat "_etime"]
graphToGLSL _ _ EBeat = return [glFloat "_ebeat"]
graphToGLSL ah (_,fxy) Fx = alignHint ah $ fmap swizzleX fxy
graphToGLSL ah (_,fxy) Fy = alignHint ah $ fmap swizzleY fxy
graphToGLSL ah (_,fxy) Fxy = alignHint ah $ fxy

-- multichannel operations: multi, mono, rep, unrep, Append (++)

graphToGLSL ah env (Multi xs) = multiToGLSL ah env xs

graphToGLSL ah env (Append xs ys) = do
  xs' <- graphToGLSL ah env xs
  ys' <- graphToGLSL ah env ys
  return $ xs' ++ ys'

graphToGLSL _ env (Mono x) = do
  x' <- graphToGLSL (Just GLFloat) env x
  case x' of
    [] -> return [constantFloat 0]
    _ -> do
      xs <- align GLFloat x'
      return [ Foldable.foldr1 (+) xs ]

graphToGLSL ah env (Rep n x) = do
  x' <- mapM (graphToGLSL Nothing env) $ replicate n x
  alignHint ah $ concat x'

graphToGLSL _ _ (UnRep 0 _) = return [constantFloat 0]
graphToGLSL ah env (UnRep n x) = do
  x' <- graphToGLSL (Just GLFloat) env x >>= align GLFloat
  alignHint ah $ fmap (Foldable.foldr1 (+)) $ chunksOf n x'

-- unary functions

graphToGLSL ah env (Bipolar x) = graphToGLSL ah env x >>= return . fmap bipolar
graphToGLSL ah env (Unipolar x) =graphToGLSL ah env x >>= return . fmap unipolar
graphToGLSL ah env (Sin x) = do
  x' <- graphToGLSL ah env x
  return $ fmap (unaryFunctionMatched "sin" . (*) (constantFloat 3.14159265 * constantFloat 2 * _time)) x'
graphToGLSL ah env (MidiCps x) = graphToGLSL ah env x >>= return . fmap midicps
graphToGLSL ah env (CpsMidi x) = graphToGLSL ah env x >>= return . fmap cpsmidi
graphToGLSL ah env (DbAmp x) = graphToGLSL ah env x >>= return . fmap dbamp
graphToGLSL ah env (AmpDb x) = graphToGLSL ah env x >>= return . fmap ampdb
graphToGLSL ah env (Abs x) = graphToGLSL ah env x >>= return . fmap (unaryFunctionMatched "abs")
graphToGLSL ah env (Sqrt x) = graphToGLSL ah env x >>= return . fmap (unaryFunctionMatched "sqrt")
graphToGLSL ah env (Floor x) = graphToGLSL ah env x >>= return . fmap (unaryFunctionMatched "floor")
graphToGLSL ah env (Ceil x) = graphToGLSL ah env x >>= return . fmap (unaryFunctionMatched "ceil")
graphToGLSL ah env (Fract x) = graphToGLSL ah env x >>= return . fmap (unaryFunctionMatched "fract")
graphToGLSL ah env (Tri x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah . fmap (unaryFunctionMatched "tri")
graphToGLSL ah env (Saw x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah . fmap (unaryFunctionMatched "saw")
graphToGLSL ah env (Sqr x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah. fmap (unaryFunctionMatched "sqr")
graphToGLSL ah env (LFTri x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah . fmap (unaryFunctionMatched "tri")
graphToGLSL ah env (LFSaw x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah . fmap (unaryFunctionMatched "saw")
graphToGLSL ah env (LFSqr x) = graphToGLSL (Just GLFloat) env x >>= align GLFloat >>= alignHint ah . fmap (unaryFunctionMatched "sqr")
graphToGLSL _ env (Blend x) = do
  xs <- graphToGLSL (Just Vec4) env x >>= alignRGBA
  case length xs of
    1 -> return xs
    _ -> foldM blend (head xs) (tail xs) >>= (return . pure)
graphToGLSL _ env (HsvRgb x) = graphToGLSL (Just Vec3) env x >>= align Vec3 >>= return . fmap (unaryFunctionMatched "hsvrgb")
graphToGLSL _ env (RgbHsv x) = graphToGLSL (Just Vec3) env x >>= align Vec3 >>= return . fmap (unaryFunctionMatched "rgbhsv")
graphToGLSL ah env (HsvH x) = graphToGLSL (Just Vec3) env x >>= align Vec3 >>= alignHint ah . fmap swizzleX
graphToGLSL ah env (HsvS x) = graphToGLSL (Just Vec3) env x >>= align Vec3 >>= alignHint ah . fmap swizzleY
graphToGLSL ah env (HsvV x) = graphToGLSL (Just Vec3) env x >>= align Vec3 >>= alignHint ah . fmap swizzleZ
graphToGLSL ah env (HsvR x) = graphToGLSL (Just Vec3) env (HsvRgb x) >>= alignHint ah . fmap swizzleX
graphToGLSL ah env (HsvG x) = graphToGLSL (Just Vec3) env (HsvRgb x) >>= alignHint ah . fmap swizzleY
graphToGLSL ah env (HsvB x) = graphToGLSL (Just Vec3) env (HsvRgb x) >>= alignHint ah . fmap swizzleZ
graphToGLSL ah env (RgbR x) = graphToGLSL ah env (HsvH x)
graphToGLSL ah env (RgbG x) = graphToGLSL ah env (HsvS x)
graphToGLSL ah env (RgbB x) = graphToGLSL ah env (HsvV x)
graphToGLSL ah env (RgbH x) = graphToGLSL (Just Vec3) env (RgbHsv x) >>= alignHint ah . fmap swizzleX
graphToGLSL ah env (RgbS x) = graphToGLSL (Just Vec3) env (RgbHsv x) >>= alignHint ah . fmap swizzleY
graphToGLSL ah env (RgbV x) = graphToGLSL (Just Vec3) env (RgbHsv x) >>= alignHint ah . fmap swizzleZ


-- unary functions that access textures

graphToGLSL _ (texMap,fxy) (Img texRef) = texture2D ("tex" <> showb n) fxy
  where n = min 14 $ max 0 $ Map.findWithDefault 0 texRef texMap

graphToGLSL _ (texMap,fxy) (Vid texRef) = texture2D ("tex" <> showb n) fxy
  where n = min 14 $ max 0 $ Map.findWithDefault 0 texRef texMap

-- deprecated
graphToGLSL _ env@(texMap,_) (Tex texRef xy) = graphToGLSL (Just Vec2) env xy >>= texture2D ("tex" <> showb n)
  where n = min 14 $ max 0 $ Map.findWithDefault 0 texRef texMap

graphToGLSL _ env (Fb xy) = graphToGLSL (Just Vec2) env xy >>= texture2D "_fb"

graphToGLSL ah env (FFT x) = do
  a <- graphToGLSL (Just GLFloat) env (Unipolar x) >>= align GLFloat
  let b = zipWith exprExprToVec2 a (repeat 0) -- [GLSLExpr] where each is a Vec2
  c <- texture2D "_fft" b
  alignHint ah $ fmap swizzleX c

graphToGLSL ah env (IFFT x) = do
  a <- graphToGLSL (Just GLFloat) env (Unipolar x) >>= align GLFloat
  let b = zipWith exprExprToVec2 a (repeat 0)
  c <- texture2D "_ifft" b
  alignHint ah $ fmap swizzleX c

-- unary functions that access position
graphToGLSL ah env (Point xy) = unaryFunctionWithPosition point ah env xy
graphToGLSL ah env (Distance xy) = unaryFunctionWithPosition distance ah env xy
graphToGLSL ah env (Prox xy) = unaryFunctionWithPosition (binaryFunction "prox" GLFloat) ah env xy

-- unary transformations of position (technically binary functions, then)
graphToGLSL ah env (SetFx a b) = unaryPositionTransform setfx GLFloat ah env a b
graphToGLSL ah env (SetFy a b) = unaryPositionTransform setfy GLFloat ah env a b
graphToGLSL ah env (SetFxy a b) = unaryPositionTransform setfxy Vec2 ah env a b
graphToGLSL ah env (Zoom a b) = unaryPositionTransform zoom Vec2 ah env a b
graphToGLSL ah env (Move a b) = unaryPositionTransform move Vec2 ah env a b
graphToGLSL ah env (Tile a b) = unaryPositionTransform tile Vec2 ah env a b
graphToGLSL ah env (Spin a b) = unaryPositionTransform spin GLFloat ah env a b

-- (simple) binary functions
graphToGLSL ah env (Sum mm x y) = binaryMatchedGraphs mm (+) ah env x y
graphToGLSL ah env (Product mm x y) = binaryMatchedGraphs mm (*) ah env x y
graphToGLSL ah env (Division mm x y) = binaryMatchedGraphs mm (/) ah env x y
graphToGLSL ah env (Pow mm x y) = binaryMatchedGraphs mm pow ah env x y
graphToGLSL ah env (Equal mm x y) = binaryMatchedGraphs mm equal ah env x y
graphToGLSL ah env (NotEqual mm x y) = binaryMatchedGraphs mm notEqual ah env x y
graphToGLSL ah env (GreaterThan mm x y) = binaryMatchedGraphs mm greaterThan ah env x y
graphToGLSL ah env (GreaterThanOrEqual mm x y) = binaryMatchedGraphs mm greaterThanEqual ah env x y
graphToGLSL ah env (LessThan mm x y) = binaryMatchedGraphs mm lessThan ah env x y
graphToGLSL ah env (LessThanOrEqual mm x y) = binaryMatchedGraphs mm lessThanEqual ah env x y

graphToGLSL ah env (Max x y) = binaryMatchedGraphs Combinatorial (binaryFunctionMatched "max") ah env x y
graphToGLSL ah env (Min x y) = binaryMatchedGraphs Combinatorial (binaryFunctionMatched "min") ah env x y
graphToGLSL ah env (Gate x y) = do
  x' <- graphToGLSL ah env x
  y' <- graphToGLSL ah env y
  y'' <- mapM assign y'
  binaryMatchedGLSLExprs gate ah x' y''

graphToGLSL ah env (Clip r x) = do
  r' <- graphToGLSL (Just Vec2) env r >>= align Vec2
  x' <- graphToGLSL ah env x
  sequence [ clip r'' x'' | r'' <- r', x'' <- x' ]

graphToGLSL ah env (Between r x) = do
  r' <- graphToGLSL (Just Vec2) env r >>= align Vec2
  x' <- graphToGLSL ah env x
  return [ between r'' x'' | r'' <- r', x'' <- x' ]

-- *** TODO: Step's semantics currently only make sense for single-channel outputs.
graphToGLSL _ _ (Step [] _) = return [constantFloat 0]
graphToGLSL ah env (Step (x:[]) _) = graphToGLSL ah env x
graphToGLSL ah env (Step xs (Constant y)) =
  let y' = max (min y 0.99999999) 0
      y'' = floor (y' * fromIntegral (length xs))
  in graphToGLSL ah env (xs!!y'')
graphToGLSL _ env (Step xs y) = do
  xs' <- mapM (graphToGLSL Nothing env) xs -- :: [[GLSLExpr]]
  xs'' <- mapM (align GLFloat) xs' >>= return . concat -- :: [GLSLExpr] where all are GLFloat
  y' <- graphToGLSL (Just GLFloat) env y >>= align GLFloat -- :: [GLSLExpr] where all are GLFloat
  return $ fmap (step xs'') y'

-- binary functions, with position

graphToGLSL ah env (Rect xy wh) = do
  xy' <- graphToGLSL (Just Vec2) env xy >>= align Vec2
  wh' <- graphToGLSL (Just Vec2) env wh >>= align Vec2
  binaryFunctionWithPositionGraphM rect env xy' wh' >>= alignHint ah

graphToGLSL ah env (Circle xy r) = do
  xy' <- graphToGLSL (Just Vec2) env xy >>= align Vec2
  r' <- graphToGLSL (Just GLFloat) env r >>= align GLFloat
  binaryFunctionWithPositionGraph circle env xy' r' >>= alignHint ah

graphToGLSL ah env (VLine x w) = do
  x' <- graphToGLSL (Just GLFloat) env x >>= align GLFloat
  w' <- graphToGLSL (Just GLFloat) env w >>= align GLFloat
  binaryFunctionWithPositionGraph vline env x' w' >>= alignHint ah

graphToGLSL ah env (HLine y w) = do
  y' <- graphToGLSL (Just GLFloat) env y >>= align GLFloat
  w' <- graphToGLSL (Just GLFloat) env w >>= align GLFloat
  binaryFunctionWithPositionGraph hline env y' w' >>= alignHint ah

-- (simple) ternary functions
graphToGLSL ah env (LinLin r1 r2 w) = do
  r1' <- graphToGLSL (Just Vec2) env r1 >>= align Vec2
  r2' <- graphToGLSL (Just Vec2) env r2 >>= align Vec2
  w' <- graphToGLSL (Just GLFloat) env w >>= align GLFloat
  alignHint ah [ linlin r1'' r2'' w'' | r1'' <- r1', r2'' <- r2', w'' <- w' ]

-- get all channels of a result branch per channel of condition, by
-- aligning result branches to each other + aligning condition to GLFloat
graphToGLSL ah env (IfThenElse x y z) = do
  x' <- graphToGLSL ah env x >>= align GLFloat
  y' <- graphToGLSL ah env y
  z' <- graphToGLSL ah env z
  (y'',z'') <- alignExprs y' z'
  sequence [ ifThenElse a b c | a <- x', (b,c) <- zip y'' z'']

-- ternary functions with position

graphToGLSL ah env@(_,fxy) (ILine xy1 xy2 w) = do
  xy1' <- graphToGLSL (Just Vec2) env xy1 >>= align Vec2
  xy2' <- graphToGLSL (Just Vec2) env xy2 >>= align Vec2
  w' <- graphToGLSL (Just GLFloat) env w >>= align GLFloat
  alignHint ah [ iline xy1'' xy2'' w'' fxy' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w', fxy' <- fxy ]

graphToGLSL ah env@(_,fxy) (Line xy1 xy2 w) = do
  xy1' <- graphToGLSL (Just Vec2) env xy1 >>= align Vec2
  xy2' <- graphToGLSL (Just Vec2) env xy2 >>= align Vec2
  w' <- graphToGLSL (Just GLFloat) env w >>= align GLFloat
  alignHint ah [ line xy1'' xy2'' w'' fxy' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w', fxy' <- fxy ]

graphToGLSL _ _ _ = return [constantFloat 0]

multiToGLSL :: AlignHint -> GraphEnv -> [Graph] -> GLSL [GLSLExpr]
multiToGLSL _ _ [] = return [constantFloat 0.0]
multiToGLSL ah env xs = do
  xs' <- mapM (graphToGLSL ah env) xs
  xs'' <- mapM (align GLFloat) xs'
  alignHint ah $ concat $ multi xs''


unaryFunctionWithPosition :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> AlignHint -> GraphEnv -> Graph -> GLSL [GLSLExpr]
unaryFunctionWithPosition f ah env@(_,fxy) x = do
  xs <- graphToGLSL (Just Vec2) env x >>= align Vec2
  alignHint ah [ f a b | a <- fxy, b <- xs ]

unaryPositionTransform :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> GLSLType -> AlignHint -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
unaryPositionTransform f t ah env@(texMap,fxy) a b = do
  a' <- graphToGLSL (Just t) env a >>= align t
  fxy' <- mapM assign [ f fxy' a'' | fxy' <- fxy, a'' <- a' ]
  graphToGLSL ah (texMap,fxy') b

binaryMatchedGraphs :: MultiMode -> (GLSLExpr -> GLSLExpr -> GLSLExpr) -> AlignHint -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryMatchedGraphs Combinatorial = binaryMatchedGraphsCombinatorial
binaryMatchedGraphs PairWise = binaryMatchedGraphsPairWise

binaryMatchedGraphsPairWise :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> AlignHint -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryMatchedGraphsPairWise f ah env x y = do
  x' <- graphToGLSL ah env x
  y' <- graphToGLSL ah env y
  (x'',y'') <- alignExprsOptimized x' y' -- alignExprsOptimized aligns so that 1-channel signals are GLFloat no matter what
  return $ zipWith f x'' y''

binaryMatchedGraphsCombinatorial :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> AlignHint -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryMatchedGraphsCombinatorial f ah env x y = do
  x' <- graphToGLSL ah env x
  y' <- graphToGLSL ah env y
  binaryMatchedGLSLExprs f ah x' y'

binaryMatchedGLSLExprs :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> AlignHint -> [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
binaryMatchedGLSLExprs f ah xs ys = case (exprsChannels xs == 1 || exprsChannels ys == 1) of
  True -> alignHint ah [ f x y  | x <- xs, y <- ys ]
  False -> do -- if neither input is a single GLFloat, then we need expressions for each channel of both
    xs' <- align GLFloat xs
    ys' <- align GLFloat ys
    alignHint ah [ f x y  | x <- xs', y <- ys' ]


binaryFunctionWithPositionGraph :: (GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr) -> GraphEnv -> [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
binaryFunctionWithPositionGraph f (_,fxys) as bs = return [ f a b c | a <- as, b <- bs, c <- fxys ]

binaryFunctionWithPositionGraphM :: (GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSL GLSLExpr) -> GraphEnv -> [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
binaryFunctionWithPositionGraphM f (_,fxys) as bs = sequence [ f a b c | a <- as, b <- bs, c <- fxys ]

setfx :: GLSLExpr -> GLSLExpr -> GLSLExpr  -- Vec2 -> GLFloat -> Vec2
setfx fxy x = exprExprToVec2 x (swizzleY fxy)

setfy :: GLSLExpr -> GLSLExpr -> GLSLExpr  -- Vec2 -> GLFloat -> Vec2
setfy fxy y = exprExprToVec2 (swizzleX fxy) y

setfxy :: GLSLExpr -> GLSLExpr -> GLSLExpr -- a -> Vec2 -> Vec2
setfxy _ xy = xy

-- both arguments must represent Vec2
zoom :: GLSLExpr -> GLSLExpr -> GLSLExpr
zoom fxy a = fxy / a

-- both arguments must represent Vec2
move :: GLSLExpr -> GLSLExpr -> GLSLExpr
move fxy a = fxy - a

-- both arguments must represent Vec2
tile :: GLSLExpr -> GLSLExpr -> GLSLExpr
tile fxy a = binaryFunction "tile" Vec2 a fxy

-- both arguments must represent Vec2
spin :: GLSLExpr -> GLSLExpr -> GLSLExpr
spin fxy a = binaryFunction "spin" Vec2 a fxy

-- r must be Vec2, x can be any type
clip :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr
clip r x = do
  rx <- assign $ swizzleX r -- *** TODO: note: this assignment should be avoided in cases where r is already a simple variable or a constant
  ry <- assign $ swizzleY r
  return $ GLSLExpr (glslType x) False $ "clamp(" <> builder x <> "," <> builder rx <> "," <> builder ry <> ")"

-- r (the range to test if something is between) is expected to be Vec2, x can be any type
between :: GLSLExpr -> GLSLExpr -> GLSLExpr
between r x = GLSLExpr (glslType x) False $ "between(" <> builder r <> "," <> builder x <> ")"

-- xs is expected to have 2 or more members (and must have at least one), all GLSLExpr :: GLFloat
step :: [GLSLExpr] -> GLSLExpr -> GLSLExpr
step xs y = foldr1 (+) xs''
  where
    nTotal = length xs
    xs' = zip xs [0..]
    xs'' = fmap (\(x,n) -> x * _step nTotal n y) xs'

_step :: Int -> Int -> GLSLExpr -> GLSLExpr
_step nTotal n y = GLSLExpr GLFloat False $ "_step(" <> showb nTotal <> "," <> showb n <> "," <> builder y <> ")"

linlin :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
linlin r1 r2 w = GLSLExpr GLFloat False $ "linlin(" <> builder r1 <> "," <> builder r2 <> "," <> builder w <> ")"

ifThenElse :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSL GLSLExpr
ifThenElse cond a b = do
  condTrue <- assign $ cond `greaterThan` 0
  return $ (condTrue * a) + ((1 - condTrue) * b)

iline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
iline xy1 xy2 w fxy = GLSLExpr GLFloat False $ "iline(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> "," <> builder fxy <> ")"

line :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
line xy1 xy2 w fxy = GLSLExpr GLFloat False $ "line(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> "," <> builder fxy <> ")"

-- arguments must be pre-aligned (same underlying GLSL types)
gate :: GLSLExpr -> GLSLExpr -> GLSLExpr
gate x y = comparisonOperator "<" "lessThan" x y * y

bipolar :: GLSLExpr -> GLSLExpr
bipolar x = x * 2 - 1

unipolar :: GLSLExpr -> GLSLExpr
unipolar x = (x + 1) * constantFloat 0.5

midicps :: GLSLExpr -> GLSLExpr
midicps x = 440 * pow ((x-69)/12) 2

cpsmidi :: GLSLExpr -> GLSLExpr
cpsmidi x = 69 + (12 * log2 (x/440))

dbamp :: GLSLExpr -> GLSLExpr
dbamp x = pow 10 (x/20)

ampdb :: GLSLExpr -> GLSLExpr
ampdb x = 20 * Sound.Punctual.GLSLExpr.log x / Sound.Punctual.GLSLExpr.log 10

-- must be GLFloat GLFloat Vec2
-- currently w parameter is not quite-intuitive - it means something like 1/2 of the width of the line...
vline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vline x w fxy = 1 - (smoothstep 0.0 aa $ abs (swizzleX fxy - x) - w)
  where aa = glslMin (px*1.5) w

-- must be GLFloat GLFloat Vec2
hline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
hline y w fxy = 1 - (smoothstep 0.0 aa $ abs (swizzleY fxy - y) - w)
  where aa = glslMin (py*1.5) w

-- ie. the width of a pixel in [-1,1] geometry
px :: GLSLExpr
px = glFloat "(2./width)"

-- ie. the height of a pixel in [-1,1] geometry
py :: GLSLExpr
py = glFloat "(2./height)"

pxy :: GLSLExpr
pxy = GLSLExpr Vec2 False "(2./vec2(width,height))"

-- must be Vec2 GLFloat Vec2
-- perhaps this should be reworked to use smoothstep separately on x and y axis?
circle :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
circle xy diameter fxy = smoothstep ((px+py)*0.75) 0.0 $ distance xy fxy - (diameter/2)

point :: GLSLExpr -> GLSLExpr -> GLSLExpr
point fxy xy = circle xy ((px+py)*0.5) fxy

rect :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSL GLSLExpr
rect xy wh fxy = do
  let aa = pxy * 1.5
  d <- assign $ 1 - (smoothstep (exprToVec2 0.0) aa $ abs (fxy-xy) - abs (wh * 0.5))
  return $ swizzleX d * swizzleY d

blend :: GLSLExpr -> GLSLExpr -> GLSL GLSLExpr -- all Vec4
blend a b = do
  b' <- assign b
  return $ GLSLExpr Vec4 False $ "mix(" <> builder a <> "," <> builder b' <> "," <> builder b' <> ".a)"

hsvrgb :: GLSLExpr -> GLSLExpr
hsvrgb = unaryFunction "hsvrgb" Vec3

defaultFragmentShader :: Text
defaultFragmentShader = (toText header) <> "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: Builder
header
 = "precision mediump float;\
   \uniform lowp vec2 res;\
   \uniform lowp float width;\
   \uniform lowp float height;\
   \uniform sampler2D _fb;\
   \uniform sampler2D _fft,_ifft;\
   \uniform sampler2D tex0,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12;\
   \uniform float lo,mid,hi,ilo,imid,ihi;\
   \uniform float _defaultAlpha,_cps,_time,_etime,_beat,_ebeat;\
   \vec2 _fxy() { return (gl_FragCoord.xy/res) * 2. - 1.; }\
   \float unitSquare(vec2 fxy) { return float(all(lessThanEqual(abs(fxy),vec2(1.)))); }\
   \vec3 tex(sampler2D n,vec2 fxy) { return texture2D(n,fxy*0.5+0.5).xyz*unitSquare(fxy);}\
   \vec3 fb(float r){\
   \  vec3 x = texture2D(_fb,gl_FragCoord.xy/res).xyz * r;\
   \  return vec3(x.x > 0.1 ? x.x : 0.,x.y > 0.1 ? x.y : 0.,x.z > 0.1 ? x.z : 0.);}\
   \float phasor(float f) { return (_time*f - floor(_time*f));}\
   \float tri(float f) { float p = phasor(f); return p < 0.5 ? p*4.-1. : 1.-((p-0.5)*4.) ;}\
   \float saw(float f) { return phasor(f)*2.-1.;}\
   \float sqr(float f) { float p = phasor(f); return p < 0.5 ? -1. : 1.;}\
   \float prox(vec2 x,vec2 y){return clamp((2.828427-distance(x,y))/2.828427,0.,1.);}\
   \float _step(int n,int x,float y){return float(x==int((y*0.5+0.5)*float(n)));}\
   \float xFadeNew(float t1,float t2){return clamp((_etime-t1)/(t2-t1),0.,1.);}\
   \float xFadeOld(float t1,float t2){return 1.-xFadeNew(t1,t2);}\
   \vec3 hsvrgb(vec3 c) {\
   \  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);\
   \  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);\
   \  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}\
   \vec3 rgbhsv(vec3 c){\
   \  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0); \
   \  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));\
   \  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));\
   \  float d = q.x - min(q.w, q.y);\
   \  float e = 1.0e-10;\
   \  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);}\
   \float iline(vec2 xy1,vec2 xy2,float w,vec2 fxy) {\
   \  fxy -= xy1, xy2 -= xy1;\
   \  float h = dot(fxy,xy2)/dot(xy2,xy2);\
   \  float aa = min(((1.5/width)+(1.5/height))*0.5,w);\
   \  return smoothstep(aa,0.,length(fxy - xy2 * h)-(w*0.5));}\
   \float between(vec2 r,float x) { return (step(r.x,x)*step(x,r.y)) + (step(x,r.x)*step(r.y,x)); }\
   \vec2 between(vec2 r,vec2 x) { return (step(r.x,x)*step(x,vec2(r.y))) + (step(x,vec2(r.x))*step(r.y,x)); }\
   \vec3 between(vec2 r,vec3 x){ return (step(r.x,x)*step(x,vec3(r.y))) + (step(x,vec3(r.x))*step(r.y,x)); }\
   \vec4 between(vec2 r,vec4 x){ return (step(r.x,x)*step(x,vec4(r.y))) + (step(x,vec4(r.x))*step(r.y,x)); }\
   \float line(vec2 xy1,vec2 xy2,float w,vec2 fxy) {\
   \  fxy -= xy1, xy2 -= xy1;\
   \  float h = clamp(dot(fxy,xy2)/dot(xy2,xy2),0.,1.);\
   \  float aa = min(((1.5/width)+(1.5/height))*0.5,w);\
   \  return smoothstep(aa,0.,length(fxy - xy2 * h)-(w*0.5));}\
   \float linlin(vec2 r1, vec2 r2, float x) { return r2.x+((r2.y-r2.x)*(x-r1.x)/(r1.y-r1.x));}\
   \vec2 tile(vec2 ab,vec2 fxy) { return fract(((fxy*0.5)+0.5)*ab)*2.-1.;}\
   \vec2 spin(float a,vec2 fxy) {\
   \ float ct = cos(a*3.1415926538); float st = sin(a*3.1415926538);\
   \ return vec2(fxy.x*ct-fxy.y*st,fxy.y*ct+fxy.x*st);}\n"
   -- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the HSV-RGB conversion algorithms above!

_time :: GLSLExpr
_time = GLSLExpr GLFloat True "_time"


actionToGLSL :: Output -> Map TextureRef Int -> Action -> GLSL GLSLExpr
actionToGLSL oType texMap a = do

  -- 1. convert the action's graph to GLSLExpr-s, aligning according to the output type
  let ah = actionAlignment oType
  bs <- graphToGLSL (Just ah) (texMap,[defaultFxy]) $ graph a
  cs <- align ah bs

  -- 2. if output of action is HSV, then each Vec3 has to be converted to RGB
  ds <- case oType of
    HSV -> return $ fmap hsvrgb cs
    _ -> return cs

  -- 3. blend when multiple expressions are RGBA, sum in all other cases
  e <- case ds of
    [] -> error "'impossible' error in actionToGLSL"
    (x:[]) -> return x
    (x:xs) -> case oType of
      RGBA -> foldM blend x xs >>= assign
      _ -> foldM (\y z -> return $ y+z ) x xs >>= assign

  -- for Red Green or Blue direct outputs convert to a Vec3 with 2 zero fields
  case oType of
    Red -> return $ exprExprExprToVec3 e 0.0 0.0
    Green -> return $ exprExprExprToVec3 0.0 e 0.0
    Blue -> return $ exprExprExprToVec3 0.0 0.0 e
    _ -> return e


defaultFxy :: GLSLExpr
defaultFxy = GLSLExpr Vec2 True "_fxy()"

actionOutputType :: Action -> Output
actionOutputType x = head (outputs x)

actionAlignment :: Output -> GLSLType
actionAlignment RGBA = Vec4
actionAlignment RGB = Vec3
actionAlignment HSV = Vec3
actionAlignment _ = GLFloat


-- *** TODO: prior to GLSL-refactor... addedAction, discontinuedAction, continuingAction
-- all used a GLSL if-then-else as an optimization, we probably should restore this ***

addedAction :: Tempo -> UTCTime -> Map TextureRef Int -> Int -> Action -> GLSL (GLSLExpr,[Output])
addedAction tempo eTime texMap _ newAction = do
  actionExpr <- actionToGLSL (actionOutputType newAction) texMap newAction
  let (t1,t2) = actionToTimes tempo eTime newAction
  r <- assign $ actionExpr * xFadeNew eTime t1 t2
  return (r, outputs newAction)

discontinuedAction :: UTCTime -> Map TextureRef Int -> Int -> Action -> GLSL (GLSLExpr,[Output])
discontinuedAction eTime texMap _ oldAction = do
  actionExpr <- actionToGLSL (actionOutputType oldAction) texMap oldAction
  let (t1,t2) = (eTime,addUTCTime 0.5 eTime) -- 0.5 sec fadeout
  r <- assign $ actionExpr * xFadeOld eTime t1 t2
  return (r, outputs oldAction)

continuingAction :: Tempo -> UTCTime -> Map TextureRef Int -> Int -> Action -> Action -> GLSL (GLSLExpr,[Output])
continuingAction tempo eTime texMap _ newAction oldAction = do
  let oType = actionOutputType newAction
  oldExpr <- actionToGLSL oType texMap oldAction
  newExpr <- actionToGLSL oType texMap newAction
  let (t1,t2) = actionToTimes tempo eTime newAction
  let oldExpr' = oldExpr * xFadeOld eTime t1 t2
  let newExpr' = newExpr * xFadeNew eTime t1 t2
  r <- assign $ oldExpr' + newExpr'
  return (r, outputs newAction)

xFadeOld :: UTCTime -> UTCTime -> UTCTime -> GLSLExpr
xFadeOld = xFadeFunction "xFadeOld"

xFadeNew :: UTCTime -> UTCTime -> UTCTime -> GLSLExpr
xFadeNew = xFadeFunction "xFadeNew"

xFadeFunction :: Builder -> UTCTime -> UTCTime -> UTCTime -> GLSLExpr
xFadeFunction funcName eTime t1 t2 = GLSLExpr GLFloat False b
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)
    b = funcName <> "(" <> t1' <> "," <> t2' <> ")"


-- the resulting GLSLExpr is what should be assigned to gl_FragColor
fragmentShaderGLSL :: Tempo -> Map TextureRef Int -> Program -> Program -> GLSL GLSLExpr
fragmentShaderGLSL tempo texMap oldProgram newProgram = do
  let eTime = evalTime newProgram

  -- generate maps of previous, current and all relevant expressions
  let oldActions = IntMap.filter actionOutputsWebGL $ actions oldProgram
  let newActions = IntMap.filter actionOutputsWebGL $ actions newProgram

  -- generate a GLSLExpr for all actions, with crossfades
  continuingExprs <- sequence $ IntMap.intersectionWithKey (continuingAction tempo eTime texMap) newActions oldActions
  discontinuedExprs <- sequence $ IntMap.mapWithKey (discontinuedAction eTime texMap) $ IntMap.difference oldActions newActions
  newExprs <- sequence $ IntMap.mapWithKey (addedAction tempo eTime texMap) $ IntMap.difference newActions oldActions
  let allExprs = IntMap.elems $ continuingExprs <> discontinuedExprs <> newExprs -- :: GLSL [ (GLSLExpr,[Output]) ]

  -- generate GLSL shader code that maps the sources to outputs
  red <- generateOutput Red 0 allExprs
  green <- generateOutput Green 0 allExprs
  blue <- generateOutput Blue 0 allExprs
  let _defaultAlpha = GLSLExpr GLFloat True "_defaultAlpha"
  alpha <- generateOutput Alpha _defaultAlpha allExprs
  fdbk <- generateOutput Fdbk 0 allExprs
  let fdbk' = GLSLExpr Vec3 False $"fb(" <> builder fdbk <> ")"
  hsv <- generateOutput HSV (exprToVec3 0) allExprs
  rgb <- generateOutput RGB (exprToVec3 0) allExprs
  rgba <- generateRGBA allExprs
  let rgb' = exprExprToVec4 (red + green + blue + hsv + rgb + fdbk') alpha
  blend rgb' rgba -- NOTE: it would make more sense to convert all outputs to RGBA then blend all...

  -- TODO: red green and blue are always vec3s with data in the appropriate channel
  -- THEN ALSO: is it doing something weird to alpha when we have red/green/blue/rgb/hsv but no rgba
  -- (as will be common for everyone except me?)

generateOutput :: Output -> GLSLExpr -> [(GLSLExpr,[Output])] -> GLSL GLSLExpr
generateOutput o zeroExpr allExprs = do
  let xs = fmap fst $ Prelude.filter (elem o . snd) allExprs
  case xs of
    [] -> return zeroExpr
    (x:[]) -> return x
    _ -> assign $ Foldable.foldr1 (+) xs

generateRGBA :: [(GLSLExpr,[Output])] -> GLSL GLSLExpr
generateRGBA xs = do
  let xs' = fmap fst $ Prelude.filter (elem RGBA . snd) xs
  case xs' of
    [] -> return $ GLSLExpr Vec4 False "vec4(0.)"
    (x:[]) -> return x
    _ -> foldM blend (head xs') (tail xs') >>= assign

fragmentShader :: Tempo -> Map TextureRef Int -> Program -> Program -> Text
fragmentShader _ _ _ newProgram | isJust (directGLSL newProgram) = toText header <> fromJust (directGLSL newProgram)
fragmentShader tempo texMap oldProgram newProgram = toText $ header <> body
  where
    (gl_FragColor,assignments) = runGLSL $ fragmentShaderGLSL tempo texMap oldProgram newProgram
    gl_FragColor' = "gl_FragColor = " <> builder gl_FragColor <> ";\n"
    body = "\nvoid main() {\n" <> assignments <> gl_FragColor' <> "}"
