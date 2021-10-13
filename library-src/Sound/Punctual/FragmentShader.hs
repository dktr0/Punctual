{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Sound.Punctual.FragmentShader where

import Data.IntMap.Strict as IntMap
import Data.Text (Text)
import Data.Text.IO as T
import Data.Semigroup ((<>))
import TextShow
import Data.Map as Map
import Data.Foldable as Foldable
import Data.Maybe
import Data.List.Split
import Data.Time
import Data.Tempo
import Data.Set as Set


import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((<>),(>>))
import Sound.Punctual.Program
import Sound.Punctual.GLSL


type GraphEnv = (Map Text Int, [GLSLExpr]) -- texture map, fxy expressions

testGraphEnv :: GraphEnv
testGraphEnv = (Map.empty,[defaultFxy] )


-- graphToGLSL MUST return a non-empty list, so
-- generally speaking, in cases where there is no meaningful return value,
-- the return value will be a one-item list containing a 0 :: GLFloat

graphToGLSL :: GraphEnv -> Graph -> GLSL [GLSLExpr]

-- constants and uniforms

graphToGLSL _ (Constant x) = return [ constantFloat x ]
graphToGLSL _ Px = return [glFloat "1./res.x"]
graphToGLSL _ Py = return [glFloat "1./res.y"]
graphToGLSL _ Lo = return [glFloat "lo"]
graphToGLSL _ Mid = return [glFloat "mid"]
graphToGLSL _ Hi = return [glFloat "hi"]
graphToGLSL _ ILo = return [glFloat "ilo"]
graphToGLSL _ IMid = return [glFloat "imid"]
graphToGLSL _ IHi = return [glFloat "ihi"]
graphToGLSL _ Cps = return [glFloat "_cps"]
graphToGLSL _ Time = return [glFloat "_time"]
graphToGLSL _ Beat = return [glFloat "_beat"]
graphToGLSL _ ETime = return [glFloat "_etime"]
graphToGLSL _ EBeat = return [glFloat "_ebeat"]
graphToGLSL (_,fxy) Fx = return $ fmap swizzleX fxy
graphToGLSL (_,fxy) Fy = return $ fmap swizzleY fxy
graphToGLSL (_,fxy) Fxy = return fxy

-- multichannel operations: multi, mono, rep, unrep

graphToGLSL env (Multi xs) = do
  xs' <- mapM (graphToGLSL env) xs
  return $ concat xs'

graphToGLSL env (Mono x) = do
  x' <- graphToGLSL env x
  case x' of
    [] -> return [constantFloat 0]
    _ -> do
      xs <- align GLFloat x'
      return [ Foldable.foldr1 (+) xs ]

graphToGLSL env (Rep n x) = (mapM (graphToGLSL env) $ replicate n x) >>= return . concat

graphToGLSL _ (UnRep 0 _) = return [constantFloat 0]
graphToGLSL env (UnRep n x) = do
  x' <- graphToGLSL env x
  x'' <- align GLFloat x'
  return $ fmap (Foldable.foldr1 (+)) $ chunksOf n x''

-- unary functions
graphToGLSL env (Bipolar x) = graphToGLSL env x >>= unaryFunction' bipolar
graphToGLSL env (Unipolar x) = graphToGLSL env x >>= unaryFunction' unipolar
graphToGLSL env (Sin x) = do
  x' <- graphToGLSL env x >>= alignMax
  unaryFunction "sin" $ fmap ((*) (constantFloat 3.14159265 * constantFloat 2 * _time)) x'
graphToGLSL env (MidiCps x) = graphToGLSL env x >>= unaryFunction "midicps"
graphToGLSL env (CpsMidi x) = graphToGLSL env x >>= unaryFunction "cpsmidi"
graphToGLSL env (DbAmp x) = graphToGLSL env x >>= unaryFunction "dbamp"
graphToGLSL env (AmpDb x) = graphToGLSL env x >>= unaryFunction "ampdb"
graphToGLSL env (Abs x) = graphToGLSL env x >>= unaryFunction "abs"
graphToGLSL env (Sqrt x) = graphToGLSL env x >>= unaryFunction "sqrt"
graphToGLSL env (Floor x) = graphToGLSL env x >>= unaryFunction "floor"
graphToGLSL env (Ceil x) = graphToGLSL env x >>= unaryFunction "ceil"
graphToGLSL env (Fract x) = graphToGLSL env x >>= unaryFunction "fract"
graphToGLSL env (Tri x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "tri"
graphToGLSL env (Saw x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "saw"
graphToGLSL env (Sqr x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "sqr"
graphToGLSL env (LFTri x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "tri"
graphToGLSL env (LFSaw x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "saw"
graphToGLSL env (LFSqr x) = graphToGLSL env x >>= align GLFloat >>= unaryFunction "sqr"
graphToGLSL env (HsvRgb x) = graphToGLSL env x >>= align Vec3 >>= unaryFunction "hsvrgb"
graphToGLSL env (RgbHsv x) = graphToGLSL env x >>= align Vec3 >>= unaryFunction "rgbhsv"
graphToGLSL env (HsvH x) = graphToGLSL env x >>= align Vec3 >>= return . fmap swizzleX
graphToGLSL env (HsvS x) = graphToGLSL env x >>= align Vec3 >>= return . fmap swizzleY
graphToGLSL env (HsvV x) = graphToGLSL env x >>= align Vec3 >>= return . fmap swizzleZ
graphToGLSL env (HsvR x) = graphToGLSL env (HsvRgb x) >>= return . fmap swizzleX
graphToGLSL env (HsvG x) = graphToGLSL env (HsvRgb x) >>= return . fmap swizzleY
graphToGLSL env (HsvB x) = graphToGLSL env (HsvRgb x) >>= return . fmap swizzleZ
graphToGLSL env (RgbR x) = graphToGLSL env (HsvH x)
graphToGLSL env (RgbG x) = graphToGLSL env (HsvS x)
graphToGLSL env (RgbB x) = graphToGLSL env (HsvV x)
graphToGLSL env (RgbH x) = graphToGLSL env (RgbHsv x) >>= return . fmap swizzleX
graphToGLSL env (RgbS x) = graphToGLSL env (RgbHsv x) >>= return . fmap swizzleY
graphToGLSL env (RgbV x) = graphToGLSL env (RgbHsv x) >>= return . fmap swizzleZ

-- unary functions that access textures

graphToGLSL env@(texMap,_) (Tex t xy) = graphToGLSL env xy >>= texture2D (showb n)
  where n = min 14 $ max 0 $ Map.findWithDefault 0 t texMap

graphToGLSL env (Fb xy) = graphToGLSL env xy >>= texture2D "_fb"

graphToGLSL env (FFT x) = do
  a <- graphToGLSL env (Unipolar x) >>= align GLFloat
  let b = zipWith exprExprToVec2 a (repeat 0) -- [GLSLExpr] where each is a Vec2
  c <- texture2D "_fft" b
  return $ fmap swizzleX c

graphToGLSL env (IFFT x) = do
  a <- graphToGLSL env (Unipolar x) >>= align GLFloat
  let b = zipWith exprExprToVec2 a (repeat 0)
  c <- texture2D "_ifft" b
  return $ fmap swizzleX c

-- unary functions that access position
graphToGLSL env (Point xy) = unaryFunctionWithPosition env "point" xy
graphToGLSL env (Distance xy) = unaryFunctionWithPosition env "distance" xy
graphToGLSL env (Prox xy) = unaryFunctionWithPosition env "prox" xy

-- unary transformations of position (technically binary functions, then)
graphToGLSL env (Zoom a b) = unaryPositionTransform zoom env a b
graphToGLSL env (Move a b) = unaryPositionTransform move env a b
graphToGLSL env (Tile a b) = unaryPositionTransform tile env a b
graphToGLSL env@(texMap,fxy) (Spin a b) = do
  a' <- graphToGLSL env a >>= align GLFloat
  graphToGLSL (texMap,[ spin fxy' a'' | fxy' <- fxy, a'' <- a' ]) b

-- (simple) binary functions
graphToGLSL env (Sum x y) = binaryOp "+" env x y
graphToGLSL env (Max x y) = binaryFunction "max" env x y
graphToGLSL env (Min x y) = binaryFunction "min" env x y
graphToGLSL env (Product x y) = binaryOp "*" env x y
graphToGLSL env (Division x y) = binaryOp "/" env x y
graphToGLSL env (GreaterThan x y) = comparisonOpGLSL ">" "greaterThan" env x y
graphToGLSL env (GreaterThanOrEqual x y) = comparisonOpGLSL ">=" "greaterThanEqual" env x y
graphToGLSL env (LessThan x y) = comparisonOpGLSL "<" "lessThan" env x y
graphToGLSL env (LessThanOrEqual x y) = comparisonOpGLSL "<=" "lessThanEqual" env x y
graphToGLSL env (Equal x y) = comparisonOpGLSL "==" "equal" env x y
graphToGLSL env (NotEqual x y) = comparisonOpGLSL "!=" "notEqual" env x y
graphToGLSL env (Gate x y) = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  y''' <- mapM assign y'' -- assign y since it is used twice by gate
  return $ zipWith gate x'' y'''
graphToGLSL env (Pow x y) = binaryFunction "pow" env x y


graphToGLSL env (Clip r x) = do
  r' <- graphToGLSL env r >>= align Vec2
  x' <- graphToGLSL env x
  return [ clip r'' x'' | r'' <- r', x'' <- x' ]

graphToGLSL env (Between r x) = do
  r' <- graphToGLSL env r >>= align Vec2
  x' <- graphToGLSL env x
  return [ between r'' x'' | r'' <- r', x'' <- x' ]

graphToGLSL _ (Step [] _) = return [constantFloat 0]
graphToGLSL env (Step (x:[]) _) = graphToGLSL env x
graphToGLSL env (Step xs (Constant y)) =
  let y' = max (min y 0.99999999) 0
      y'' = floor (y' * fromIntegral (length xs))
  in graphToGLSL env (xs!!y'')
graphToGLSL env (Step xs y) = do
  xs' <- mapM (graphToGLSL env) xs -- :: [[GLSLExpr]]
  xs'' <- mapM (align GLFloat) xs' >>= return . concat -- :: [GLSLExpr] where all are GLFloat
  y' <- graphToGLSL env y >>= align GLFloat -- :: [GLSLExpr] where all are GLFloat
  return $ fmap (step xs'') y'

-- binary functions, with position

graphToGLSL env (Rect xy wh) = do
  xy' <- graphToGLSL env xy >>= align Vec2
  wh' <- graphToGLSL env wh >>= align Vec2
  binaryFunctionWithPosition "rect" env xy' wh'

graphToGLSL env (Circle xy r) = do
  xy' <- graphToGLSL env xy >>= align Vec2
  r' <- graphToGLSL env r >>= align GLFloat
  binaryFunctionWithPosition "circle" env xy' r'

graphToGLSL env (VLine x w) = do
  x' <- graphToGLSL env x >>= align GLFloat
  w' <- graphToGLSL env w >>= align GLFloat
  binaryFunctionWithPosition "vline" env x' w'

graphToGLSL env (HLine y w) = do
  y' <- graphToGLSL env y >>= align GLFloat
  w' <- graphToGLSL env w >>= align GLFloat
  binaryFunctionWithPosition "hline" env y' w'

-- (simple) ternary functions
graphToGLSL env (LinLin r1 r2 w) = do
  r1' <- graphToGLSL env r1 >>= align Vec2
  r2' <- graphToGLSL env r2 >>= align Vec2
  w' <- graphToGLSL env w >>= align GLFloat
  return [ linlin r1'' r2'' w'' | r1'' <- r1', r2'' <- r2', w'' <- w' ]

-- get all channels of a result branch per channel of condition, by
-- aligning result branches to each other + aligning condition to GLFloat
graphToGLSL env (IfThenElse x y z) = do
  x' <- graphToGLSL env x >>= align GLFloat
  y' <- graphToGLSL env y
  z' <- graphToGLSL env z
  (y'',z'') <- alignExprs y' z'
  return [ ifthenelse x'' yz | x'' <- x', yz <- zip y'' z'']

-- ternary functions with position

graphToGLSL env@(_,fxy) (ILine xy1 xy2 w) = do
  xy1' <- graphToGLSL env xy1 >>= align Vec2
  xy2' <- graphToGLSL env xy2 >>= align Vec2
  w' <- graphToGLSL env w >>= align GLFloat
  return [ iline xy1'' xy2'' w'' fxy' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w', fxy' <- fxy ]

graphToGLSL env@(_,fxy) (Line xy1 xy2 w) = do
  xy1' <- graphToGLSL env xy1 >>= align Vec2
  xy2' <- graphToGLSL env xy2 >>= align Vec2
  w' <- graphToGLSL env w >>= align GLFloat
  return [ line xy1'' xy2'' w'' fxy' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w', fxy' <- fxy ]

graphToGLSL _ _ = return [constantFloat 0]


unaryFunction :: Builder -> [GLSLExpr] -> GLSL [GLSLExpr]
unaryFunction funcName x = return $ fmap (unaryExprFunction funcName) x

unaryFunction' :: (GLSLExpr -> GLSLExpr) -> [GLSLExpr] -> GLSL [GLSLExpr]
unaryFunction' f x = return $ fmap f x

unaryFunctionWithPosition :: GraphEnv -> Builder -> Graph -> GLSL [GLSLExpr]
unaryFunctionWithPosition env@(_,fxy) funcName x = do
  xs <- graphToGLSL env x >>= align Vec2
  let f fxy' x' = GLSLExpr {
    glslType = GLFloat,
    builder = funcName <> "(" <> builder x' <> "," <> builder fxy' <> ")",
    deps = Set.union (deps fxy') (deps x')
    }
  return [ f a b | a <- fxy, b <- xs ]

unaryPositionTransform :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
unaryPositionTransform f env@(texMap,fxy) a b = do
  a' <- graphToGLSL env a >>= align Vec2
  graphToGLSL (texMap,[ f fxy' a'' | fxy' <- fxy, a'' <- a' ]) b

binaryFunction :: Builder -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryFunction funcName env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  return $ zipWith (binaryExprFunction funcName) x'' y''

-- like binaryFunction but takes a Haskell representation of a binary function over GLSL expressions
-- instead of the name of such a function, and assigns x and y
binaryFunction' :: (GLSLExpr -> GLSLExpr -> GLSLExpr) -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryFunction' f env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  return $ zipWith f x'' y''

binaryOp :: Builder -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryOp opName env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  return $ zipWith (binaryExprOp opName) x'' y''

-- comparisonOpGLSL and comparisonOp: specialized for comparison operators which are binary operators
-- for float comparisons but binary functions for vec2/3/4 comparisons
comparisonOpGLSL :: Builder -> Builder -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
comparisonOpGLSL opName funcName env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  return $ zipWith (comparisonOp opName funcName) x'' y''

comparisonOp :: Builder -> Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
comparisonOp opName _ (GLSLExpr GLFloat x xDeps) (GLSLExpr GLFloat y yDeps) = GLSLExpr GLFloat b (Set.union xDeps yDeps)
  where b = "float(" <> x <> opName <> y <> ")"
comparisonOp _ funcName (GLSLExpr Vec2 x xDeps) (GLSLExpr Vec2 y yDeps) = GLSLExpr Vec2 b (Set.union xDeps yDeps)
  where b = "vec2(" <> funcName <> "(" <> x <> "," <> y <> "))"
comparisonOp _ funcName (GLSLExpr Vec3 x xDeps) (GLSLExpr Vec3 y yDeps) = GLSLExpr Vec3 b (Set.union xDeps yDeps)
  where b = "vec3(" <> funcName <> "(" <> x <> "," <> y <> "))"
comparisonOp _ funcName (GLSLExpr Vec4 x xDeps) (GLSLExpr Vec4 y yDeps) = GLSLExpr Vec4 b (Set.union xDeps yDeps)
  where b = "vec4(" <> funcName <> "(" <> x <> "," <> y <> "))"
comparisonOp _ _ _ _ = error "uhoh - comparisonOp called with mismatched/misaligned GLSLExpr types"

binaryFunctionWithPosition :: Builder -> GraphEnv -> [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
binaryFunctionWithPosition funcName (_,fxys) as bs = do
  let f a b c = GLSLExpr {
    glslType = GLFloat,
    deps = Set.union (deps a) $ Set.union (deps b) (deps c),
    builder = funcName <> "(" <> builder a <> "," <> builder b <> "," <> builder c <> ")"
    }
  return [ f a b c | a <- as, b <- bs, c <- fxys ]


-- both arguments must represent Vec2
zoom :: GLSLExpr -> GLSLExpr -> GLSLExpr
zoom fxy a = GLSLExpr { glslType = Vec2, builder = b, deps = Set.union (deps fxy) (deps a) }
  where b = "(" <> builder fxy <> "/" <> builder a <> ")"

-- both arguments must represent Vec2
move :: GLSLExpr -> GLSLExpr -> GLSLExpr
move fxy a = GLSLExpr { glslType = Vec2, builder = b, deps = Set.union (deps fxy) (deps a) }
  where b = "(" <> builder a <> "-" <> builder fxy <> ")"

-- both arguments must represent Vec2
tile :: GLSLExpr -> GLSLExpr -> GLSLExpr
tile fxy a = GLSLExpr { glslType = Vec2, builder = b, deps = Set.union (deps fxy) (deps a) }
  where b = "tile(" <> builder a <> "," <> builder fxy <> ")"

-- both arguments must represent Vec2
spin :: GLSLExpr -> GLSLExpr -> GLSLExpr
spin fxy a = GLSLExpr { glslType = Vec2, builder = b, deps = Set.union (deps fxy) (deps a) }
  where b = "spin(" <> builder a <> "," <> builder fxy <> ")"

-- r (the clipping range) is expected to be Vec2, x can be any type
clip :: GLSLExpr -> GLSLExpr -> GLSLExpr
clip r x = GLSLExpr { glslType = glslType x, builder = b, deps = Set.union (deps r) (deps x) }
  where b = "clip(" <> builder r <> "," <> builder x <> ")"

-- r (the range to test if something is between) is expected to be Vec2, x can be any type
between :: GLSLExpr -> GLSLExpr -> GLSLExpr
between r x = GLSLExpr { glslType = glslType x, builder = b, deps = Set.union (deps r) (deps x) }
  where b = "between(" <> builder r <> "," <> builder x <> ")"

-- xs is expected to have 2 or more members (and must have at least one), all GLSLExpr :: GLFloat
step :: [GLSLExpr] -> GLSLExpr -> GLSLExpr
step xs y = foldr1 (+) xs''
  where
    nTotal = length xs
    xs' = zip xs [0..]
    xs'' = fmap (\(x,n) -> x * _step nTotal n y) xs'

_step :: Int -> Int -> GLSLExpr -> GLSLExpr
_step nTotal n y = GLSLExpr { glslType = GLFloat, builder = b, deps = deps y }
  where b = "_step(" <> showb nTotal <> "," <> showb n <> "," <> builder y <> ")"

linlin :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
linlin r1 r2 w = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.union (deps r1) $ Set.union (deps r2) (deps w)}
  where b = "linlin(" <> builder r1 <> "," <> builder r2 <> "," <> builder w <> ")"

ifthenelse :: GLSLExpr -> (GLSLExpr,GLSLExpr) -> GLSLExpr
ifthenelse c (r1,r2) = GLSLExpr { glslType = glslType r1, builder = b, deps = Set.union (deps c) $ Set.union (deps r1) (deps r2) }
  where b = "ifthenelse(" <> builder c <> "," <> builder r1 <> "," <> builder r2 <> ")"

iline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
iline xy1 xy2 w fxy = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.union (deps xy1) $ Set.union (deps xy2) $ Set.union (deps w) (deps fxy) }
  where b = "iline(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> "," <> builder fxy <> ")"

line :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
line xy1 xy2 w fxy = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.union (deps xy1) $ Set.union (deps xy2) $ Set.union (deps w) (deps fxy) }
  where b = "line(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> "," <> builder fxy <> ")"

-- arguments must be pre-aligned (same underlying GLSL types)
gate :: GLSLExpr -> GLSLExpr -> GLSLExpr
gate x y = comparisonOp "<" "lessThan" x y * y

bipolar :: GLSLExpr -> GLSLExpr
bipolar x = x * 2 - 1

unipolar :: GLSLExpr -> GLSLExpr
unipolar x = (x + 1) * constantFloat 0.5


defaultFragmentShader :: Text
defaultFragmentShader = (toText header) <> "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: Builder
header
 = "precision mediump float;\
   \uniform lowp vec2 res;\
   \uniform sampler2D _fb;\
   \uniform sampler2D _fft,_ifft;\
   \uniform sampler2D tex0,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12;\
   \uniform float lo,mid,hi,ilo,imid,ihi;\
   \uniform float _defaultAlpha,_cps,_time,_etime,_beat,_ebeat;\
   \float fx() { return (gl_FragCoord.x/res.x) * 2. - 1.; }\
   \float fy() { return (gl_FragCoord.y/res.y) * 2. - 1.; }\
   \vec2 _fxy() { return (gl_FragCoord.xy/res) * 2. - 1.; }\
   \vec2 uv() { return (gl_FragCoord.xy/res); }\
   \vec3 fb(float r){\
   \  vec3 x = texture2D(_fb,uv()).xyz * r;\
   \  return vec3(x.x > 0.1 ? x.x : 0.,x.y > 0.1 ? x.y : 0.,x.z > 0.1 ? x.z : 0.);}\
   \float phasor(float f) { return (_time*f - floor(_time*f));}\
   \float tri(float f) { float p = phasor(f); return p < 0.5 ? p*4.-1. : 1.-((p-0.5)*4.) ;}\
   \float saw(float f) { return phasor(f)*2.-1.;}\
   \float sqr(float f) { float p = phasor(f); return p < 0.5 ? -1. : 1.;}\
   \float midicps(float x) { return 440. * pow(2.,(x-69.)/12.); }\
   \vec2 midicps(vec2 x) { return 440. * pow(vec2(2.),(x-69.)/12.); }\
   \vec3 midicps(vec3 x) { return 440. * pow(vec3(2.),(x-69.)/12.); }\
   \vec4 midicps(vec4 x) { return 440. * pow(vec4(2.),(x-69.)/12.); }\
   \float cpsmidi(float x) { return 69. + (12. * log2(x/440.)); }\
   \vec2 cpsmidi(vec2 x) { return 69. + (12. * log2(x/440.)); }\
   \vec3 cpsmidi(vec3 x) { return 69. + (12. * log2(x/440.)); }\
   \vec4 cpsmidi(vec4 x) { return 69. + (12. * log2(x/440.)); }\
   \float dbamp(float x) { return pow(10.,x/20.); }\
   \vec2 dbamp(vec2 x) { return pow(vec2(10.),x/20.); }\
   \vec3 dbamp(vec3 x) { return pow(vec3(10.),x/20.); }\
   \vec4 dbamp(vec4 x) { return pow(vec4(10.),x/20.); }\
   \float ampdb(float x) { return 20. * log(x) / log(10.); }\
   \vec2 ampdb(vec2 x) { return 20. * log(x) / log(10.); }\
   \vec3 ampdb(vec3 x) { return 20. * log(x) / log(10.); }\
   \vec4 ampdb(vec4 x) { return 20. * log(x) / log(10.); }\
   \float ifthenelse(float x,float y,float z){return float(x>0.)*y+float(x<=0.)*z;}\
   \vec2 ifthenelse(vec2 x,vec2 y,vec2 z){return vec2(ifthenelse(x.x,y.x,z.x),ifthenelse(x.y,y.y,z.y));}\
   \vec3 ifthenelse(vec3 x,vec3 y,vec3 z){return vec3(ifthenelse(x.x,y.x,z.x),ifthenelse(x.y,y.y,z.y),ifthenelse(x.z,y.z,z.z));}\
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
   \float vline(float x,float w, vec2 fxy) { if(abs(fxy.x-x)<w) return 1.; else return 0.;}\
   \float hline(float y,float w, vec2 fxy) { if(abs(fxy.y-y)<w) return 1.; else return 0.;}\
   \float iline(vec2 xy1,vec2 xy2,float w,vec2 fxy) {\
   \  if(xy2.x == xy1.x) return vline(xy1.x,w,fxy);\
   \  if(xy2.y == xy1.y) return hline(xy1.y,w,fxy);\
   \  float d = abs((xy2.y-xy1.y)*fxy.x-(xy2.x-xy1.x)*fxy.y+xy2.x*xy1.y-xy2.y*xy1.x)/sqrt((xy2.x-xy1.x)*(xy2.x-xy1.x)+(xy2.y-xy1.y)*(xy2.y-xy1.y));\
   \  if(d<w) return 1.; else return 0.;}\
   \float clip(vec2 r,float x){return clamp(x,r.x,r.y);}\
   \vec2 clip(vec2 r,vec2 x){return clamp(x,r.x,r.y);}\
   \vec3 clip(vec2 r,vec3 x){return clamp(x,r.x,r.y);}\
   \vec4 clip(vec2 r,vec4 x){return clamp(x,r.x,r.y);}\
   \float between(vec2 r,float x) {\
   \ if(r.y>=r.x && x>=r.x && x<=r.y) return 1.;\
   \ if(r.x>=r.y && x>=r.y && x<=r.x) return 1.;\
   \ return 0.;}\
   \vec2 between(vec2 r,vec2 x){ return vec2(between(r,x.x),between(r,x.y));}\
   \vec3 between(vec2 r,vec3 x){ return vec3(between(r,x.x),between(r,x.y),between(r,x.z));}\
   \vec4 between(vec2 r,vec4 x){ return vec4(between(r,x.x),between(r,x.y),between(r,x.z),between(r,x.w));}\
   \float line(vec2 xy1,vec2 xy2,float w,vec2 fxy) {\
   \ float m;\
   \ if(xy1.x == xy2.x) m = between(vec2(xy1.y,xy2.y),fxy.y);\
   \ else m = between(vec2(xy1.x,xy2.x),fx())*between(vec2(xy1.y,xy2.y),fxy.y);\
   \ return m*iline(xy1,xy2,w,fxy);}\
   \float linlin(vec2 r1, vec2 r2, float x) { return r2.x+((r2.y-r2.x)*(x-r1.x)/(r1.y-r1.x));}\
   \float rect(vec2 xy,vec2 wh,vec2 fxy) {\
   \ float x1 = xy.x + (wh.x*-0.5);\
   \ float x2 = xy.x + (wh.x*0.5);\
   \ float y1 = xy.y + (wh.y*-0.5);\
   \ float y2 = xy.y + (wh.y*0.5);\
   \ return between(vec2(x1,x2),fxy.x)*between(vec2(y1,y2),fxy.y);}\
   \float circle(vec2 xy,float r,vec2 fxy) { if(distance(xy,fxy)<r)return 1.; else return 0.;}\
   \float point(vec2 xy,vec2 fxy) { return circle(xy,0.002,fxy); }\
   \vec2 tile(vec2 ab,vec2 fxy) { return fract(((fxy*0.5)+0.5)*ab)*2.-1.;}\
   \vec2 spin(float a,vec2 fxy) {\
   \ float ct = cos(a*3.1415926538); float st = sin(a*3.1415926538);\
   \ return vec2(fxy.x*ct-fxy.y*st,fxy.y*ct+fxy.x*st);}"
   -- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the HSV-RGB conversion algorithms above!

_time :: GLSLExpr
_time = GLSLExpr { glslType = GLFloat, builder = "_time", deps = Set.empty }

actionToGLSL :: Map Text Int -> Action -> GLSL GLSLExpr
actionToGLSL texMap a = do
  b <- graphToGLSL (texMap,[defaultFxy]) $ graph a
  let actionType = if isVec3 a then Vec3 else GLFloat
  c <- align actionType b
  let d = foldr1 (+) c
  assign $ if isHsv a then hsvrgb d else d

defaultFxy :: GLSLExpr
defaultFxy = GLSLExpr { glslType = Vec2, builder = "_fxy()", deps = Set.empty }

isVec3 :: Action -> Bool
isVec3 x = elem RGB (outputs x) || elem HSV (outputs x)

isHsv :: Action -> Bool
isHsv x = elem HSV (outputs x)

hsvrgb :: GLSLExpr -> GLSLExpr
hsvrgb = unaryExprFunction "hsvrgb"


-- *** TODO: prior to GLSL-refactor... addedAction, discontinuedAction, continuingAction
-- all used a GLSL if-then-else as an optimization, we probably should restore this ***

addedAction :: Tempo -> UTCTime -> Map Text Int -> Int -> Action -> GLSL (GLSLExpr,[Output])
addedAction tempo eTime texMap _ newAction = do
  actionExpr <- actionToGLSL texMap newAction
  let (t1,t2) = actionToTimes tempo eTime newAction
  r <- assign $ actionExpr * xFadeNew eTime t1 t2
  return (r, outputs newAction)

discontinuedAction :: UTCTime -> Map Text Int -> Int -> Action -> GLSL (GLSLExpr,[Output])
discontinuedAction eTime texMap _ oldAction = do
  actionExpr <- actionToGLSL texMap oldAction
  let (t1,t2) = (eTime,addUTCTime 0.5 eTime) -- 0.5 sec fadeout
  r <- assign $ actionExpr * xFadeOld eTime t1 t2
  return (r, outputs oldAction)

continuingAction :: Tempo -> UTCTime -> Map Text Int -> Int -> Action -> Action -> GLSL (GLSLExpr,[Output])
continuingAction tempo eTime texMap _ newAction oldAction = do
  oldExpr <- actionToGLSL texMap oldAction
  newExpr <- actionToGLSL texMap newAction
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
xFadeFunction funcName eTime t1 t2 = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.empty}
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)
    b = funcName <> "(" <> t1' <> "," <> t2' <> ")"


-- the resulting GLSLExpr is what should be assigned to gl_FragColor
fragmentShaderGLSL :: Tempo -> Map Text Int -> Program -> Program -> GLSL GLSLExpr
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
  let _defaultAlpha = GLSLExpr { glslType = GLFloat, builder = "_defaultAlpha", deps = Set.empty }
  alpha <- generateOutput Alpha _defaultAlpha allExprs
  fdbk <- generateOutput Fdbk 0 allExprs
  let fdbk' = GLSLExpr { glslType = Vec3, builder = "fb(" <> builder fdbk <> ")", deps = deps fdbk }
  hsv <- generateOutput HSV (exprToVec3 0) allExprs
  rgb <- generateOutput RGB (exprToVec3 0) allExprs
  let rgb' = exprExprExprToVec3 red green blue + hsv + rgb + fdbk'
  return $ exprExprToVec4 rgb' alpha

generateOutput :: Output -> GLSLExpr -> [(GLSLExpr,[Output])] -> GLSL GLSLExpr
generateOutput o zeroExpr allExprs = do
  let xs = fmap fst $ Prelude.filter (elem o . snd) allExprs
  case xs of
    [] -> return zeroExpr
    _ -> assign $ Foldable.foldr1 (+) xs


fragmentShader :: Tempo -> Map Text Int -> Program -> Program -> Text
fragmentShader _ _ _ newProgram | isJust (directGLSL newProgram) = toText header <> fromJust (directGLSL newProgram)
fragmentShader tempo texMap oldProgram newProgram = toText $ header <> body
  where
    (gl_FragColor,assignments) = runGLSL $ fragmentShaderGLSL tempo texMap oldProgram newProgram
    assignments' = realizeAssignments assignments
    gl_FragColor' = "gl_FragColor = " <> builder gl_FragColor <> ";\n"
    body = "\nvoid main() {\n" <> assignments' <> gl_FragColor' <> "}"


testFragmentShader :: IO ()
testFragmentShader = do
  now <- getCurrentTime
  let testTempo = Tempo 1.0 now 0
  let testProgram = (emptyProgram now) {
    actions = IntMap.empty
    }
  T.putStrLn $ fragmentShader testTempo Map.empty (emptyProgram now) testProgram
