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
import Data.Set as Set


import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((<>),(>>))
import Sound.Punctual.Program
import Sound.Punctual.GLSL


type GraphEnv = (Map Text Int, [GLSLExpr]) -- texture map, fxy expressions

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
graphToGLSL env (Bipolar x) = graphToGLSL env x >>= unaryFunction "unipolar"
graphToGLSL env (Unipolar x) = graphToGLSL env x >>= unaryFunction "unipolar"
graphToGLSL env (Sin x) = graphToGLSL env x >>= unaryFunction "sin_"
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

graphToGLSL env@(texMap,fxy) (Tex t xy) = graphToGLSL env xy >>= texture2D (showb n)
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
graphToGLSL env (Spin a b) = unaryPositionTransform spin env a b

-- (simple) binary functions
graphToGLSL env (Sum x y) = binaryOp "+" env x y
graphToGLSL env (Max x y) = binaryFunction "max" env x y
graphToGLSL env (Min x y) = binaryFunction "min" env x y
graphToGLSL env (Product x y) = binaryOp "*" env x y
graphToGLSL env (Division x y) = binaryOp "/" env x y
graphToGLSL env (GreaterThan x y) = binaryFunction "_gt" env x y
graphToGLSL env (GreaterThanOrEqual x y) = binaryFunction "_gte" env x y
graphToGLSL env (LessThan x y) = binaryFunction "_lt" env x y
graphToGLSL env (LessThanOrEqual x y) = binaryFunction "_lte" env x y
graphToGLSL env (Equal x y) = binaryOpBool "==" env x y
graphToGLSL env (NotEqual x y) = binaryOpBool "!=" env x y
graphToGLSL env (Gate x y) = binaryFunction "gate" env x y
graphToGLSL env (Pow x y) = binaryFunction "pow" env x y

graphToGLSL env (Clip r x) = do
  r' <- graphToGLSL env r >>= align Vec2
  x' <- graphToGLSL env x
  return [ clip r'' x'' | r'' <- r', x'' <- x' ]

graphToGLSL env (Between r x) = do
  r' <- graphToGLSL env r >>= align Vec2
  x' <- graphToGLSL env x
  return [ between r'' x'' | r'' <- r', x'' <- x' ]

graphToGLSL env (Step [] _) = return [constantFloat 0]
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

graphToGLSL env (ILine xy1 xy2 w) = do
  xy1' <- graphToGLSL env xy1 >>= align Vec2
  xy2' <- graphToGLSL env xy2 >>= align Vec2
  w' <- graphToGLSL env w >>= align GLFloat
  return [ iline xy1'' xy2'' w'' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w' ]

graphToGLSL env (Line xy1 xy2 w) = do
  xy1' <- graphToGLSL env xy1 >>= align Vec2
  xy2' <- graphToGLSL env xy2 >>= align Vec2
  w' <- graphToGLSL env w >>= align GLFloat
  return [ line xy1'' xy2'' w'' | xy1'' <- xy1', xy2'' <- xy2', w'' <- w' ]

graphToGLSL _ _ = return [constantFloat 0]


unaryFunction :: Builder -> [GLSLExpr] -> GLSL [GLSLExpr]
unaryFunction funcName x = return $ fmap (unaryExprFunction funcName) x

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

binaryOp :: Builder -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryOp opName env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  return $ zipWith (binaryExprOp opName) x'' y''

-- like binaryOp except the op named by opName returns a bool/bvec2/bvec3/bvec4 that is cast to GLFloat/Vec2/Vec3/Vec4
binaryOpBool :: Builder -> GraphEnv -> Graph -> Graph -> GLSL [GLSLExpr]
binaryOpBool opName env x y = do
  x' <- graphToGLSL env x
  y' <- graphToGLSL env y
  (x'',y'') <- alignExprs x' y'
  let z = zipWith (binaryExprOp opName) x'' y'' -- note: GLSLExpr types will be (momentarily) wrong at this point
  let f x = unsafeCast (glslType x) x -- ie. explicitly cast a GLSLExpr to its indicated type
  return $ fmap f z

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

iline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
iline xy1 xy2 w = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.union (deps xy1) $ Set.union (deps xy2) (deps w) }
  where b = "iline(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> ")"

line :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
line xy1 xy2 w = GLSLExpr { glslType = GLFloat, builder = b, deps = Set.union (deps xy1) $ Set.union (deps xy2) (deps w) }
  where b = "line(" <> builder xy1 <> "," <> builder xy2 <> "," <> builder w <> ")"


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
   \float bipolar(float a) { return a * 2. - 1.; }\
   \vec2 bipolar(vec2 a) { return a * 2. - 1.; }\
   \vec3 bipolar(vec3 a) { return a * 2. - 1.; }\
   \vec4 bipolar(vec4 a) { return a * 2. - 1.; }\
   \float unipolar(float a) { return (a + 1.) * 0.5; }\
   \vec2 unipolar(vec2 a) { return (a + 1.) * 0.5; }\
   \vec3 unipolar(vec3 a) { return (a + 1.) * 0.5; }\
   \vec4 unipolar(vec4 a) { return (a + 1.) * 0.5; }\
   \float fx() { return bipolar(gl_FragCoord.x/res.x); }\
   \float fy() { return bipolar(gl_FragCoord.y/res.y); }\
   \vec2 _fxy() { return bipolar(gl_FragCoord.xy/res); }\
   \vec2 uv() { return (gl_FragCoord.xy/res); }\
   \vec3 fb(float r){\
   \  vec3 x = texture2D(_fb,uv()).xyz * r;\
   \  return vec3(x.x > 0.1 ? x.x : 0.,x.y > 0.1 ? x.y : 0.,x.z > 0.1 ? x.z : 0.);}\
   \float sin_(float f) { return sin(f*3.14159265*2.*_time);}\
   \vec2 sin_(vec2 f) { return sin(f*3.14159265*2.*_time);}\
   \vec3 sin_(vec3 f) { return sin(f*3.14159265*2.*_time);}\
   \vec4 sin_(vec4 f) { return sin(f*3.14159265*2.*_time);}\
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
   \float _gt(float x,float y){return float(x>y);}\
   \vec2 _gt(vec2 x,vec2 y){return vec2(bvec2(x.x>y.x,x.y>y.y));}\
   \vec3 _gt(vec3 x,vec3 y){return vec3(bvec3(x.x>y.x,x.y>y.y,x.z>y.z));}\
   \float _gte(float x,float y){return float(x>=y);}\
   \vec2 _gte(vec2 x,vec2 y){return vec2(bvec2(x.x>=y.x,x.y>=y.y));}\
   \vec3 _gte(vec3 x,vec3 y){return vec3(bvec3(x.x>=y.x,x.y>=y.y,x.z>=y.z));}\
   \float _lt(float x,float y){return float(x<y);}\
   \vec2 _lt(vec2 x,vec2 y){return vec2(bvec2(x.x<y.x,x.y<y.y));}\
   \vec3 _lt(vec3 x,vec3 y){return vec3(bvec3(x.x<y.x,x.y<y.y,x.z<y.z));}\
   \float _lte(float x,float y){return float(x<=y);}\
   \vec2 _lte(vec2 x,vec2 y){return vec2(bvec2(x.x<=y.x,x.y<=y.y));}\
   \vec3 _lte(vec3 x,vec3 y){return vec3(bvec3(x.x<=y.x,x.y<=y.y,x.z<=y.z));}\
   \float prox(vec2 x,vec2 y){return clamp((2.828427-distance(x,y))/2.828427,0.,1.);}\
   \float gate(float x,float y){return float(abs(x)<abs(y))*y;}\
   \vec2 gate(vec2 x,vec2 y){return vec2(gate(x.x,y.x),gate(x.y,y.y));}\
   \vec3 gate(vec3 x,vec3 y){return vec3(gate(x.x,y.x),gate(x.y,y.y),gate(x.z,y.z));}\
   \float _step(int n,int x,float y){return float(x==int((y*0.5+0.5)*float(n)));}\
   \float xFadeNew(float t1,float t2){return clamp((_etime-t1)/(t2-t1),0.,1.);}\
   \float xFadeOld(float t1,float t2){return 1.-xFadeNew(t1,t2);}\
   \vec3 xFadeNewHsv(float t1,float t2){return vec3(1.,1.,xFadeNew(t1,t2));}\
   \vec3 xFadeOldHsv(float t1,float t2){return vec3(1.,1.,xFadeOld(t1,t2));}\
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

{-
isVec3 :: Action -> Bool
isVec3 x = elem RGB (outputs x) || elem HSV (outputs x)

isHsv :: Action -> Bool
isHsv x = elem HSV (outputs x)

-- ?? still necessary after refactor ??
actionToFloat :: Map Text Int -> Action -> Builder
actionToFloat texMap = glslToFloatBuilder 0 . graphToGLSL (texMap,defaultFxy) . graph

-- ?? still necessary after refactor ??
actionToVec3 :: Map Text Int -> Action -> Builder
actionToVec3 texMap = glslToVec3Builder 0 . graphToGLSL (texMap,defaultFxy) . graph

defaultFxy :: GLSLExpr
defaultFxy = GLSLExpr { glslType = Vec2, builder = "_fxy()", deps = Set.empty }

continuingAction :: Tempo -> UTCTime -> Map Text Int -> Int -> Action -> Action -> Builder
continuingAction tempo eTime texMap i newAction oldAction = line1 <> line2 <> line3 <> line4
  where
    typeText | isVec3 newAction = "vec3"
             | otherwise = "float"
    varName = "_" <> showb i
    line1 = typeText <> " " <> varName <> ";\n"
    (t1,t2) = actionToTimes tempo eTime newAction
    oldText | isVec3 newAction = actionToVec3 texMap oldAction
            | otherwise = actionToFloat texMap oldAction
    newText | isVec3 newAction = actionToVec3 texMap newAction
            | otherwise = actionToFloat texMap newAction
    fromRGBtoHSV = (not $ isHsv oldAction) && isHsv newAction && isVec3 newAction -- convert old output to HSV
    fromHSVtoRGB = isHsv oldAction && (not $ isHsv newAction) && isVec3 newAction -- convert old output to RGB
    oldText' | fromRGBtoHSV = "rgbhsv(" <> oldText <> ")"
             | fromHSVtoRGB = "hsvrgb(" <> oldText <> ")"
             | otherwise = oldText
    xfn = xFadeNew eTime t1 t2
    xfo = xFadeOld eTime t1 t2
    t1' = realToFrac (diffUTCTime t1 eTime) :: Double
    t2' = realToFrac (diffUTCTime t2 eTime) :: Double
    line2 = "if(_etime<" <> showb t1' <> ")" <> varName <> "=" <> oldText' <> ";\n"
    line3 = "else if(_etime>" <> showb t2' <> ")" <> varName <> "=" <> newText <> ";\n"
    line4 = "else " <> varName <> "=(" <> oldText' <> ")*" <> xfo <> "+(" <> newText <> ")*" <> xfn <> ";\n"

discontinuedAction :: UTCTime -> Map Text Int -> Int -> Action -> Builder
discontinuedAction eTime texMap i oldAction = line1 <> line2 <> line3
  where
    varName = "_" <> showb i
    line1 | isVec3 oldAction = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = (eTime,addUTCTime 0.5 eTime) -- 0.5 sec
    oldText | isVec3 oldAction = actionToVec3 texMap oldAction
            | otherwise = actionToFloat texMap oldAction
    xfo | isHsv oldAction = xFadeOldHsv eTime t1 t2
        | otherwise = xFadeOld eTime t1 t2
    t1' = realToFrac (diffUTCTime t1 eTime) :: Double
    t2' = realToFrac (diffUTCTime t2 eTime) :: Double
    line2 = "if(_etime<" <> showb t1' <> ")" <> varName <> "=" <> oldText <> ";\n"
    line3 = "else if(_etime<=" <> showb t2' <> ")" <> varName <> "=(" <> oldText <> ")*" <> xfo <> ";\n"

addedAction :: Tempo -> UTCTime -> Map Text Int -> Int -> Action -> Builder
addedAction tempo eTime texMap i newAction = line1 <> line2 <> line3
  where
    varName = "_" <> showb i
    line1 | isVec3 newAction = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = actionToTimes tempo eTime newAction
    newText | isVec3 newAction = actionToVec3 texMap newAction
            | otherwise = actionToFloat texMap newAction
    xfn | isHsv newAction = xFadeNewHsv eTime t1 t2
        | otherwise = xFadeNew eTime t1 t2
    t1' = realToFrac (diffUTCTime t1 eTime) :: Double
    t2' = realToFrac (diffUTCTime t2 eTime) :: Double
    line2 = "if(_etime>=" <> showb t2' <> ")" <> varName <> "=" <> newText <> ";\n"
    line3 = "else if(_etime>" <> showb t1' <> ")" <> varName <> "=(" <> newText <> ")*" <> xfn <> ";\n"


xFadeOld :: UTCTime -> UTCTime -> UTCTime -> Builder
xFadeOld eTime t1 t2 = "xFadeOld(" <> t1' <> "," <> t2' <> ")"
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)

xFadeNew :: UTCTime -> UTCTime -> UTCTime -> Builder
xFadeNew eTime t1 t2 = "xFadeNew(" <> t1' <> "," <> t2' <> ")"
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)

xFadeOldHsv :: UTCTime -> UTCTime -> UTCTime -> Builder
xFadeOldHsv eTime t1 t2 = "xFadeOldHsv(" <> t1' <> "," <> t2' <> ")"
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)

xFadeNewHsv :: UTCTime -> UTCTime -> UTCTime -> Builder
xFadeNewHsv eTime t1 t2 = "xFadeNewHsv(" <> t1' <> "," <> t2' <> ")"
  where
    t1' = showb $ ((realToFrac $ diffUTCTime t1 eTime) :: Double)
    t2' = showb $ ((realToFrac $ diffUTCTime t2 eTime) :: Double)


-- *** thought experiment: trying to re-write overly simplified version of fragmentShader in GLSL monad
actionsToGLSL :: (AudioTime,Double) -> Map Text Int -> IntMap Action -> GLSL ()
actionsToGLSL tempo texMap p = do
  let xs = actions p


fragmentShader :: (AudioTime,Double) -> Map Text Int -> Program -> Program -> Text
fragmentShader :: Tempo -> Map Text Int -> Program -> Program -> Text
fragmentShader _ _ _ newProgram | isJust (directGLSL newProgram) = toText header <> fromJust (directGLSL newProgram)
fragmentShader tempo texMap oldProgram newProgram = toText $ header <> body
  where
    eTime = evalTime newProgram
    -- generate maps of previous, current and all relevant expressions
    oldActions = IntMap.filter actionOutputsWebGL $ actions oldProgram
    newActions = IntMap.filter actionOutputsWebGL $ actions newProgram
    allActions = IntMap.union newActions oldActions
    -- generate GLSL shader code for each action, with crossfades
    continuingSources = Foldable.fold $ IntMap.intersectionWithKey (continuingAction tempo eTime texMap) newActions oldActions
    discontinuedSources = Foldable.fold $ IntMap.mapWithKey (discontinuedAction eTime texMap) $ IntMap.difference oldActions newActions
    newSources = Foldable.fold $ IntMap.mapWithKey (addedAction tempo eTime texMap) $ IntMap.difference newActions oldActions
    allSources = continuingSources <> discontinuedSources <> newSources
    -- generate GLSL shader code that maps the sources to outputs
    red = generateOutput Red "float red" "0." allActions
    green = generateOutput Green "float green" "0." allActions
    blue = generateOutput Blue "float blue" "0." allActions
    hue = generateOutput Hue "float hue" "0." allActions
    saturation = generateOutput Saturation "float saturation" "0." allActions
    value = generateOutput Value "float value" "0." allActions
    alpha = generateOutput Alpha "float alpha" "_defaultAlpha" allActions
    hsv = generateOutput HSV "vec3 hsv" "vec3(0.,0.,0.)" allActions
    rgb = generateOutput RGB "vec3 rgb" "vec3(0.,0.,0.)" allActions
    fdbk = generateOutput Fdbk "float fdbk" "0." allActions
    allOutputs = red <> green <> blue <> hue <> saturation <> value <> hsv <> rgb <> alpha <> fdbk
    --
    body = "void main() {\n" <> allSources <> allOutputs <> "gl_FragColor = vec4(vec3(red,green,blue)+rgb+fb(fdbk)+hsvrgb(hsv+vec3(hue,saturation,value)),alpha);}"

generateOutput :: Output -> Builder -> Builder -> IntMap Action -> Builder
generateOutput o typeDecl zeroBuilder xs = typeDecl <> "=" <> interspersePluses zeroBuilder xs' <> ";\n"
  where xs' = IntMap.mapWithKey (\k _ -> "_" <> showb k) $ IntMap.filter (elem o . outputs) xs

-}
