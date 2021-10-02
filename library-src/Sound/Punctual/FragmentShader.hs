{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

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

graphToGLSL _ (Constant x) = return $ constantFloat x
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
    [] -> return 0
    _ -> do
      xs <- align GLFloat x'
      return [ Foldable.foldr1 (+) xs ]

graphToGLSL env (Rep n x) = graphToGLSL env $ concat $ replicate n x

graphToGLSL _ (UnRep 0 _) = return 0
graphToGLSL env (UnRep n x) = do
  x' <- graphToGLSL env x
  x'' <- align GLFloat x'
  return $ fmap (Foldable.foldr1 (+)) chunksOf n x''

-- unary functions
graphToGLSL env (Bipolar x) = unaryFunctionToGLSL "bipolar" Nothing x
graphToGLSL env (Unipolar x) = unaryFunctionToGLSL "unipolar" Nothing x
graphToGLSL env (Sin x) = unaryFunctionToGLSL "sin_" Nothing x
graphToGLSL env (MidiCps x) = unaryFunctionToGLSL "midicps" Nothing x
graphToGLSL env (CpsMidi x) = unaryFunctionToGLSL "cpsmidi" Nothing x
graphToGLSL env (DbAmp x) = unaryFunctionToGLSL "dbamp" Nothing x
graphToGLSL env (AmpDb x) = unaryFunctionToGLSL "ampdb" Nothing x
graphToGLSL env (Abs x) = unaryFunctionToGLSL "abs" Nothing x
graphToGLSL env (Sqrt x) = unaryFunctionToGLSL "sqrt" Nothing x
graphToGLSL env (Floor x) = unaryFunctionToGLSL "floor" Nothing x
graphToGLSL env (Ceil x) = unaryFunctionToGLSL "ceil" Nothing x
graphToGLSL env (Fract x) = unaryFunctionToGLSL "fract" Nothing x
graphToGLSL env (Tri x) = unaryFunctionToGLSL "tri" (Just GLFloat) x
graphToGLSL env (Saw x) = unaryFunctionToGLSL "tri" (Just GLFloat) x
graphToGLSL env (Sqr x) = unaryFunctionToGLSL "sqr" (Just GLFloat) x
graphToGLSL env (LFTri x) = unaryFunctionToGLSL "tri" (Just GLFloat) x
graphToGLSL env (LFSaw x) = unaryFunctionToGLSL "saw" (Just GLFloat) x
graphToGLSL env (LFSqr x) = unaryFunctionToGLSL "sqr" (Just GLFloat) x
graphToGLSL env (HsvRgb x) = unaryFunctionToGLSL "hsvrgb" (Just Vec3) x
graphToGLSL env (RgbHsv x) = unaryFunctionToGLSL "rgbhsv" (Just Vec3) x
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
  a <- graphToGLSL env (Unipolar x)
  b <- align GLFloat a
  c <- *** TODO *** ... now for each of x'' turn into a vec2 by pairing with 0 ...
  d <- texture2D "_fft" ...
  return $ swizzleX d

graphToGLSL env (IFFT x) = do
  a <- graphToGLSL env (Unipolar x)
  b <- align GLFloat a
  c <- *** TODO *** ... now for each of x'' turn into a vec2 by pairing with 0 ...
  d <- texture2D "_ifft" ...
  return $ swizzleX d

-- unary functions that also access position 
graphToGLSL env (Point xy) = unaryFunctionWithPosition env "point" xy
graphToGLSL env (Distance xy) = unaryFunctionWithPosition env "distance" xy
graphToGLSL env (Prox xy) = unaryFunctionWithPosition env "prox" xy
fmap (\(b,_) -> ("prox("<>b<>","<>(fst $ fxy!!0)<>")",GLFloat)) $ toVec2s $ graphToGLSL env xy

-- *** WORKING BELOW HERE ***


-- binary functions
graphToGLSL (texMap,fxy) (Zoom a b) = graphToGLSL (texMap,fxy'') b
  where
    a' = toVec2s $ graphToGLSL (texMap,fxy) a  -- :: GLSL = [(Builder,GLSLType)]
    (a'',fxy') = alignGLSL a' fxy
    fxy'' = zipWith (\(c,_) (d,_) -> ("("<>d<>"/"<>c<>")",Vec2)) a'' fxy'
graphToGLSL (texMap,fxy) (Move a b) = graphToGLSL (texMap,fxy'') b
  where
    a' = toVec2s $ graphToGLSL (texMap,fxy) a  -- :: GLSL = [(Builder,GLSLType)]
    (a'',fxy') = alignGLSL a' fxy
    fxy'' = zipWith (\(c,_) (d,_) -> ("("<>c<>"-"<>d<>")",Vec2)) a'' fxy'
graphToGLSL (texMap,fxy) (Tile a b) = graphToGLSL (texMap,fxy'') b
  where
    a' = toVec2s $ graphToGLSL (texMap,fxy) a  -- :: GLSL = [(Builder,GLSLType)]
    (a'',fxy') = alignGLSL a' fxy
    fxy'' = zipWith (\(c,_) (d,_) -> ("tile("<>c<>","<>d<>")",Vec2)) a'' fxy'
graphToGLSL (texMap,fxy) (Spin a b) = graphToGLSL (texMap,fxy') b
  where
    a' = toGLFloats $ graphToGLSL (texMap,fxy) a  -- :: GLSL = [(Builder,GLSLType)]
    -- fxy is assumed to be only Vec2s, so...
    -- ?? what happens when number of a' is different than number of fxy Vec2s though ??
    fxy' = zipWith (\(c,_) (d,_) -> ("spin("<>c<>","<>d<>")",Vec2)) a' fxy

graphToGLSL env (Sum x y) = binaryShaderOp "+" env x y
graphToGLSL env (Max x y) = binaryShaderFunction "max" env x y
graphToGLSL env (Min x y) = binaryShaderFunction "min" env x y
graphToGLSL env (Product x y) = binaryShaderOp "*" env x y
graphToGLSL env (Division x y) = binaryShaderOp "/" env x y
graphToGLSL env (GreaterThan x y) = binaryShaderFunction "_gt" env x y
graphToGLSL env (GreaterThanOrEqual x y) = binaryShaderFunction "_gte" env x y
graphToGLSL env (LessThan x y) = binaryShaderFunction "_lt" env x y
graphToGLSL env (LessThanOrEqual x y) = binaryShaderFunction "_lte" env x y
graphToGLSL env (Equal x y) = binaryShaderOpBool "==" env x y
graphToGLSL env (NotEqual x y) = binaryShaderOpBool "!=" env x y
graphToGLSL env (Gate x y) = binaryShaderFunction "gate" env x y
graphToGLSL env (Pow x y) = binaryShaderFunction "pow" env x y


-- ** TODO: working here, need to figure out how to apply all of the positions represented in fxy, not just the first!
-- ** note that some (but not all) of the graph constructors that use ternaryShaderFunction need to be reworked also, along the same lines

graphToGLSL env@(_,fxy) (Rect xy wh) = expandWith (\(a,_) (b,_) -> ("rect("<>a<>","<>b<>","<>(fst $ fxy!!0)<>")",GLFloat)) (toVec2s $ graphToGLSL env xy) (toVec2s $ graphToGLSL env wh)
graphToGLSL env@(_,fxy) (Circle xy r) = expandWith (\(a,_) (b,_) -> ("circle("<>a<>","<>b<>","<>(fst $ fxy!!0)<>")",GLFloat)) (toVec2s $ graphToGLSL env xy) (toGLFloats $ graphToGLSL env r)
graphToGLSL env@(_,fxy) (VLine x w) = expandWith (\(a,_) (b,_) -> ("vline("<>a<>","<>b<>","<>(fst $ fxy!!0)<>")",GLFloat)) (toGLFloats $ graphToGLSL env x) (toGLFloats $ graphToGLSL env w)
graphToGLSL env@(_,fxy) (HLine y w) = expandWith (\(a,_) (b,_) -> ("hline("<>a<>","<>b<>","<>(fst $ fxy!!0)<>")",GLFloat)) (toGLFloats $ graphToGLSL env y) (toGLFloats $ graphToGLSL env w)

graphToGLSL env (Clip r x) = expandWith (\(r',_) (b,t) -> ("clip("<>r'<>","<>b<>")",t)) (toVec2s $ graphToGLSL env r) (graphToGLSL env x)
graphToGLSL env (Between r x) = expandWith (\(r',_) (b,t) -> ("between("<>r'<>","<>b<>")",t)) (toVec2s $ graphToGLSL env r) (graphToGLSL env x)
graphToGLSL env (Step [] _) = graphToGLSL env (Constant 0)
graphToGLSL env (Step (x:[]) _) = graphToGLSL env x
graphToGLSL env (Step xs (Constant y)) =
  let y' = max (min y 0.99999999) 0
      y'' = floor (y' * fromIntegral (length xs))
  in graphToGLSL env (xs!!y'')
graphToGLSL env (Step xs y) =
  let xs' = fmap (graphToGLSL env) xs -- [GLSL]
  in fmap (\(a,_) -> (stepGLSL xs' a,GLFloat)) $ toGLFloats $ graphToGLSL env y

-- ternary functions
graphToGLSL env (ILine xy1 xy2 w) = ternaryShaderFunction' "iline" env xy1 xy2 w
graphToGLSL env (Line xy1 xy2 w) = ternaryShaderFunction' "line" env xy1 xy2 w
graphToGLSL env (LinLin r1 r2 w) = ternaryShaderFunction "linlin" env r1 r2 w
graphToGLSL env (IfThenElse x y z) = zipWith3 (\(a,t) (b,_) (c,_) -> ("ifthenelse("<>a<>","<>b<>","<>c<>")",t)) x' y' z'
  where (x',y',z') = alignGLSL3 (graphToGLSL env x) (graphToGLSL env y) (graphToGLSL env z)

graphToGLSL _ _ = []


unaryFunctionToGLSL :: Builder -> Maybe GLSLType -> Graph
unaryFunctionToGLSL b alignment x = do
  x' <- graphToGLSL env x
  x'' <- case alignment of
    Nothing -> return x'
    GLFloat -> align GLFloat x'
    Vec2 -> align Vec2 x'
    Vec3 -> align Vec3 x'
    Vec4 -> align Vec4 x'
  return $ fmap (unaryExprFunction b) x''

unaryFunctionWithPosition :: GraphEnv -> Builder -> Graph -> GLSL [GLSLExpr]
unaryFunctionWithPosition env@(_,fxy) b x = do
  xs <- graphToGLSL env x >>= align Vec2
  let f fxy' x' = GLSLExpr {
    glslType = GLFloat,
    builder = b <> "(" <> builder x' <> "," <> builder fxy' <> ")",
    deps = Set.union (deps a) (deps b)
    }
  return $ prism f fxy xs -- for each combination of fxy and x'' apply the function and collect the GLFloat result ...

-- apply a binary function to every possible combination of two lists
prism :: (a -> a -> a) -> [a] -> [a] -> [a]
prism f xs ys = fmap (\(x,y) -> f x y) $ [ (x,y) | x <- xs, y <- ys]



stepGLSL :: [GLSL] -> Builder -> Builder
stepGLSL xs y = "(" <> (interspersePluses "0." $ zipWith f xs' ([0..]::[Int])) <> ")"
  where
    xs' = fmap (glslToFloatBuilder 0) xs
    f x n = x <> "*_step(" <> showb (length xs') <> "," <> showb n <> "," <> y <> ")"

-- note: GLSL functions/ops implemented using unaryShaderFunction must exist in versions specialized for float, vec2, and vec3
unaryShaderFunction :: Builder -> GLSLEnv -> Graph -> GLSL
unaryShaderFunction f env x = fmap (\(b,t) -> (f <> "(" <> b <> ")",t)) $ graphToGLSL env x

binaryShaderFunction :: Builder -> GLSLEnv -> Graph -> Graph -> GLSL
binaryShaderFunction f env x y = zipWith (\(a,t) (b,_) -> (f<>"("<>a<>","<>b<>")",t)) x' y'
  where (x',y') = alignGLSL (graphToGLSL env x) (graphToGLSL env y)

binaryShaderOp :: Builder -> GLSLEnv -> Graph -> Graph -> GLSL
binaryShaderOp f env x y = zipWith (\(a,t) (b,_) -> ("("<>a<>f<>b<>")",t)) x' y'
  where (x',y') = alignGLSL (graphToGLSL env x) (graphToGLSL env y)

-- like binaryShaderOp except the function f returns a bool/bvec2/bvec3 that gets cast to GLFloat/Vec2/Vec3
binaryShaderOpBool :: Builder -> GLSLEnv -> Graph -> Graph -> GLSL
binaryShaderOpBool f env x y = zipWith (\(a,t) (b,_) -> (glslTypeToCast t<>"("<>a<>f<>b<>")",t)) x' y'
  where (x',y') = alignGLSL (graphToGLSL env x) (graphToGLSL env y)

glslTypeToCast :: GLSLType -> Builder
glslTypeToCast GLFloat = "float"
glslTypeToCast Vec2 = "vec2"
glslTypeToCast Vec3 = "vec3"

-- note that ternaryShaderFunction is currently specialized for functions of the form: float f(vec2,vec2,float)
ternaryShaderFunction :: Builder -> GLSLEnv -> Graph -> Graph -> Graph -> GLSL
ternaryShaderFunction f env x y z = expandWith3 (\(a,_) (b,_) (c,_) -> (f<>"("<>a<>","<>b<>","<>c<>")",GLFloat)) x' y' z'
  where
    x' = toVec2s $ graphToGLSL env x
    y' = toVec2s $ graphToGLSL env y
    z' = toGLFloats $ graphToGLSL env z

-- for use with functions that take fxy arguments (line,iline)
ternaryShaderFunction' :: Builder -> GLSLEnv -> Graph -> Graph -> Graph -> GLSL
ternaryShaderFunction' f env@(_,fxy) x y z = expandWith3 (\(a,_) (b,_) (c,_) -> (f<>"("<>a<>","<>b<>","<>c<>","<>(fst $ fxy!!0)<>")",GLFloat)) x' y' z'
  where
    x' = toVec2s $ graphToGLSL env x
    y' = toVec2s $ graphToGLSL env y
    z' = toGLFloats $ graphToGLSL env z

glslChannels :: GLSL -> Int
glslChannels ((_,Vec3):xs) = 3 + glslChannels xs
glslChannels ((_,Vec2):xs) = 2 + glslChannels xs
glslChannels ((_,GLFloat):xs) = 1 + glslChannels xs
glslChannels _ = 0

alignGLSL :: GLSL -> GLSL -> (GLSL,GLSL)
alignGLSL a b = if aIsModel then (a, cycleGLSL a b) else (cycleGLSL b a, b)
  where aIsModel = (glslChannels a > glslChannels b) || ((glslChannels a == glslChannels b) && (length a <= length b))

alignGLSL3 :: GLSL -> GLSL -> GLSL -> (GLSL,GLSL,GLSL)
alignGLSL3 a b c = if aIsModel then (a, cycleGLSL a b, cycleGLSL a c) else (if bIsModel then (cycleGLSL b a, b, cycleGLSL b c) else (cycleGLSL c a,cycleGLSL c b,c))
  where
    aOverB = (glslChannels a > glslChannels b) || ((glslChannels a == glslChannels b) && (length a <= length b))
    aOverC = (glslChannels a > glslChannels c) || ((glslChannels a == glslChannels c) && (length a <= length c))
    bOverC = (glslChannels b > glslChannels c) || ((glslChannels b == glslChannels c) && (length b <= length c))
    aIsModel = aOverB && aOverC
    bIsModel = (not aOverB) && bOverC

-- cycle through the builders in a in a way that matches the model of m
cycleGLSL :: GLSL -> GLSL -> GLSL
cycleGLSL m a = f (fmap snd m) (cycle a)
  where
    f [] _ = []
    f (GLFloat:ms) ((x,GLFloat):xs) = (x,GLFloat):(f ms xs)
    f (GLFloat:ms) ((x,Vec2):xs) = (x <> ".x",GLFloat) : (f ms $ (x <> ".y",GLFloat):xs)
    f (GLFloat:ms) ((x,Vec3):xs) = (x <> ".x",GLFloat) : (f ms $ (x <> ".yz",Vec2):xs)
    f (Vec2:ms) ((x,Vec2):xs) = (x,Vec2):(f ms xs)
    f (Vec2:ms) ((x,Vec3):xs) = (x <> ".xy",Vec2) : (f ms $ (x <> ".z",GLFloat):xs)
    f (Vec2:ms) ((x,GLFloat):(y,GLFloat):xs) = ("vec2(" <> x <> "," <> y <> ")",Vec2) : (f ms xs)
    f (Vec2:ms) ((x,GLFloat):(y,Vec2):xs) = ("vec2(" <> x <> "," <> y <> ".x)",Vec2) : (f ms $ (y <> ".y",GLFloat):xs)
    f (Vec2:ms) ((x,GLFloat):(y,Vec3):xs) = ("vec2(" <> x <> "," <> y <> ".x)",Vec2) : (f ms $ (y <> ".yz",Vec2):xs)
    f (Vec3:ms) ((x,Vec3):xs) = (x,Vec3):(f ms xs)
    f (Vec3:ms) ((x,Vec2):(y,GLFloat):xs) = ("vec3(" <> x <> "," <> y <> ")",Vec3) : (f ms xs)
    f (Vec3:ms) ((x,Vec2):(y,Vec2):xs) = ("vec3(" <> x <> "," <> y <> ".x)",Vec3) : (f ms $ (y <> ".y",GLFloat):xs)
    f (Vec3:ms) ((x,Vec2):(y,Vec3):xs) = ("vec3(" <> x <> "," <> y <> ".x)",Vec3) : (f ms $ (y <> ".yz",Vec2):xs)
    f (Vec3:ms) ((x,GLFloat):(y,GLFloat):(z,GLFloat):xs) = ("vec3(" <> x <> "," <> y <> "," <> z <> ")",Vec3) : (f ms xs)
    f (Vec3:ms) ((x,GLFloat):(y,GLFloat):(z,Vec2):xs) = ("vec3(" <> x <> "," <> y <> "," <> z <> ".x)",Vec3) : (f ms $ (z <> ".y",GLFloat):xs)
    f (Vec3:ms) ((x,GLFloat):(y,GLFloat):(z,Vec3):xs) = ("vec3(" <> x <> "," <> y <> "," <> z <> ".x)",Vec3) : (f ms $ (z <> ".yz",Vec2):xs)
    f (Vec3:ms) ((x,GLFloat):(y,Vec2):xs) = ("vec3(" <> x <> "," <> y <> ")",Vec3) : (f ms xs)
    f (Vec3:ms) ((x,GLFloat):(y,Vec3):xs) = ("vec3(" <> x <> "," <> y <> ".xy)",Vec3) : (f ms $ (y <> ".z",GLFloat):xs)
    f _ _ = error "strange error in alignGLSL"


expandWith :: (a -> b -> c) -> [a] -> [b] -> [c]
expandWith _ [] _ = []
expandWith _ _ [] = []
expandWith f xs ys = zipWith f xs' ys'
  where
    n = max (length xs) (length ys)
    xs' = Prelude.take n $ cycle xs -- *** TODO: not quite right, we mean to extend last element instead
    ys' = Prelude.take n $ cycle ys

expandWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
expandWith3 _ [] _ _ = []
expandWith3 _ _ [] _ = []
expandWith3 _ _ _ [] = []
expandWith3 f xs ys zs = zipWith3 f xs' ys' zs'
  where
    n = maximum [length xs,length ys,length zs]
    xs' = Prelude.take n $ cycle xs -- *** TODO: not quite right, we mean to extend last element instead
    ys' = Prelude.take n $ cycle ys
    zs' = Prelude.take n $ cycle zs


defaultFxy :: GLSL
defaultFxy = [("_fxy()",Vec2)]

actionToFloat :: Map Text Int -> Action -> Builder
actionToFloat texMap = glslToFloatBuilder 0 . graphToGLSL (texMap,defaultFxy) . graph

actionToVec3 :: Map Text Int -> Action -> Builder
actionToVec3 texMap = glslToVec3Builder 0 . graphToGLSL (texMap,defaultFxy) . graph

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
   \float between(vec2 r,float x) {\
   \ if(r.y>=r.x && x>=r.x && x<=r.y) return 1.;\
   \ if(r.x>=r.y && x>=r.y && x<=r.x) return 1.;\
   \ return 0.;}\
   \vec2 between(vec2 r,vec2 x){\
   \ return vec2(between(r,x.x),between(r,x.y));}\
   \vec3 between(vec2 r,vec3 x){\
   \ return vec3(between(r,x.x),between(r,x.y),between(r,x.z));}\
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

isVec3 :: Action -> Bool
isVec3 x = elem RGB (outputs x) || elem HSV (outputs x)

isHsv :: Action -> Bool
isHsv x = elem HSV (outputs x)

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
