{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Data.IntMap.Strict as IntMap
import Data.Text (Text)
import Data.Semigroup ((<>))
import TextShow
import Data.Foldable
import Data.Maybe

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((<>),(>>))
import Sound.Punctual.Program

data GLSLType = GLFloat | Vec2 | Vec3

type GLSL = [(Builder,GLSLType)]

glslToFloatBuilder :: Double -> GLSL -> Builder
glslToFloatBuilder def xs = interspersePluses (showb def) $ fmap fst $ toGLFloats xs

glslToVec3Builder :: Double -> GLSL -> Builder
glslToVec3Builder def xs = interspersePluses ("vec3(" <> showb def <> ")") $ fmap fst $ toVec3s xs

interspersePluses :: Foldable t => Builder -> t Builder -> Builder
interspersePluses zero xs = if Data.Foldable.null xs then zero else Data.Foldable.foldr1 (\a b -> a <> "+" <> b) xs

toGLFloat :: GLSL -> GLSL
toGLFloat [] = []
toGLFloat xs = [(glslToFloatBuilder 0 xs,GLFloat)]

toGLFloats :: GLSL -> GLSL
toGLFloats [] = []
toGLFloats ((x,GLFloat):xs) = (x,GLFloat):(toGLFloats xs)
toGLFloats ((x,Vec2):xs) = (x<>".x",GLFloat):(x<>".y",GLFloat):(toGLFloats xs)
toGLFloats ((x,Vec3):xs) = (x<>".x",GLFloat):(x<>".y",GLFloat):(x<>".z",GLFloat):(toGLFloats xs)

-- *** TODO: there are some non-matched patterns here that should be caught
toVec2s :: GLSL -> GLSL
toVec2s [] = []
toVec2s ((x,Vec2):xs) = (x,Vec2):(toVec2s xs)
toVec2s ((x,GLFloat):(y,GLFloat):xs) = ("vec2("<>x<>","<>y<>")",Vec2):(toVec2s xs)
toVec2s ((x,Vec3):(y,GLFloat):xs) =  ("vec2("<>x<>".x,"<>x<>".y)",Vec2):("vec2("<>x<>".z,"<>y<>")",Vec2):(toVec2s xs)
toVec2s ((x,GLFloat):(y,Vec3):xs) = ("vec2("<>x<>","<>y<>".x)",Vec2):("vec2("<>y<>".y,"<>y<>".z)",Vec2):(toVec2s xs)
toVec2s ((x,GLFloat):[]) = [("vec2("<>x<>","<>x<>")",Vec2)]
toVec2s ((x,Vec3):[]) = [("vec2("<>x<>".x,"<>x<>".y)",Vec2),("vec2("<>x<>".z,"<>x<>".z)",Vec2)]

-- *** TODO: there are some non-matched patterns here that should be caught
toVec3s :: GLSL -> GLSL
toVec3s [] = []
toVec3s ((x,Vec3):xs) = (x,Vec3):(toVec3s xs)
toVec3s ((x,GLFloat):(y,GLFloat):(z,GLFloat):xs) = ("vec3("<>x<>","<>y<>","<>z<>")",Vec3):(toVec3s xs)
toVec3s ((x,GLFloat):(y,Vec2):xs) = ("vec3("<>x<>","<>y<>")",Vec3):(toVec3s xs)
toVec3s ((x,Vec2):(y,GLFloat):xs) = ("vec3("<>x<>","<>y<>")",Vec3):(toVec3s xs)
toVec3s ((x,GLFloat):[]) = [("vec3("<>x<>")",Vec3)]
toVec3s ((x,GLFloat):(y,GLFloat):[]) = [("vec3("<>x<>","<>y<>","<>y<>")",Vec3)]
toVec3s ((x,Vec2):[]) = [("vec3("<>x<>","<>x<>".y)",Vec3)]


graphToGLSL :: Graph -> GLSL

-- basics: multi, mono, constants, uniforms
graphToGLSL (Multi xs) = concat $ fmap graphToGLSL xs
graphToGLSL (Mono x) = toGLFloat $ graphToGLSL x
graphToGLSL (Constant x) = [(showb x,GLFloat)]
graphToGLSL Fx = [("fx()",GLFloat)]
graphToGLSL Fy = [("fy()",GLFloat)]
graphToGLSL Fxy = [("fxy()",Vec2)]
graphToGLSL Px = [("10./1920.",GLFloat)]
graphToGLSL Py = [("10./1080.",GLFloat)]
graphToGLSL Lo = [("lo",GLFloat)]
graphToGLSL Mid = [("mid",GLFloat)]
graphToGLSL Hi = [("hi",GLFloat)]
graphToGLSL ILo = [("ilo",GLFloat)]
graphToGLSL IMid = [("imid",GLFloat)]
graphToGLSL IHi = [("ihi",GLFloat)]

-- unary functions
graphToGLSL (Bipolar x) = unaryShaderFunction "bipolar" x
graphToGLSL (Unipolar x) = unaryShaderFunction "unipolar" x
graphToGLSL (Sin x) = unaryShaderFunction "sin_" x
graphToGLSL (Tri x) = fmap (\(b,_) -> ("tri(" <> b <> ")",GLFloat)) $ toGLFloats $ graphToGLSL x
graphToGLSL (Saw x) = fmap (\(b,_) -> ("saw(" <> b <> ")",GLFloat)) $ toGLFloats $ graphToGLSL x
graphToGLSL (Sqr x) = fmap (\(b,_) -> ("sqr(" <> b <> ")",GLFloat)) $ toGLFloats $ graphToGLSL x
graphToGLSL (MidiCps x) = unaryShaderFunction "midicps" x
graphToGLSL (CpsMidi x) = unaryShaderFunction "cpsmidi" x
graphToGLSL (DbAmp x) = unaryShaderFunction "dbamp" x
graphToGLSL (AmpDb x) = unaryShaderFunction "ampdb" x
graphToGLSL (Abs x) = unaryShaderFunction "abs" x
graphToGLSL (Sqrt x) = unaryShaderFunction "sqrt" x
graphToGLSL (Floor x) = unaryShaderFunction "floor" x
graphToGLSL (Fract x) = unaryShaderFunction "fract" x
graphToGLSL (HsvRgb x) = fmap (\(b,_) -> ("hsvrgb("<>b<>")",Vec3))  $ toVec3s $ graphToGLSL x
graphToGLSL (RgbHsv x) = fmap (\(b,_) -> ("rgbhsv("<>b<>")",Vec3))  $ toVec3s $ graphToGLSL x
graphToGLSL (Tex n xy) = fmap (\(b,_) -> ("tex(" <> showb n <> "," <> b <> ")",Vec3)) $ toVec2s $ graphToGLSL xy
graphToGLSL (Point xy) = fmap (\(b,_) -> ("point(" <> b <> ")",GLFloat)) $ toVec2s $ graphToGLSL xy
graphToGLSL (Distance xy) = fmap (\(b,_) -> ("distance(" <> b <> ",fxy())",GLFloat)) $ toVec2s $ graphToGLSL xy

-- binary functions
graphToGLSL (Sum x y) = binaryShaderOp "+" x y
graphToGLSL (Max x y) = binaryShaderFunction "max" x y
graphToGLSL (Min x y) = binaryShaderFunction "min" x y
graphToGLSL (Product x y) = binaryShaderOp "*" x y
graphToGLSL (Division x y) = binaryShaderOp "/" x y
graphToGLSL (GreaterThan x y) = binaryShaderOpBool ">" x y
graphToGLSL (GreaterThanOrEqual x y) = binaryShaderOpBool ">=" x y
graphToGLSL (LessThan x y) = binaryShaderOpBool "<" x y
graphToGLSL (LessThanOrEqual x y) = binaryShaderOpBool "<=" x y
graphToGLSL (Equal x y) = binaryShaderOpBool "==" x y
graphToGLSL (NotEqual x y) = binaryShaderOpBool "!=" x y
graphToGLSL (Pow x y) = binaryShaderFunction "pow" x y
graphToGLSL (Rect xy wh) = expandWith (\(a,_) (b,_) -> ("rect("<>a<>","<>b<>")",GLFloat)) (toVec2s $ graphToGLSL xy) (toVec2s $ graphToGLSL wh)
graphToGLSL (Circle xy r) = expandWith (\(a,_) (b,_) -> ("circle("<>a<>","<>b<>")",GLFloat)) (toVec2s $ graphToGLSL xy) (toGLFloats $ graphToGLSL r)
graphToGLSL (VLine x w) = expandWith (\(a,_) (b,_) -> ("vline("<>a<>","<>b<>")",GLFloat)) (toGLFloats $ graphToGLSL x) (toGLFloats $ graphToGLSL w)
graphToGLSL (HLine y w) = expandWith (\(a,_) (b,_) -> ("hline("<>a<>","<>b<>")",GLFloat)) (toGLFloats $ graphToGLSL y) (toGLFloats $ graphToGLSL w)
graphToGLSL (Clip r x) = expandWith (\(r',_) (b,t) -> ("clip("<>r'<>","<>b<>")",t)) (toVec2s $ graphToGLSL r) (graphToGLSL x)
graphToGLSL (Between r x) = expandWith (\(r',_) (b,t) -> ("between("<>r'<>","<>b<>")",t)) (toVec2s $ graphToGLSL r) (graphToGLSL x)

-- ternary functions
graphToGLSL (ILine xy1 xy2 w) = ternaryShaderFunction "iline" xy1 xy2 w
graphToGLSL (Line xy1 xy2 w) = ternaryShaderFunction "line" xy1 xy2 w
graphToGLSL (LinLin r1 r2 w) = ternaryShaderFunction "linlin" r1 r2 w
graphToGLSL _ = []

-- note: GLSL functions/ops implemented using unaryShaderFunction must exist in versions specialized for float, vec2, and vec3
unaryShaderFunction :: Builder -> Graph -> GLSL
unaryShaderFunction f x = fmap (\(b,t) -> (f <> "(" <> b <> ")",t)) $ graphToGLSL x

-- TODO: for now this is reducing everything to floats but later we should figure out how to preserve larger types when possible
binaryShaderFunction :: Builder -> Graph -> Graph -> GLSL
binaryShaderFunction f x y = expandWith (\(a,_) (b,_) -> (f<>"("<>a<>","<>b<>")",GLFloat)) (toGLFloats $ graphToGLSL x) (toGLFloats $ graphToGLSL y)

-- TODO: for now this is reducing everything to floats but later we should figure out how to preserve larger types when possible
binaryShaderOp :: Builder -> Graph -> Graph -> GLSL
binaryShaderOp f x y = expandWith (\(a,_) (b,_) -> ("("<>a<>f<>b<>")",GLFloat)) (toGLFloats $ graphToGLSL x) (toGLFloats $ graphToGLSL y)

-- like binaryShaderOp except the function f returns a bool that gets cast to a GLFloat
binaryShaderOpBool :: Builder -> Graph -> Graph -> GLSL
binaryShaderOpBool f x y = expandWith (\(a,_) (b,_) -> ("float("<>a<>f<>b<>")",GLFloat)) (toGLFloats $ graphToGLSL x) (toGLFloats $ graphToGLSL y)

-- note that ternaryShaderFunction is currently specialized for functions of the form: float f(vec2,vec2,float)
ternaryShaderFunction :: Builder -> Graph -> Graph -> Graph -> GLSL
ternaryShaderFunction f x y z = expandWith3 (\(a,_) (b,_) (c,_) -> (f<>"("<>a<>","<>b<>","<>c<>")",GLFloat)) x' y' z'
  where
    x' = toVec2s $ graphToGLSL x
    y' = toVec2s $ graphToGLSL y
    z' = toGLFloats $ graphToGLSL z

expandWith :: (a -> b -> c) -> [a] -> [b] -> [c]
expandWith f xs ys = zipWith f xs' ys'
  where
    n = max (length xs) (length ys)
    xs' = take n $ cycle xs -- *** TODO: not quite right, we mean to extend last element instead
    ys' = take n $ cycle ys

expandWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
expandWith3 f xs ys zs = zipWith3 f xs' ys' zs'
  where
    n = maximum [length xs,length ys,length zs]
    xs' = take n $ cycle xs -- *** TODO: not quite right, we mean to extend last element instead
    ys' = take n $ cycle ys
    zs' = take n $ cycle zs



actionToFloat :: Action -> Builder
actionToFloat = glslToFloatBuilder 0 . graphToGLSL . graph

actionToVec3 :: Action -> Builder
actionToVec3 = glslToVec3Builder 0 . graphToGLSL . graph

defaultFragmentShader :: Text
defaultFragmentShader = (toText header) <> "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: Builder
header
 = "precision mediump float;\
   \uniform float t;\
   \uniform lowp vec2 res;\
   \uniform sampler2D tex0,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex15;\
   \uniform float lo,mid,hi,ilo,imid,ihi;\
   \uniform float _defaultAlpha;\
   \float bipolar(float a) { return a * 2. - 1.; }\
   \vec2 bipolar(vec2 a) { return a * 2. - 1.; }\
   \vec3 bipolar(vec3 a) { return a * 2. - 1.; }\
   \float unipolar(float a) { return (a + 1.) * 0.5; }\
   \vec2 unipolar(vec2 a) { return (a + 1.) * 0.5; }\
   \vec3 unipolar(vec3 a) { return (a + 1.) * 0.5; }\
   \float fx() { return bipolar(gl_FragCoord.x/res.x); }\
   \float fy() { return bipolar(gl_FragCoord.y/res.y); }\
   \vec2 fxy() { return bipolar(gl_FragCoord.xy/res); }\
   \vec2 uv() { return (gl_FragCoord.xy/res); }\
   \vec3 tex(int n,vec2 xy) {\
   \ if(n==0)return texture2D(tex0,unipolar(xy)).xyz; else\
   \ if(n==1)return texture2D(tex1,unipolar(xy)).xyz; else\
   \ if(n==2)return texture2D(tex2,unipolar(xy)).xyz; else\
   \ if(n==3)return texture2D(tex3,unipolar(xy)).xyz; else\
   \ if(n==4)return texture2D(tex4,unipolar(xy)).xyz; else\
   \ if(n==5)return texture2D(tex5,unipolar(xy)).xyz; else\
   \ if(n==6)return texture2D(tex6,unipolar(xy)).xyz; else\
   \ if(n==7)return texture2D(tex7,unipolar(xy)).xyz; else\
   \ if(n==8)return texture2D(tex8,unipolar(xy)).xyz; else\
   \ if(n==9)return texture2D(tex9,unipolar(xy)).xyz; else\
   \ if(n==10)return texture2D(tex10,unipolar(xy)).xyz; else\
   \ if(n==11)return texture2D(tex11,unipolar(xy)).xyz; else\
   \ if(n==12)return texture2D(tex12,unipolar(xy)).xyz; else\
   \ if(n==13)return texture2D(tex13,unipolar(xy)).xyz; else\
   \ if(n==14)return texture2D(tex14,unipolar(xy)).xyz; else\
   \ if(n==15)return texture2D(tex15,unipolar(xy)).xyz; else\
   \ return vec3(0.);}\
   \vec3 fb(float r){\
   \  vec3 x = texture2D(tex0,uv()).xyz * r;\
   \  return vec3(x.x > 0.1 ? x.x : 0.,x.y > 0.1 ? x.y : 0.,x.z > 0.1 ? x.z : 0.);}\
   \float sin_(float f) { return sin(f*3.14159265*2.*t);}\
   \vec2 sin_(vec2 f) { return sin(f*3.14159265*2.*t);}\
   \vec3 sin_(vec3 f) { return sin(f*3.14159265*2.*t);}\
   \float phasor(float f) { return (t*f - floor(t*f));}\
   \float tri(float f) { float p = phasor(f); return p < 0.5 ? p*4.-1. : 1.-((p-0.5)*4.) ;}\
   \float saw(float f) { return phasor(f)*2.-1.;}\
   \float sqr(float f) { float p = phasor(f); return p < 0.5 ? -1. : 1.;}\
   \float midicps(float x) { return 440. * pow(2.,(x-69.)/12.); }\
   \vec2 midicps(vec2 x) { return 440. * pow(vec2(2.),(x-69.)/12.); }\
   \vec3 midicps(vec3 x) { return 440. * pow(vec3(2.),(x-69.)/12.); }\
   \float cpsmidi(float x) { return 69. + (12. * log2(x/440.)); }\
   \vec2 cpsmidi(vec2 x) { return 69. + (12. * log2(x/440.)); }\
   \vec3 cpsmidi(vec3 x) { return 69. + (12. * log2(x/440.)); }\
   \float dbamp(float x) { return pow(10.,x/20.); }\
   \vec2 dbamp(vec2 x) { return pow(vec2(10.),x/20.); }\
   \vec3 dbamp(vec3 x) { return pow(vec3(10.),x/20.); }\
   \float ampdb(float x) { return 20. * log(x) / log(10.); }\
   \vec2 ampdb(vec2 x) { return 20. * log(x) / log(10.); }\
   \vec3 ampdb(vec3 x) { return 20. * log(x) / log(10.); }\
   \float xFadeNew(float t1,float t2) { if (t>t2) return 1.; if (t<t1) return 0.; return ((t-t1)/(t2-t1));}\
   \float xFadeOld(float t1,float t2) { return 1.-xFadeNew(t1,t2);}\
   \vec3 hsvrgb(vec3 c) {\
   \  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);\
   \  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);\
   \  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}\
   \float vline(float x,float w) { if(abs(fx()-x)<w) return 1.; else return 0.;}\
   \float hline(float y,float w) { if(abs(fy()-y)<w) return 1.; else return 0.;}\
   \float iline(vec2 xy1,vec2 xy2,float w) {\
   \  if(xy2.x == xy1.x) return vline(xy1.x,w);\
   \  if(xy2.y == xy1.y) return hline(xy1.y,w);\
   \  float d = abs((xy2.y-xy1.y)*fx()-(xy2.x-xy1.x)*fy()+xy2.x*xy1.y-xy2.y*xy1.x)/sqrt((xy2.x-xy1.x)*(xy2.x-xy1.x)+(xy2.y-xy1.y)*(xy2.y-xy1.y));\
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
   \float line(vec2 xy1,vec2 xy2,float w) {\
   \ float m;\
   \ if(xy1.x == xy2.x) m = between(vec2(xy1.y,xy2.y),fy());\
   \ else m = between(vec2(xy1.x,xy2.x),fx())*between(vec2(xy1.y,xy2.y),fy());\
   \ return m*iline(xy1,xy2,w);}\
   \float linlin(vec2 r1, vec2 r2, float x) { return r2.x+((r2.y-r2.x)*(x-r1.x)/(r1.y-r1.x));}\
   \float rect(vec2 xy,vec2 wh) {\
   \ float x1 = xy.x + (wh.x*-0.5);\
   \ float x2 = xy.x + (wh.x*0.5);\
   \ float y1 = xy.y + (wh.y*-0.5);\
   \ float y2 = xy.y + (wh.y*0.5);\
   \ return between(vec2(x1,x2),fx())*between(vec2(y1,y2),fy());}\
   \float circle(vec2 xy,float r) { if(distance(xy,fxy())<r)return 1.; else return 0.;}\
   \float point(vec2 xy) { return circle(xy,0.002); }"
   -- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the hsv to rgb algorithm above!

isVec3 :: Action -> Bool
isVec3 x = elem RGB (outputs x) || elem HSV (outputs x)

continuingAction :: (AudioTime,Double) -> AudioTime -> Int -> Action -> Action -> Builder
continuingAction tempo eTime i newAction oldAction = line1 <> line2 <> line3 <> line4
  where
    typeText | isVec3 newAction = "vec3"
             | otherwise = "float"
    varName = "_" <> showb i
    line1 = typeText <> " " <> varName <> ";\n"
    (t1,t2) = actionToTimes tempo eTime newAction
    oldText | isVec3 newAction = actionToVec3 oldAction
            | otherwise = actionToFloat oldAction
    newText | isVec3 newAction = actionToVec3 newAction
            | otherwise = actionToFloat newAction
    line2 = "if(t<" <> showb t1 <> ")" <> varName <> "=" <> oldText <> ";\n"
    line3 = "else if(t>" <> showb t2 <> ")" <> varName <> "=" <> newText <> ";\n"
    line4 = "else " <> varName <> "=" <> oldText <> "*" <> xFadeOld t1 t2 <> "+" <> newText <> "*" <> xFadeNew t1 t2 <> ";\n"

discontinuedAction :: (AudioTime,Double) -> AudioTime -> Int -> Action -> Builder
discontinuedAction _ eTime i oldAction = line1 <> line2 <> line3
  where
    varName = "_" <> showb i
    line1 | isVec3 oldAction = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = (eTime,0.5 + eTime) -- 0.5 sec
    oldText | isVec3 oldAction = actionToVec3 oldAction
            | otherwise = actionToFloat oldAction
    line2 = "if(t<" <> showb t1 <> ")" <> varName <> "=" <> oldText <> ";\n"
    line3 = "else if(t<=" <> showb t2 <> ")" <> varName <> "=" <> oldText <> "*" <> xFadeOld t1 t2 <> ";\n"

addedAction :: (AudioTime,Double) -> AudioTime -> Int -> Action -> Builder
addedAction tempo eTime i newAction = line1 <> line2 <> line3
  where
    varName = "_" <> showb i
    line1 | isVec3 newAction = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = actionToTimes tempo eTime newAction
    newText | isVec3 newAction = actionToVec3 newAction
            | otherwise = actionToFloat newAction
    line2 = "if(t>=" <> showb t2 <> ")" <> varName <> "=" <> newText <> ";\n"
    line3 = "else if(t>" <> showb t1 <> ")" <> varName <> "=" <> newText <> "*" <> xFadeNew t1 t2 <> ";\n"

xFadeOld :: AudioTime -> AudioTime -> Builder
xFadeOld t1 t2 = "xFadeOld(" <> showb t1 <> "," <> showb t2 <> ")"

xFadeNew :: AudioTime -> AudioTime -> Builder
xFadeNew t1 t2 = "xFadeNew(" <> showb t1 <> "," <> showb t2 <> ")"

fragmentShader :: (AudioTime,Double) -> Program -> Program -> Text
fragmentShader _ _ newProgram | isJust (directGLSL newProgram) = toText header <> fromJust (directGLSL newProgram)
fragmentShader tempo oldProgram newProgram = toText $ header <> body
  where
    eTime = 0.2 + (evalTime newProgram)
    -- generate maps of previous, current and all relevant expressions
    oldActions = IntMap.filter actionOutputsWebGL $ actions oldProgram
    newActions = IntMap.filter actionOutputsWebGL $ actions newProgram
    allActions = union newActions oldActions
    -- generate GLSL shader code for each action, with crossfades
    continuingSources = fold $ intersectionWithKey (continuingAction tempo eTime) newActions oldActions
    discontinuedSources = fold $ mapWithKey (discontinuedAction tempo eTime) $ difference oldActions newActions
    newSources = fold $ mapWithKey (addedAction tempo eTime) $ difference newActions oldActions
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
  where xs' = mapWithKey (\k _ -> "_" <> showb k) $ IntMap.filter (elem o . outputs) xs
