{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Data.IntMap.Strict as IntMap
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import TextShow
import Data.Foldable
import Data.Maybe

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((<>),(>>))
import Sound.Punctual.Program

graphToFloat :: Graph -> Builder
graphToFloat = graphToFloat' . graphsToMono . expandMultis

graphToVec3 :: Graph -> Builder
graphToVec3 x = "vec3(" <> r <> "," <> g <> "," <> b <> ")"
  where
    x' = cycleVec3 $ fmap graphToFloat' $ expandMultis x -- :: [Text]
    reds = fmap (\(y,_,_) -> y) x'
    greens = fmap (\(_,y,_) -> y) x'
    blues = fmap (\(_,_,y) -> y) x'
    r = interspersePluses "0." reds
    g = interspersePluses "0." greens
    b = interspersePluses "0." blues

cycleVec3 :: [a] -> [(a,a,a)]
cycleVec3 [] = []
cycleVec3 (r:g:b:xs) = (r,g,b):cycleVec3 xs
cycleVec3 (r:g:[]) = [(r,g,r)]
cycleVec3 (r:[]) = [(r,r,r)]

graphToFloat' :: Graph -> Builder
graphToFloat' (Multi _) = error "internal error: graphToFloat' should only be used after multi-channel expansion (can't handle Multi)"
graphToFloat' (Mono _) = error "internal error: graphToFloat' should only be used after multi-channel expansion (can't handle Mono)"
graphToFloat' EmptyGraph = "0."
graphToFloat' (Constant x) = showb x
graphToFloat' Noise = "0." -- placeholder
graphToFloat' Pink = "0." -- placeholder
graphToFloat' Fx = "fx()"
graphToFloat' Fy = "fy()"
graphToFloat' Px = "10./1920."
graphToFloat' Py = "10./1080."
graphToFloat' (TexR n x y) = "tex(" <> showb n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").r"
graphToFloat' (TexG n x y) = "tex(" <> showb n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").g"
graphToFloat' (TexB n x y) = "tex(" <> showb n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").b"
graphToFloat' Lo = "lo"
graphToFloat' Mid = "mid"
graphToFloat' Hi = "hi"
graphToFloat' (Bipolar x) = unaryShaderFunction "bipolar" (graphToFloat' x)
graphToFloat' (Unipolar x) = unaryShaderFunction "unipolar" (graphToFloat' x)
graphToFloat' (Sine x) = unaryShaderFunction "sin_" (graphToFloat' x)
graphToFloat' (Tri x) = unaryShaderFunction "tri" (graphToFloat' x)
graphToFloat' (Saw x) = unaryShaderFunction "saw" (graphToFloat' x)
graphToFloat' (Square x) = unaryShaderFunction "sqr" (graphToFloat' x)
graphToFloat' (LPF i f q) = graphToFloat' i -- placeholder, doesn't filter yet
graphToFloat' (HPF i f q) = graphToFloat' i -- placeholder, doesn't filter yet
graphToFloat' (FromTarget x) = "0." -- placeholder
graphToFloat' (Sum x y) = "(" <> graphToFloat' x <> "+" <> graphToFloat' y <> ")"
graphToFloat' (Mean x y) = "((" <> graphToFloat' x <> "+" <> graphToFloat' y <> ")*0.5)"
graphToFloat' (Max x y) = "max(" <> graphToFloat' x <> "," <> graphToFloat' y <> ")"
graphToFloat' (Min x y) = "min(" <> graphToFloat' x <> "," <> graphToFloat' y <> ")"
graphToFloat' (Product x y) = "(" <> graphToFloat' x <> "*" <> graphToFloat' y <> ")"
graphToFloat' (Division x y) = "(" <> graphToFloat' x <> "/" <> graphToFloat' y <> ")"
graphToFloat' (GreaterThan x y) = "float(" <> graphToFloat' x <> ">" <> graphToFloat' y <> ")"
graphToFloat' (GreaterThanOrEqual x y) = "float(" <> graphToFloat' x <> ">=" <> graphToFloat' y <> ")"
graphToFloat' (LessThan x y) = "float(" <> graphToFloat' x <> "<" <> graphToFloat' y <> ")"
graphToFloat' (LessThanOrEqual x y) = "float(" <> graphToFloat' x <> "<=" <> graphToFloat' y <> ")"
graphToFloat' (Equal x y) = "float(" <> graphToFloat' x <> "==" <> graphToFloat' y <> ")"
graphToFloat' (NotEqual x y) = "float(" <> graphToFloat' x <> "!=" <> graphToFloat' y <> ")"
graphToFloat' (Rect x y w h) = "rect(" <> graphToFloat' x <> "," <> graphToFloat' y <> "," <> graphToFloat' w <> "," <> graphToFloat' h <> ")"
graphToFloat' (Circle x y r) = "circle(" <> graphToFloat' x <> "," <> graphToFloat' y <> "," <> graphToFloat' r <> ")"
graphToFloat' (Point x y) = "point(" <> graphToFloat' x <> "," <> graphToFloat' y <> ")"
graphToFloat' (Distance x y) = "distance(vec2(" <> graphToFloat' x <> "," <> graphToFloat' y <> "),vec2(fx(),fy()))"
graphToFloat' (MidiCps x) = "midicps(" <> graphToFloat' x <> ")"
graphToFloat' (CpsMidi x) = "cpsmidi(" <> graphToFloat' x <> ")"
graphToFloat' (DbAmp x) = "dbamp(" <> graphToFloat' x <> ")"
graphToFloat' (AmpDb x) = "ampdb(" <> graphToFloat' x <> ")"
graphToFloat' (Abs x) = "abs(" <> graphToFloat' x <> ")"
graphToFloat' (Sqrt x) = "sqrt(" <> graphToFloat' x <> ")"
graphToFloat' (Pow x y) = "pow(" <> graphToFloat' x <> "," <> graphToFloat' y <> ")"
graphToFloat' (Floor x) = "floor(" <> graphToFloat' x <> ")"
graphToFloat' (Fract x) = "fract(" <> graphToFloat' x <> ")"
graphToFloat' (Clip x y z) = "clamp(" <> graphToFloat' z <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ")"
graphToFloat' (Between r1 r2 x) = "between(" <> graphToFloat' r1 <> "," <> graphToFloat' r2 <> "," <> graphToFloat' x <> ")"
graphToFloat' (VLine x w) = "vline(" <> graphToFloat' x <> "," <> graphToFloat' w <> ")"
graphToFloat' (HLine x w) = "hline(" <> graphToFloat' x <> "," <> graphToFloat' w <> ")"
graphToFloat' (ILine x1 y1 x2 y2 w) = "iline(" <> graphToFloat' x1 <> "," <> graphToFloat' y1 <> "," <> graphToFloat' x2 <> "," <> graphToFloat' y2 <> "," <> graphToFloat' w <> ")"
graphToFloat' (Line x1 y1 x2 y2 w) = "line(" <> graphToFloat' x1 <> "," <> graphToFloat' y1 <> "," <> graphToFloat' x2 <> "," <> graphToFloat' y2 <> "," <> graphToFloat' w <> ")"
graphToFloat' (LinLin min1 max1 min2 max2 x) = "linlin(" <> graphToFloat' min1 <> "," <> graphToFloat' max1 <> "," <> graphToFloat' min2 <> "," <> graphToFloat' max2 <> "," <> graphToFloat' x <> ")"

unaryShaderFunction :: Builder -> Builder -> Builder
unaryShaderFunction f x = f <> "(" <> x <> ")"

actionToFloat :: Action -> Builder
actionToFloat = graphToFloat . graph

actionToVec3 :: Action -> Builder
actionToVec3 = graphToVec3 . graph

defaultFragmentShader :: Text
defaultFragmentShader = (toText header) <> "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: Builder
header
 = "precision mediump float;\
   \uniform float t;\
   \uniform lowp vec2 res;\
   \uniform sampler2D tex0;\
   \uniform sampler2D tex1;\
   \uniform sampler2D tex2;\
   \uniform sampler2D tex3;\
   \uniform sampler2D tex4;\
   \uniform sampler2D tex5;\
   \uniform sampler2D tex6;\
   \uniform sampler2D tex7;\
   \uniform sampler2D tex8;\
   \uniform sampler2D tex9;\
   \uniform sampler2D tex10;\
   \uniform sampler2D tex11;\
   \uniform sampler2D tex12;\
   \uniform sampler2D tex13;\
   \uniform sampler2D tex14;\
   \uniform sampler2D tex15;\
   \uniform float lo;\
   \uniform float mid;\
   \uniform float hi;\
   \uniform float _defaultAlpha;\
   \float bipolar(float x) { return x * 2. - 1.; }\
   \float unipolar(float x) { return (x + 1.) * 0.5; }\
   \vec4 tex(int n,float x,float y) {\
   \ if(n==0)return texture2D(tex0,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==1)return texture2D(tex1,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==2)return texture2D(tex2,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==3)return texture2D(tex3,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==4)return texture2D(tex4,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==5)return texture2D(tex5,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==6)return texture2D(tex6,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==7)return texture2D(tex7,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==8)return texture2D(tex8,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==9)return texture2D(tex9,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==10)return texture2D(tex10,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==11)return texture2D(tex11,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==12)return texture2D(tex12,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==13)return texture2D(tex13,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==14)return texture2D(tex14,vec2(unipolar(x),unipolar(y))); else\
   \ if(n==15)return texture2D(tex15,vec2(unipolar(x),unipolar(y))); else\
   \ return vec4(0.);}\
   \vec2 uv() { return vec2(gl_FragCoord.x / res.x, gl_FragCoord.y / res.y); }\
   \vec3 fb(float r){\
   \  vec3 x = texture2D(tex0,uv()).xyz * r;\
   \  return vec3(x.x > 0.1 ? x.x : 0.,x.y > 0.1 ? x.y : 0.,x.z > 0.1 ? x.z : 0.);}\
   \float fx() { return bipolar(gl_FragCoord.x / res.x); }\
   \float fy() { return bipolar(gl_FragCoord.y / res.y); }\
   \float sin_(float f) { return sin(f*3.14159265*2.*t);}\
   \float phasor(float f) { return (t*f - floor(t*f));}\
   \float tri(float f) { float p = phasor(f); return p < 0.5 ? p*4.-1. : 1.-((p-0.5)*4.) ;}\
   \float saw(float f) { return phasor(f)*2.-1.;}\
   \float sqr(float f) { float p = phasor(f); return p < 0.5 ? -1. : 1.;}\
   \float midicps(float x) { return 440. * pow(2.,(x-69.)/12.); }\
   \float cpsmidi(float x) { return 69. + (12. * log2(x/440.)); }\
   \float dbamp(float x) { return pow(10.,x/20.); }\
   \float ampdb(float x) { return 20. * log(x) / log(10.); }\
   \float xFadeNew(float t1,float t2) { if (t>t2) return 1.; if (t<t1) return 0.; return ((t-t1)/(t2-t1));}\
   \float xFadeOld(float t1,float t2) { return 1.-xFadeNew(t1,t2);}\
   \vec3 hsv2rgb(vec3 c) {\
   \  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);\
   \  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);\
   \  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);\
   \}\
   \float vline(float x,float w) { if(abs(fx()-x)<w) return 1.; else return 0.;}\
   \float hline(float y,float w) { if(abs(fy()-y)<w) return 1.; else return 0.;}\
   \float iline(float x1,float y1,float x2,float y2,float w) {\
   \  if(x2 == x1) return vline(x1,w);\
   \  if(y2 == y1) return hline(y1,w);\
   \  float d = abs((y2-y1)*fx()-(x2-x1)*fy()+x2*y1-y2*x1)/sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));\
   \  if(d<w) return 1.; else return 0.;\
   \}\
   \float between(float r1,float r2,float x) {\
   \ if(r2>=r1 && x>=r1 && x<=r2) return 1.;\
   \ if(r1>=r2 && x>=r2 && x<=r1) return 1.;\
   \ return 0.;\
   \}\
   \float line(float x1,float y1,float x2,float y2,float w) {\
   \ float m;\
   \ if(x1 == x2) m = between(y1,y2,fy());\
   \ else m = between(x1,x2,fx())*between(y1,y2,fy());\
   \ return m*iline(x1,y1,x2,y2,w);\
   \}\
   \float linlin(float min1,float max1,float min2,float max2,float x) {\
   \ return min2+((max2-min2)*(x-min1)/(max1-min1));\
   \}\
   \float rect(float x,float y,float w,float h) {\
   \ float x1 = x + (w*-0.5);\
   \ float x2 = x + (w*0.5);\
   \ float y1 = y + (h*-0.5);\
   \ float y2 = y + (h*0.5);\
   \ return between(x1,x2,fx())*between(y1,y2,fy());\
   \}\
   \float circle(float x,float y,float r) { if(distance(vec2(x,y),vec2(fx(),fy()))<r)return 1.; else return 0.;}\
   \float point(float x,float y) { return circle(x,y,0.002); }"
   -- thanks to http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl for the hsv2rgb algorithm above!

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
discontinuedAction tempo eTime i oldAction = line1 <> line2 <> line3
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
    body = "void main() {\n" <> allSources <> allOutputs <> "gl_FragColor = vec4(vec3(red,green,blue)+rgb+fb(fdbk)+hsv2rgb(hsv+vec3(hue,saturation,value)),alpha);}"

generateOutput :: Output -> Builder -> Builder -> IntMap Action -> Builder
generateOutput o typeDecl zeroBuilder xs = typeDecl <> "=" <> interspersePluses zeroBuilder xs' <> ";\n"
  where xs' = mapWithKey (\k _ -> "_" <> showb k) $ IntMap.filter (elem o . outputs) xs

interspersePluses :: Foldable t => Builder -> t Builder -> Builder
interspersePluses zero xs = if Data.Foldable.null xs then zero else Data.Foldable.foldr1 (\a b -> a <> "+" <> b) xs
