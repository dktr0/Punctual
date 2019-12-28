{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Data.Map.Strict
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import TextShow
import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Graph hiding (difference)
import Sound.Punctual.Types hiding ((<>))
import Sound.Punctual.Evaluation

graphToFloat :: Graph -> Text
graphToFloat = graphToFloat' . graphsToMono . expandMultis

graphToVec3 :: Graph -> Text
graphToVec3 x = "vec3(" <> r <> "," <> g <> "," <> b <> ")"
  where
    x' = cycleVec3 $ fmap graphToFloat' $ expandMultis x -- :: [Text]
    reds = fmap (\(y,_,_) -> y) x'
    greens = fmap (\(_,y,_) -> y) x'
    blues = fmap (\(_,_,y) -> y) x'
    r = T.intercalate "+" $ ["0."] <> reds
    g = T.intercalate "+" $ ["0."] <> greens
    b = T.intercalate "+" $ ["0."] <> blues

cycleVec3 :: [a] -> [(a,a,a)]
cycleVec3 [] = []
cycleVec3 (r:g:b:xs) = (r,g,b):cycleVec3 xs
cycleVec3 (r:g:[]) = [(r,g,r)]
cycleVec3 (r:[]) = [(r,r,r)]

graphToFloat' :: Graph -> Text
graphToFloat' (Multi _) = error "internal error: graphToFloat' should only be used after multi-channel expansion (can't handle Multi)"
graphToFloat' (Mono _) = error "internal error: graphToFloat' should only be used after multi-channel expansion (can't handle Mono)"
graphToFloat' EmptyGraph = "0."
graphToFloat' (Constant x) = showt x
graphToFloat' Noise = "0." -- placeholder
graphToFloat' Pink = "0." -- placeholder
graphToFloat' Fx = "fx()"
graphToFloat' Fy = "fy()"
graphToFloat' Px = "10./1920."
graphToFloat' Py = "10./1080."
graphToFloat' (TexR n x y) = "tex(" <> graphToFloat' n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").r"
graphToFloat' (TexG n x y) = "tex(" <> graphToFloat' n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").g"
graphToFloat' (TexB n x y) = "tex(" <> graphToFloat' n <> "," <> graphToFloat' x <> "," <> graphToFloat' y <> ").b"
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

unaryShaderFunction :: Text -> Text -> Text
unaryShaderFunction f x = f <> "(" <> x <> ")"

expressionToFloat :: Expression -> Text
expressionToFloat (Expression (Definition _ _ _ g) _) = graphToFloat g

expressionToVec3 :: Expression -> Text
expressionToVec3 (Expression (Definition _ _ _ g) _) = graphToVec3 g

defaultFragmentShader :: Text
defaultFragmentShader = header <> "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: Text
header
 = "precision mediump float;\
   \uniform float t;\
   \uniform lowp vec2 res;\
   \uniform sampler2D tex0;\
   \uniform sampler2D tex1;\
   \uniform sampler2D tex2;\
   \uniform sampler2D tex3;\
   \uniform float lo;\
   \uniform float mid;\
   \uniform float hi;\
   \float bipolar(float x) { return x * 2. - 1.; }\
   \float unipolar(float x) { return (x + 1.) * 0.5; }\
   \float prox1(float x,float y) { return max(1.-abs(x-y),0.); }\
   \vec4 tex(float n,float x,float y) {\
   \ return\
   \  (texture2D(tex0,vec2(unipolar(x),unipolar(y)))*prox1(n,0.))+\
   \  (texture2D(tex1,vec2(unipolar(x),unipolar(y)))*prox1(n,1.))+\
   \  (texture2D(tex2,vec2(unipolar(x),unipolar(y)))*prox1(n,2.))+\
   \  (texture2D(tex3,vec2(unipolar(x),unipolar(y)))*prox1(n,3.));}\
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

targetToVariableName :: Target' -> Text
targetToVariableName (Named s) = "_named_" <> s;
targetToVariableName (Anon i) = "_anon_" <> (showt i);

isVec3 :: Expression -> Bool
isVec3 x = (output x == NamedOutput "rgb") || (output x == NamedOutput "hsv")

continuingTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> (Target',Expression) -> Text
continuingTarget tempo evalTime (newTarget,newExpr) (target',oldExpr) = line1 <> line2 <> line3 <> line4
  where
    typeText | isVec3 newExpr = "vec3"
             | otherwise = "float"
    varName = targetToVariableName target'
    line1 = typeText <> " " <> varName <> ";\n"
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    oldText | isVec3 newExpr = expressionToVec3 oldExpr
            | otherwise = expressionToFloat oldExpr
    newText | isVec3 newExpr = expressionToVec3 newExpr
            | otherwise = expressionToFloat newExpr
    line2 = "if(t<" <> showt t1 <> ")" <> varName <> "=" <> oldText <> ";\n"
    line3 = "else if(t>" <> showt t2 <> ")" <> varName <> "=" <> newText <> ";\n"
    line4 = "else " <> varName <> "=" <> oldText <> "*" <> xFadeOld t1 t2 <> "+" <> newText <> "*" <> xFadeNew t1 t2 <> ";\n"

discontinuedTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> Text
discontinuedTarget tempo evalTime (target',oldExpr) = line1 <> line2 <> line3
  where
    varName = targetToVariableName target'
    line1 | isVec3 oldExpr = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = (evalTime,0.5 + evalTime) -- 0.5 sec
    oldText | isVec3 oldExpr = expressionToVec3 oldExpr
            | otherwise = expressionToFloat oldExpr
    line2 = "if(t<" <> showt t1 <> ")" <> varName <> "=" <> oldText <> ";\n"
    line3 = "else if(t<=" <> showt t2 <> ")" <> varName <> "=" <> oldText <> "*" <> xFadeOld t1 t2 <> ";\n"

addedTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> Text
addedTarget tempo evalTime (target',newExpr) = line1 <> line2 <> line3
  where
    varName = targetToVariableName target'
    line1 | isVec3 newExpr = "vec3 " <> varName <> "=vec3(0.);\n"
          | otherwise = "float " <> varName <> "=0.;\n"
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    newText | isVec3 newExpr = expressionToVec3 newExpr
            | otherwise = expressionToFloat newExpr
    line2 = "if(t>=" <> showt t2 <> ")" <> varName <> "=" <> newText <> ";\n"
    line3 = "else if(t>" <> showt t1 <> ")" <> varName <> "=" <> newText <> "*" <> xFadeNew t1 t2 <> ";\n"

xFadeOld :: AudioTime -> AudioTime -> Text
xFadeOld t1 t2 = "xFadeOld(" <> showt t1 <> "," <> showt t2 <> ")"

xFadeNew :: AudioTime -> AudioTime -> Text
xFadeNew t1 t2 = "xFadeNew(" <> showt t1 <> "," <> showt t2 <> ")"

fragmentShader :: [Expression] -> (AudioTime,Double) -> Evaluation -> Text
fragmentShader xs0 tempo e@(xs1,t) = header <> "void main() {\n" <> allTargets <> allOutputs <> glFragColor <> "}"
  where
    evalTime = 0.2 + t
    -- generate maps of previous, current and all relevant expressions :: Map Target' (Target',Expression)
    oldExprs = mapWithKey (\k a -> (k,a)) $ listOfExpressionsToMap xs0
    newExprs = mapWithKey (\k a -> (k,a)) $ listOfExpressionsToMap xs1
    allExprs = union newExprs oldExprs
    -- using the maps in oldExprs and newExprs, generate GLSL shader code for each target, with crossfades
    continuing = intersectionWith (continuingTarget tempo evalTime) newExprs oldExprs -- Map Target' Text
    continuing' = T.concat $ elems continuing -- Text
    discontinued = fmap (discontinuedTarget tempo evalTime) $ difference oldExprs newExprs -- Map Target' Text
    discontinued' = T.concat $ elems discontinued -- Text
    added = fmap (addedTarget tempo evalTime) $ difference newExprs oldExprs -- Map Target' Text
    added' = T.concat $ elems added
    allTargets = continuing' <> discontinued' <> added'
    --
    redExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "red") $ elems allExprs
    greenExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "green") $ elems allExprs
    blueExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "blue") $ elems allExprs
    alphaExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "alpha") $ elems allExprs
    rgbExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "rgb") $ elems allExprs
    hsvExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "hsv") $ elems allExprs
    redVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) redExprs
    greenVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) greenExprs
    blueVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) blueExprs
    alphaVars = if length alphaExprs == 0 then "1." else
      T.intercalate "+" $ fmap (targetToVariableName . fst) alphaExprs
    rgbVars = T.intercalate "+" $ (["vec3(0.)"] <>) $ fmap (targetToVariableName . fst) rgbExprs
    hsvVars = T.intercalate "+" $ (["vec3(0.)"] <>) $ fmap (targetToVariableName . fst) hsvExprs
    red = "float red = " <> redVars <> ";\n"
    green = "float green = " <> greenVars <> ";\n"
    blue = "float blue = " <> blueVars <> ";\n"
    alpha = "float alpha = " <> alphaVars <> ";\n"
    hsv = "vec3 hsv = hsv2rgb(" <> hsvVars <> ");\n"
    rgb = "vec3 rgb = " <> rgbVars <> "+vec3(red,green,blue)+hsv;\n"
    allOutputs = red <> green <> blue <> alpha <> hsv <> rgb
    --
    glFragColor = "gl_FragColor = vec4(rgb,alpha);\n"
