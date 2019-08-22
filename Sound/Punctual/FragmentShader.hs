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
import Sound.Punctual.Types
import Sound.Punctual.Evaluation

graphToFloat :: Graph -> Text
graphToFloat = graphToFloat' . mixGraphs . expandMultis

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
graphToFloat' (Mix _) = error "internal error: graphToFloat' should only be used after multi-channel expansion (can't handle Mix)"
graphToFloat' EmptyGraph = "0."
graphToFloat' (Constant x) = showt x
graphToFloat' Noise = "0." -- placeholder
graphToFloat' Pink = "0." -- placeholder
graphToFloat' Fx = "fx()"
graphToFloat' Fy = "fy()"
graphToFloat' Px = "10./1920."
graphToFloat' Py = "10./1080."
graphToFloat' (Sine x) = unaryShaderFunction "sin_" (graphToFloat' x)
graphToFloat' (Tri x) = unaryShaderFunction "tri" (graphToFloat' x)
graphToFloat' (Saw x) = unaryShaderFunction "saw" (graphToFloat' x)
graphToFloat' (Square x) = unaryShaderFunction "sqr" (graphToFloat' x)
graphToFloat' (LPF i f q) = graphToFloat' i -- placeholder, doesn't filter yet
graphToFloat' (HPF i f q) = graphToFloat' i -- placeholder, doesn't filter yet
graphToFloat' (FromTarget x) = "0." -- placeholder
graphToFloat' (Sum x y) = "(" <> graphToFloat' x <> "+" <> graphToFloat' y <> ")"
graphToFloat' (Product x y) = "(" <> graphToFloat' x <> "*" <> graphToFloat' y <> ")"
graphToFloat' (Division x y) = "(" <> graphToFloat' x <> "/" <> graphToFloat' y <> ")"
graphToFloat' (GreaterThan x y) = "float(" <> graphToFloat' x <> ">" <> graphToFloat' y <> ")"
graphToFloat' (GreaterThanOrEqual x y) = "float(" <> graphToFloat' x <> ">=" <> graphToFloat' y <> ")"
graphToFloat' (LessThan x y) = "float(" <> graphToFloat' x <> "<" <> graphToFloat' y <> ")"
graphToFloat' (LessThanOrEqual x y) = "float(" <> graphToFloat' x <> "<=" <> graphToFloat' y <> ")"
graphToFloat' (Equal x y) = "float(" <> graphToFloat' x <> "==" <> graphToFloat' y <> ")"
graphToFloat' (NotEqual x y) = "float(" <> graphToFloat' x <> "!=" <> graphToFloat' y <> ")"
graphToFloat' (MidiCps x) = "midicps(" <> graphToFloat' x <> ")"
graphToFloat' (CpsMidi x) = "cpsmidi(" <> graphToFloat' x <> ")"
graphToFloat' (DbAmp x) = "dbamp(" <> graphToFloat' x <> ")"
graphToFloat' (AmpDb x) = "ampdb(" <> graphToFloat' x <> ")"
graphToFloat' (Abs x) = "abs(" <> graphToFloat' x <> ")"
graphToFloat' (Sqrt x) = "sqrt(" <> graphToFloat' x <> ")"

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
   \float bipolar(float x) { return x * 2. - 1.; }\
   \float unipolar(float x) { return (x + 1.) * 0.5; }\
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
   \float xFadeOld(float t1,float t2) { return 1.-xFadeNew(t1,t2);}"

targetToVariableName :: Target' -> Text
targetToVariableName (Named s) = "_named_" <> s;
targetToVariableName (Anon i) = "_anon_" <> (showt i);

isVec3 :: Expression -> Bool
isVec3 x = output x == NamedOutput "rgb"

continuingTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> (Target',Expression) -> Text
continuingTarget tempo evalTime (newTarget,newExpr) (target',oldExpr) = oldVariable <> newVariable <> oldAndNew
  where
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    n = targetToVariableName target'
    oldVariable | isVec3 newExpr = "vec3 _old" <> n <> "=" <> expressionToVec3 oldExpr <> "*" <> xFadeOld t1 t2 <> ";\n"
                | otherwise = "float _old" <> n <> "=" <> expressionToFloat oldExpr <> "*" <> xFadeOld t1 t2 <> ";\n"
    newVariable | isVec3 newExpr = "vec3 _new" <> n <> "=" <> expressionToVec3 newExpr <> "*" <> xFadeNew t1 t2 <> ";\n"
                | otherwise = "float _new" <> n <> "=" <> expressionToFloat newExpr <> "*" <> xFadeNew t1 t2 <> ";\n"
    oldAndNew | isVec3 newExpr = "vec3 " <> n <> "=_old" <> n <> "+_new" <> n <> ";\n"
              | otherwise = "float " <> n <> "=_old" <> n <> "+_new" <> n <> ";\n"

discontinuedTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> Text
discontinuedTarget tempo evalTime (target',oldExpr) = oldVariable
  where
    (t1,t2) = (evalTime,0.5 + evalTime) -- 0.5 sec
    n = targetToVariableName target'
    oldVariable | isVec3 oldExpr = "vec3 " <> n <> "=" <> expressionToVec3 oldExpr <> "*" <> xFadeOld t1 t2 <> ";\n"
                | otherwise = "float " <> n <> "=" <> expressionToFloat oldExpr <> "*" <> xFadeOld t1 t2 <> ";\n"

addedTarget :: (AudioTime,Double) -> AudioTime -> (Target',Expression) -> Text
addedTarget tempo evalTime (target',newExpr) = newVariable
  where
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    n = targetToVariableName target'
    newVariable | isVec3 newExpr = "vec3 " <> n <> "=" <> expressionToVec3 newExpr <> "*" <> xFadeNew t1 t2 <> ";\n"
                | otherwise = "float " <> n <> "=" <> expressionToFloat newExpr <> "*" <> xFadeNew t1 t2 <> ";\n"

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
    redVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) redExprs
    greenVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) greenExprs
    blueVars = T.intercalate "+" $ (["0."] <>) $ fmap (targetToVariableName . fst) blueExprs
    alphaVars = if length alphaExprs == 0 then "1." else
      T.intercalate "+" $ fmap (targetToVariableName . fst) alphaExprs
    rgbVars = T.intercalate "+" $ (["vec3(0.)"] <>) $ fmap (targetToVariableName . fst) rgbExprs
    red = "float red = " <> redVars <> ";\n"
    green = "float green = " <> greenVars <> ";\n"
    blue = "float blue = " <> blueVars <> ";\n"
    alpha = "float alpha = " <> alphaVars <> ";\n"
    rgb = "vec3 rgb = " <> rgbVars <> "+vec3(red,green,blue);\n"
    allOutputs = red <> green <> blue <> alpha <> rgb
    --
    glFragColor = "gl_FragColor = vec4(rgb,alpha);\n"
