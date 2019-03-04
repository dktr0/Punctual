module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Data.List (intercalate)
import Data.Map.Strict
import Data.Time
import GHCJS.Types
import GHCJS.DOM.Types

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.MusicW.AudioContext (utcTimeToDouble)

graphToFloat :: Graph -> String
graphToFloat EmptyGraph = "0."
graphToFloat (Constant x) = show x
graphToFloat Noise = "0." -- placeholder
graphToFloat Pink = "0." -- placeholder
graphToFloat Fx = "fx()"
graphToFloat Fy = "fy()"
graphToFloat (Sine x) = "sin_(" ++ graphToFloat x ++ ")"
graphToFloat (Tri x) = "tri(" ++ graphToFloat x ++ ")"
graphToFloat (Saw x) = "saw(" ++ graphToFloat x ++ ")"
graphToFloat (Square x) = "sqr(" ++ graphToFloat x ++ ")"
graphToFloat (LPF i f q) = graphToFloat i -- placeholder, doesn't filter yet
graphToFloat (HPF i f q) = graphToFloat i -- placeholder, doesn't filter yet
graphToFloat (FromTarget x) = "0." -- placeholder
graphToFloat (Sum x y) = "(" ++ graphToFloat x ++ "+" ++ graphToFloat y ++ ")"
graphToFloat (Product x y) = "(" ++ graphToFloat x ++ "*" ++ graphToFloat y ++ ")"

expressionToFloat :: Expression -> String
expressionToFloat (Expression (Definition _ _ _ g) _) = graphToFloat g

defaultFragmentShader :: JSString
defaultFragmentShader = toJSString $ header ++ "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

header :: String
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
   \float xFadeNew(float t1,float t2) { if (t>t2) return 1.; if (t<t1) return 0.; return ((t-t1)/(t2-t1));}\
   \float xFadeOld(float t1,float t2) { return 1.-xFadeNew(t1,t2);}"

targetToVariableName :: Target' -> String
targetToVariableName (Named s) = "_named_" ++ s;
targetToVariableName (Anon i) = "_anon_" ++ (show i);

continuingTarget :: (UTCTime,Double) -> UTCTime -> (Target',Expression) -> (Target',Expression) -> String
continuingTarget tempo evalTime (_,newExpr) (target',oldExpr) = oldVariable ++ newVariable ++ oldAndNew
  where
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    n = targetToVariableName target'
    oldVariable = "float _old" ++ n ++ "=" ++ expressionToFloat oldExpr ++ "*" ++ xFadeOld t1 t2 ++ ";\n"
    newVariable = "float _new" ++ n ++ "=" ++ expressionToFloat newExpr ++ "*" ++ xFadeNew t1 t2 ++ ";\n"
    oldAndNew = "float " ++ n ++ "=_old" ++ n ++ "+_new" ++ n ++ ";\n"

discontinuedTarget :: (UTCTime,Double) -> UTCTime -> (Target',Expression) -> String
discontinuedTarget tempo evalTime (target',oldExpr) = oldVariable
  where
    (t1,t2) = (evalTime,addUTCTime 0.5 evalTime) -- 0.5 sec
    n = targetToVariableName target'
    oldVariable = "float " ++ n ++ "=" ++ expressionToFloat oldExpr ++ "*" ++ xFadeOld t1 t2 ++ ";\n"

addedTarget :: (UTCTime,Double) -> UTCTime -> (Target',Expression) -> String
addedTarget tempo evalTime (target',newExpr) = newVariable
  where
    (t1,t2) = expressionToTimes tempo evalTime newExpr
    n = targetToVariableName target'
    newVariable = "float " ++ n ++ "=" ++ expressionToFloat newExpr ++ "*" ++ xFadeNew t1 t2 ++ ";\n"

xFadeOld :: UTCTime -> UTCTime -> String
xFadeOld t1 t2 = "xFadeOld(" ++ show (utcTimeToDouble t1) ++ "," ++ show (utcTimeToDouble t2) ++ ")"

xFadeNew :: UTCTime -> UTCTime -> String
xFadeNew t1 t2 = "xFadeNew(" ++ show (utcTimeToDouble t1) ++ "," ++ show (utcTimeToDouble t2) ++ ")"

fragmentShader :: [Expression] -> (UTCTime,Double) -> Evaluation -> JSString
fragmentShader xs0 tempo e@(xs1,t) = toJSString $ header ++ "void main() {\n" ++ allTargets ++ allOutputs ++ glFragColor ++ "}"
  where
    evalTime = addUTCTime 0.2 t
    -- generate maps of previous, current and all relevant expressions :: Map Target' (Target',Expression)
    oldExprs = mapWithKey (\k a -> (k,a)) $ listOfExpressionsToMap xs0
    newExprs = mapWithKey (\k a -> (k,a)) $ listOfExpressionsToMap xs1
    allExprs = union newExprs oldExprs
    -- using the maps in oldExprs and newExprs, generate GLSL shader code for each target, with crossfades
    continuing = intersectionWith (continuingTarget tempo evalTime) newExprs oldExprs -- Map Target' String
    continuing' = concat $ elems continuing -- String
    discontinued = fmap (discontinuedTarget tempo evalTime) $ difference oldExprs newExprs -- Map Target' String
    discontinued' = concat $ elems discontinued -- String
    added = fmap (addedTarget tempo evalTime) $ difference newExprs oldExprs -- Map Target' String
    added' = concat $ elems added
    allTargets = continuing' ++ discontinued' ++ added'
    --
    redExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "red") $ elems allExprs
    greenExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "green") $ elems allExprs
    blueExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "blue") $ elems allExprs
    redVars = intercalate "+" $ (["0."] ++) $ fmap (targetToVariableName . fst) redExprs
    greenVars = intercalate "+" $ (["0."] ++) $ fmap (targetToVariableName . fst) greenExprs
    blueVars = intercalate "+" $ (["0."] ++) $ fmap (targetToVariableName . fst) blueExprs
    red = "float red = 0.+" ++ redVars ++ ";\n"
    green = "float green = 0.+" ++ greenVars ++ ";\n"
    blue = "float blue = 0.+" ++ blueVars ++ ";\n"
    allOutputs = red ++ green ++ blue
    --
    glFragColor = "gl_FragColor = vec4(red,green,blue,1.);\n"
