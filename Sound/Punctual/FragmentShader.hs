module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Sound.Punctual.Graph
import Sound.Punctual.Evaluation

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
graphToFloat (Sum x y) = "(" ++ graphToFloat x ++ ")+(" ++ graphToFloat y ++ ")"
graphToFloat (Product x y) = "(" ++ graphToFloat x ++ ")*(" ++ graphToFloat y ++ ")"

defaultFragmentShader :: String
defaultFragmentShader = header ++ "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"

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
   \float xFadeNew(float t1,float t2) { if (t>t2) return 1.; if (t<t1) return 0.; return ();}\
   \float xFadeOld(float t1,float t2) { return 1.-xFadeNew(t1,t2);}"

fragmentShader :: Evaluation -> String
fragmentShader e = header ++ "void main(){" ++ red ++ green ++ blue ++ footer
  where
    red = "float red = " ++ expressionForChannel e "red" ++ ";"
    green = "float green = " ++ expressionForChannel e "green" ++ ";"
    blue = "float blue = " ++ expressionForChannel e "blue" ++ ";"
    footer = "gl_FragColor = vec4(red,green,blue,1.0);}"


-- *** working below here

targetToVariableName :: Target' -> String
targetToVariableName (Named s) = "_" ++ s;
targetToVariableName (Anon i) = "__anon" ++ (show i);

maybeExpressionToFloat :: Maybe Expression -> String
maybeDefinitionToFloat (Just (Expression (Definition _ _ _ g) _) = graphToFloat g

f :: (UTCTime,Double) -> UTCTime -> Target' -> Maybe Expression -> Maybe Expression -> String
f tempo evalTime target' oldExpr newExpr = oldVariable ++ newVariable ++ oldAndNew
  where
    (t1,t2) = maybeExpressionToTimes tempo evalTime newExpr
    n = targetToVariableName target'
    oldVariable = "float _old_" ++ n ++ "=" ++ maybeExpressionToFloat oldExpr ++ "*xFadeOld(" ++ show t1 ++ "," ++ show t2 ++ ");\n"
    newVariable = "float _new_" ++ n ++ '=' ++ maybeExpressionToFloat newExpr ++ "*xFadeNew(" ++ show t1 ++ "," ++ show t2 ++ ");\n"
    oldAndNew = "float _" ++ n ++ "=_old_" ++ n ++ "+_new_" ++ n ++ ";\n"

fragmentShader :: PunctualState -> (UTCTime,Double) -> Evaluation -> Identity String
fragmentShader s tempo e@(xs,t) = do
  let evalTime = addUTCTime 0.2 t
  let exprs = listOfExpressionsToMap xs -- Map Target' Expression

-- *** working above here
-- *** note: below here, expressionForChannel gets reworked so that channel expressions are just
-- sums of Target' names...
-- *** and then above, fragmentShader gets reworked to include 'f' above

expressionForChannel :: Evaluation -> String -> String
expressionForChannel e chName = foldl f "0." $ fmap graphToFloat $ findGraphsForOutput chName e
  where f b a = b ++ "+" ++ a
