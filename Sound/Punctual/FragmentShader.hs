{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.FragmentShader (fragmentShader,defaultFragmentShader) where

import Data.IntMap.Strict as IntMap
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import TextShow
import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Graph hiding (difference)
import Sound.Punctual.Evaluation

graphToFloat :: Graph -> Text
graphToFloat (Multi _) = error "internal error: graphToFloat should only be used after multi-channel expansion (can't handle Multi)"
graphToFloat (Mono _) = error "internal error: graphToFloat should only be used after multi-channel expansion (can't handle Mono)"
graphToFloat (Constant x) = showt x
graphToFloat Fx = "fx()"
graphToFloat Fy = "fy()"
graphToFloat Px = "10./1920."
graphToFloat Py = "10./1080."
graphToFloat (Sine x) = unaryShaderFunction "sin_" x
graphToFloat (Tri x) = unaryShaderFunction "tri" x
graphToFloat (Saw x) = unaryShaderFunction "saw" x
graphToFloat (Square x) = unaryShaderFunction "sqr" x
graphToFloat (LPF i f q) = graphToFloat i -- placeholder, doesn't filter yet
graphToFloat (HPF i f q) = graphToFloat i -- placeholder, doesn't filter yet
graphToFloat (FromTarget x) = "0." -- placeholder
graphToFloat (Sum x y) = "(" <> graphToFloat x <> "+" <> graphToFloat y <> ")"
graphToFloat (Product x y) = "(" <> graphToFloat x <> "*" <> graphToFloat y <> ")"
graphToFloat (Division x y) = "(" <> graphToFloat x <> "/" <> graphToFloat y <> ")"
graphToFloat (GreaterThan x y) = "float(" <> graphToFloat x <> ">" <> graphToFloat y <> ")"
graphToFloat (GreaterThanOrEqual x y) = "float(" <> graphToFloat x <> ">=" <> graphToFloat y <> ")"
graphToFloat (LessThan x y) = "float(" <> graphToFloat x <> "<" <> graphToFloat y <> ")"
graphToFloat (LessThanOrEqual x y) = "float(" <> graphToFloat x <> "<=" <> graphToFloat y <> ")"
graphToFloat (Equal x y) = "float(" <> graphToFloat x <> "==" <> graphToFloat y <> ")"
graphToFloat (NotEqual x y) = "float(" <> graphToFloat x <> "!=" <> graphToFloat y <> ")"
graphToFloat (MidiCps x) = unaryShaderFunction "midicps" x
graphToFloat (CpsMidi x) = unaryShaderFunction "cpsmidi" x
graphToFloat (DbAmp x) = unaryShaderFunction "dbamp" x
graphToFloat (AmpDb x) = unaryShaderFunction "ampdb" x
graphToFloat (Abs x) = unaryShaderFunction "abs" x
graphToFloat (Sqrt x) = unaryShaderFunction "sqrt" x

unaryShaderFunction :: Text -> Graph -> Text
unaryShaderFunction f x = f <> "(" <> graphToFloat x <> ")"

-- the Text result is GLSL code to instantiate the array with the provided name
-- and initialize all of its components/channels. The Int result is the size of
-- the array (ie. the number of channels in the expansion of the Graph in this Definition).
-- the addendum is meant to be used to add crossfades into each channel component.
definitionToFloatArray :: Text -> Text -> Definition -> (Text,Int)
definitionToFloatArray name addendum def = (declaration <> channels,nchnls)
  where
    graphs = expandMultis $ graph def
    nchnls = length graphs
    declaration = "float " <> name <> "[" <> showt nchnls <> "];\n"
    f n g = name <> "[" <> showt n <> "] = " <> graphToFloat g <> addendum <> ";\n"
    channels = concat $ zipWith f [0..] graphs

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

continuedDefinition :: (AudioTime,Double) -> AudioTime -> Int -> Definition -> Definition -> (Text,Int)
continuedDefinition tempo evalTime key newDef oldDef = (oldCode <> newCode <> mergeCode,newChnls)
  where
    (t1,t2) = definitionToTimes tempo evalTime newDef
    (oldCode,oldChnls) = definitionToFloatArray ("_old_" <> showt key) ("*" <> xFadeOld t1 t2) oldDef
    (newCode,newChnls) = definitionToFloatArray ("_new_" <> showt key) ("*" <> xFadeNew t1 t2) newDef
    mergeCode = "_" <> showt key <> "=mergeOldAndNew("

discontinuedDefinition :: (AudioTime,Double) -> AudioTime -> Int -> Definition -> (Text,Int)
discontinuedDefinition tempo evalTime key oldDef = definitionToFloatArray ("_" <> showt key) ("*" <> xFadeOld t1 t2) oldDef
  where (t1,t2) = (evalTime,0.5 + evalTime) -- 0.5 secHi Jakgord

addedDefinition :: (AudioTime,Double) -> AudioTime -> Int -> Definition -> (Text,Int)
addedDefinition tempo evalTime key newDef = definitionToFloatArray ("_" <> showt key) ("*" <> xFadeNew t1 t2) newDef
  where (t1,t2) = definitionToTimes tempo evalTime newDef

xFadeOld :: AudioTime -> AudioTime -> Text
xFadeOld t1 t2 = "xFadeOld(" <> showt t1 <> "," <> showt t2 <> ")"

xFadeNew :: AudioTime -> AudioTime -> Text
xFadeNew t1 t2 = "xFadeNew(" <> showt t1 <> "," <> showt t2 <> ")"

fragmentShader :: Program -> (AudioTime,Double) -> Evaluation -> Text
fragmentShader oldProg tempo e@(newProg,evalTime0) = header <> "void main() {\n" <> allDefsCode <> allOutputs <> glFragColor <> "}"
  where
    evalTime = 0.2 + evalTime0
    allDefs = union newProg oldProg
    -- generate GLSL shader code for each definition, with crossfades
    continued = intersectionWith (continuedDefinition tempo evalTime) newProg prevProg -- IntMap (Text,Int)
    discontinued = mapWithKey (discontinuedDefinition tempo evalTime) $ difference oldProg newProg -- IntMap (Text,Int)
    added = mapWithKey (addedDefinition tempo evalTime) $ difference newProg oldProg -- IntMap (Text,Int,Int)
    continuedCode = T.concat $ fmap fst $ elems continued
    discontinuedCode = T.concat $ fmap fst $ elems discontinued
    addedCode = T.concat $ fmap (\(x,_,_) -> x) $ elems added
    allDefsCode = continuedCode <> discontinuedCode <> addedCode
    --
    redDefs = IntMap.filter (  )
    Exprs = Prelude.filter (\(_,x) -> output x == NamedOutput "red") $ elems allExprs
    greenExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "green") $ elems allExprs
    blueExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "blue") $ elems allExprs
    alphaExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "alpha") $ elems allExprs
    rgbExprs = Prelude.filter (\(_,x) -> output x == NamedOutput "rgb") $ elems allExprs
    --
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
