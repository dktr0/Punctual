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
graphToFloat (Sine x) = "sin((" ++ graphToFloat x ++ ")*3.14159265*2.*t)"
graphToFloat (Tri x) = "0." -- placeholder
graphToFloat (Saw x) = "0." -- placeholder
graphToFloat (Square x) = "0." -- placeholder
graphToFloat (LPF i f q) = "0." -- placeholder
graphToFloat (HPF i f q) = "0." -- placeholder
graphToFloat (FromTarget x) = "0." -- placeholder
graphToFloat (Sum x y) = "(" ++ graphToFloat x ++ ")+(" ++ graphToFloat y ++ ")"
graphToFloat (Product x y) = "(" ++ graphToFloat x ++ ")*(" ++ graphToFloat y ++ ")"

header :: String
header
 = "precision mediump float;\
   \uniform float t;\
   \uniform lowp vec2 res;\
   \float fx() { return (gl_FragCoord.x / res.x) * 2. - 1.; }\
   \float fy() { return (gl_FragCoord.y / res.y) * 2. - 1.; }"

fragmentShader :: Evaluation -> String
fragmentShader e = header ++ "void main(){" ++ red ++ green ++ blue ++ footer
  where
    red = "float red = " ++ expressionForChannel e "red" ++ ";"
    green = "float green = " ++ expressionForChannel e "green" ++ ";"
    blue = "float blue = " ++ expressionForChannel e "blue" ++ ";"
    footer = "gl_FragColor = vec4(red,green,blue,1.0);}"

expressionForChannel :: Evaluation -> String -> String
expressionForChannel e chName = foldl f "0." $ fmap graphToFloat $ findGraphsForOutput chName e
  where f a b = a ++ "+" ++ b

defaultFragmentShader :: String
defaultFragmentShader = header ++ "void main() { gl_FragColor = vec4(0.,0.,0.,1.); }"
