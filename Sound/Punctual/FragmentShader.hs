module Sound.Punctual.FragmentShader where

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
graphToSynthDef (Sum x y) = "(" ++ graphToFloat x ++ ")+(" ++ graphToFloat y ++ ")"
graphToSynthDef (Product x y) = "(" ++ graphToFloat x ++ ")*(" ++ graphToFloat y ++ ")"

header :: String
header
 = "uniform float t;\
   \uniform vec2 res;\
   \float fx() { return gl_FragCoord.x / res.x * 2. - 1.; }\
   \float fy() { return gl_FragCoord.y / res.y * 2. - 1.; }"

punctualShader :: Evaluation -> String
punctualShader e = header ++ "void main(){" ++ red ++ green ++ blue ++ footer
  where
    red = "float _red = " ++ expressionForChannel e "red"
    green = "float _green = " ++ expressionForChannel e "green"
    blue = "float _blue = " ++ expressionForChannel e "blue"
    footer = "gl_FragColor = vec4(_red,_green,_blue,1.0);}"

expressionForChannel :: Evaluation -> String -> String
expressionForChannel e chName = foldl f "0." $ fmap graphToFloat $ findGraphsForOutput chName e
  where f a b = a ++ "+" ++ b
