module GLSLExpr where

-- This module provides types and functions to construct strongly-typed GLSL expressions
-- while keeping track of whether an expression is simple (and thus might avoid assignment),
-- it's type (and thus number of channels), and its dependency on any previously declared variables

import Prelude (class Eq, class Ord, class Show,(<>),(==),(/=),otherwise,(&&),($),map,(+),(||))
import Prelude as Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Set (Set,empty)
import Data.List.NonEmpty (NonEmptyList)
import Data.Foldable (foldl)

data GLSLType = Float | Vec2 | Vec3 | Vec4

derive instance Eq GLSLType
derive instance Ord GLSLType
derive instance Generic GLSLType _
instance Show GLSLType where
  show = genericShow

glslTypeToString :: GLSLType -> String
glslTypeToString Float = "float"
glslTypeToString Vec2 = "vec2"
glslTypeToString Vec3 = "vec3"
glslTypeToString Vec4 = "vec4"


type GLSLExpr = {
  string :: String,
  glslType :: GLSLType,
  isSimple :: Boolean,
  deps :: Set Int
  }

exprChannels :: GLSLExpr -> Int
exprChannels x
  | x.glslType == Float = 1
  | x.glslType == Vec2 = 2
  | x.glslType == Vec3 = 3
  | otherwise {- Vec4 -} = 4

simpleFromString :: GLSLType -> String -> GLSLExpr
simpleFromString t x = { string: x, glslType: t, isSimple: true, deps: empty }

zero :: GLSLExpr
zero = explicitlyTypedZero Float 

explicitlyTypedZero :: GLSLType -> GLSLExpr
explicitlyTypedZero Float = { string: "0.", glslType: Float, isSimple: true, deps: empty }
explicitlyTypedZero Vec2 = { string: "vec2(0.)", glslType: Vec2, isSimple: true, deps: empty }
explicitlyTypedZero Vec3 = { string: "vec3(0.)", glslType: Vec3, isSimple: true, deps: empty }
explicitlyTypedZero Vec4 = { string: "vec4(0.)", glslType: Vec4, isSimple: true, deps: empty }

one :: GLSLExpr
one = { string: "1.", glslType: Float, isSimple: true, deps: empty }

vec2unary :: GLSLExpr -> GLSLExpr
vec2unary = simpleUnaryFunctionPure "vec2" Vec2

vec2binary :: GLSLExpr -> GLSLExpr -> GLSLExpr
vec2binary = simpleBinaryFunction "vec2" Vec2

vec3unary :: GLSLExpr -> GLSLExpr
vec3unary = simpleUnaryFunctionPure "vec3" Vec3

vec3binary :: GLSLExpr -> GLSLExpr -> GLSLExpr
vec3binary = simpleBinaryFunction "vec3" Vec3

vec3ternary :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vec3ternary = ternaryFunction "vec3" Vec3

vec4unary :: GLSLExpr -> GLSLExpr
vec4unary = simpleUnaryFunctionPure "vec4" Vec4

vec4binary :: GLSLExpr -> GLSLExpr -> GLSLExpr
vec4binary = simpleBinaryFunction "vec4" Vec4

vec4ternary :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vec4ternary = ternaryFunction "vec4" Vec4

vec4quaternary :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vec4quaternary = quaternaryFunction "vec4" Vec4

-- create a Float by extending or cutting smaller/bigger types
coerceFloat :: GLSLExpr -> GLSLExpr
coerceFloat x
  | x.glslType == Float = x
  | otherwise = { string: x.string <> ".x", glslType: Float, isSimple: x.isSimple, deps: x.deps }

-- create a Vec2 by extending or cutting smaller/bigger types
coerceVec2 :: GLSLExpr -> GLSLExpr
coerceVec2 x
  | x.glslType == Float = vec2unary x
  | x.glslType == Vec2 = x
  | otherwise = { string: x.string <> ".xy", glslType: Vec2, isSimple: x.isSimple, deps: x.deps }

-- create a Vec3 by extending or cutting smaller/bigger types
coerceVec3 :: GLSLExpr -> GLSLExpr
coerceVec3 x
  | x.glslType == Float = vec3unary x
  | x.glslType == Vec2 = { string: x.string <> ".xyy", glslType: Vec3, isSimple: x.isSimple, deps: x.deps }
  | x.glslType == Vec3 = x
  | otherwise {- Vec4 -} = { string: x.string <> ".xyz", glslType: Vec3, isSimple: x.isSimple, deps: x.deps }

-- create a Vec4 by extending smaller types
coerceVec4 :: GLSLExpr -> GLSLExpr
coerceVec4 x
  | x.glslType == Float = vec4unary x
  | x.glslType == Vec2 = { string: x.string <> ".xyyy", glslType: Vec4, isSimple: x.isSimple, deps: x.deps }
  | x.glslType == Vec3 = { string: x.string <> ".xyzz", glslType: Vec4, isSimple: x.isSimple, deps: x.deps }
  | otherwise {- Vec4 -} = x

coerce :: GLSLType -> GLSLExpr -> GLSLExpr
coerce Float = coerceFloat
coerce Vec2 = coerceVec2
coerce Vec3 = coerceVec3
coerce Vec4 = coerceVec4

forceCast :: GLSLType -> GLSLExpr -> GLSLExpr
forceCast Float x = { string: "float(" <> x.string <> ")", glslType: Float, isSimple: x.isSimple, deps: x.deps }
forceCast Vec2 x = { string: "vec2(" <> x.string <> ")", glslType: Vec2, isSimple: x.isSimple, deps: x.deps }
forceCast Vec3 x = { string: "vec3(" <> x.string <> ")", glslType: Vec3, isSimple: x.isSimple, deps: x.deps }
forceCast Vec4 x = { string: "vec4(" <> x.string <> ")", glslType: Vec4, isSimple: x.isSimple, deps: x.deps }

simpleUnaryFunctionPure :: String -> GLSLType -> GLSLExpr -> GLSLExpr
simpleUnaryFunctionPure funcName rType x = { string: funcName <> "(" <> x.string <> ")", glslType: rType, isSimple: x.isSimple, deps: x.deps }

-- simpleUnaryExpressionPure :: (String -> String) -> GLSLType -> GLSLExpr -> GLSLExpr
-- simpleUnaryExpressionPure f rType x = { string: funcName <> "(" <> x.string <> ")", glslType: rType, isSimple: x.isSimple, deps: x.deps }

unaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr
unaryFunction funcName rType x = { string: funcName <> "(" <> x.string <> ")", glslType: rType, isSimple: false, deps: x.deps }

simpleBinaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
simpleBinaryFunction funcName rType x y = { string: funcName <> "(" <> x.string <> "," <> y.string <> ")", glslType: rType, isSimple: x.isSimple && y.isSimple, deps: x.deps <> y.deps }

binaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunction funcName rType x y = { string: funcName <> "(" <> x.string <> "," <> y.string <> ")", glslType: rType, isSimple: false, deps: x.deps <> y.deps }

ternaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
ternaryFunction funcName rType x y z = { string: funcName <> "(" <> x.string <> "," <> y.string <> "," <> z.string <> ")", glslType: rType, isSimple: false, deps: x.deps <> y.deps <> z.deps }

quaternaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
quaternaryFunction funcName rType w x y z = { string: funcName <> "(" <> w.string <> "," <> x.string <> "," <> y.string <> "," <> z.string <> ")", glslType: rType, isSimple: false, deps: w.deps <> x.deps <> y.deps <> z.deps }

-- sum the components of GLSLType using the dot product with 1 (or a pass-through in the case of Float)
-- (possibly faster to execute, possibly leads to somewhat terser shader code)
dotSum :: GLSLExpr -> GLSLExpr
dotSum x
  | x.glslType == Float = x
  | x.glslType == Vec2 = { string: "dot(" <> x.string <> ",vec2(1.))", glslType: Float, isSimple: x.isSimple, deps: x.deps }
  | x.glslType == Vec3 = { string: "dot(" <> x.string <> ",vec3(1.))", glslType: Float, isSimple: x.isSimple, deps: x.deps }
  | otherwise = { string: "dot(" <> x.string <> ",vec4(1.))", glslType: Float, isSimple: x.isSimple, deps: x.deps }


unsafeSwizzleXY :: GLSLExpr -> GLSLExpr
unsafeSwizzleXY a = { string: a.string <> ".xy", glslType: Vec2, isSimple: a.isSimple, deps: a.deps }

unsafeSwizzleZW :: GLSLExpr -> GLSLExpr
unsafeSwizzleZW a = { string: a.string <> ".zw", glslType: Vec2, isSimple: a.isSimple, deps: a.deps }


type Exprs = NonEmptyList GLSLExpr

exprsChannels :: Exprs -> Int
exprsChannels xs = foldl (+) 0 $ map exprChannels xs

{-
The functions below wrap binary GLSL functions and operators, polyfilling where necessary to provide
a standard model such that each function can be used with arguments of matching type, or with either argument as a float
-}

-- for +-*/ the arguments (in GLSL) are the same type or either argument can be a float (regardless of the other argument)
-- this is already the standard model
glslArithmeticOperator :: String -> GLSLExpr -> GLSLExpr -> GLSLExpr
glslArithmeticOperator o x y
  | x.glslType == y.glslType = { string: "(" <> x.string <> o <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | x.glslType == Float = { string: "(" <> x.string <> o <> y.string <> ")", glslType: y.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | y.glslType == Float = { string: "(" <> x.string <> o <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | otherwise = { string: "!! Internal Punctual GLSL generation error in " <> o, glslType: Float, isSimple: false, deps: x.deps <> y.deps }

sum :: GLSLExpr -> GLSLExpr -> GLSLExpr
sum = glslArithmeticOperator "+"

difference :: GLSLExpr -> GLSLExpr -> GLSLExpr
difference = glslArithmeticOperator "-"

product :: GLSLExpr -> GLSLExpr -> GLSLExpr
product = glslArithmeticOperator "*"

division :: GLSLExpr -> GLSLExpr -> GLSLExpr
division = glslArithmeticOperator "/"

-- arguments are same type or either can be a float, order is irrelevant
minOrMax :: String -> GLSLExpr -> GLSLExpr -> GLSLExpr
minOrMax f x y
  | x.glslType == y.glslType = { string: f <> "(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | y.glslType == Float = { string: f <> "(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | x.glslType == Float = { string: f <> "(" <> y.string <> "," <> x.string <> ")", glslType: y.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | otherwise = { string: "!! Internal Punctual GLSL generation error in " <> f, glslType: Float, isSimple: false, deps: x.deps <> y.deps }

min :: GLSLExpr -> GLSLExpr -> GLSLExpr
min = minOrMax "min"

-- arguments are same type or either can be a float (this is a small polyfill beyond GLSL standard, puts min and max with arithmetic operators)
max :: GLSLExpr -> GLSLExpr -> GLSLExpr
max = minOrMax "max"

-- to polyfill pow and mod to the standard model, coerce unmatched float arguments to the other type (will simply be a cast)
powOrMod :: String -> GLSLExpr -> GLSLExpr -> GLSLExpr
powOrMod f x y
  | x.glslType == y.glslType = { string: f <> "(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | x.glslType == Float = { string: f <> "(" <> (coerce y.glslType x).string <> "," <> y.string <> ")", glslType: y.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | y.glslType == Float = { string: f <> "(" <> x.string <> "," <> (coerce x.glslType y).string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps  }
  | otherwise = { string: "!! Internal Punctual GLSL generation error in " <> f, glslType: Float, isSimple: false, deps: x.deps <> y.deps }

pow :: GLSLExpr -> GLSLExpr -> GLSLExpr
pow = powOrMod "pow"

mod :: GLSLExpr -> GLSLExpr -> GLSLExpr
mod = powOrMod "mod"

comparisonOperator :: String -> String -> GLSLExpr -> GLSLExpr -> GLSLExpr
comparisonOperator o f x y
  | x.glslType == Float && y.glslType == Float = { string: "float(" <> x.string <> o <> y.string <> ")", glslType: Float, isSimple: false, deps: x.deps <> y.deps }
  | x.glslType == y.glslType = forceCast x.glslType { string: f <> "(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps }
  | x.glslType == Float = forceCast y.glslType { string: "(" <> (coerce y.glslType x).string <> o <> y.string <> ")", glslType: y.glslType, isSimple: false, deps: x.deps <> y.deps }
  | y.glslType == Float = forceCast y.glslType { string: "(" <> x.string <> o <> (coerce x.glslType y).string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps }
  | otherwise = { string: "!! Internal Punctual GLSL generation error in " <> f, glslType: Float, isSimple: false, deps: x.deps <> y.deps }

equal :: GLSLExpr -> GLSLExpr -> GLSLExpr
equal = comparisonOperator "==" "equal"

notEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
notEqual = comparisonOperator "!=" "notEqual"

greaterThan :: GLSLExpr -> GLSLExpr -> GLSLExpr
greaterThan = comparisonOperator ">" "greaterThan"

greaterThanEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
greaterThanEqual = comparisonOperator ">=" "greaterThanEqual"

lessThan :: GLSLExpr -> GLSLExpr -> GLSLExpr
lessThan = comparisonOperator "<" "lessThan"

lessThanEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
lessThanEqual = comparisonOperator "<=" "lessThanEqual"

-- caller should assign y because of the reuse of y in this definition
gate :: GLSLExpr -> GLSLExpr -> GLSLExpr
gate x y = product (lessThan x y) y

-- first argument must be Vec2 and should be pre-assigned because of multiple use within this definition
clip :: GLSLExpr -> GLSLExpr -> GLSLExpr
clip r x
  | r.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in clip", glslType: Float, isSimple: false, deps: r.deps <> x.deps }
  | otherwise = { string: "clamp(" <> x.string <> "," <> guaranteedMin <> "," <> guaranteedMax <> ")", glslType: x.glslType, isSimple: false, deps: r.deps <> x.deps }
      where 
        r1 = r.string <> ".x"
        r2 = r.string <> ".y"
        guaranteedMin = "min(" <> r1 <> "," <> r2 <> ")"
        guaranteedMax = "max(" <> r1 <> "," <> r2 <> ")"

-- first argument must be Vec2 and should be pre-assigned because of multiple use within this definition
between :: GLSLExpr -> GLSLExpr -> GLSLExpr
between r x
  | r.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in between", glslType: Float, isSimple: false, deps: r.deps <> x.deps }
  | otherwise = { string: s, glslType: x.glslType, isSimple: false, deps: r.deps <> x.deps }
      where 
        r1 = r.string <> ".x"
        r2 = r.string <> ".y"
        guaranteedMin = "min(" <> r1 <> "," <> r2 <> ")"
        guaranteedMax = "max(" <> r1 <> "," <> r2 <> ")"
        s = "(step(" <> guaranteedMin <> "," <> x.string <> ")*(1.-step(" <> guaranteedMax <> "," <> x.string <> ")))"

-- first argument must be Vec2 and should be pre-assigned because of multiple use within this definition
smoothstep :: GLSLExpr -> GLSLExpr -> GLSLExpr
smoothstep r x
  | r.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in smoothstep", glslType: Float, isSimple: false, deps: r.deps <> x.deps }
  | otherwise = { string: "smoothstep(" <> r1 <> "," <> r2 <> "," <> x.string <> ")", glslType: x.glslType, isSimple: false, deps: r.deps <> x.deps }
      where 
        r1 = r.string <> ".x"
        r2 = r.string <> ".y"

circle :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Any -> Any (no reuse of any arguments, so no inherent need to pre-assign them)
circle fxy xy d 
  | fxy.glslType /= Vec2 || xy.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in circle", glslType: Float, isSimple: false, deps: fxy.deps <> xy.deps <> d.deps }
  | otherwise = { string: s, glslType: d.glslType, isSimple: false, deps: fxy.deps <> xy.deps <> d.deps }
      where s = "smoothstep(1.5/(width+height),0.0,distance(" <> fxy.string <> "," <> xy.string <> ")-(" <> d.string <> "*0.5))"

point :: GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2
point fxy xy = circle fxy xy d
  where d = { string: "((1./width)+(1./height))", glslType: Float, isSimple: false, deps: empty }
 
      
-- TODO: will need to add rect function to fragment shader header
rect :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Vec2 -> Float (no reuse of any arguments, so no inherent need to pre-assign them)
rect fxy xy wh 
  | fxy.glslType /= Vec2 || xy.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in rect", glslType: Float, isSimple: false, deps: fxy.deps <> xy.deps <> wh.deps }
  | otherwise = { string: "rect(" <> fxy.string <> "," <> xy.string <> "," <> wh.string <> ")", glslType: Float, isSimple: false, deps: fxy.deps <> xy.deps <> wh.deps }

vline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
vline fxy x w 
  | x.glslType /= Float && w.glslType /= Float && x.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in vline", glslType: Float, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max x.glslType w.glslType, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".x-" <> x.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./width)"
        s = "(1.-smoothstep(" <> edge0.string <> "," <> edge1 <> "," <> a <> "))"

hline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
hline fxy y w 
  | y.glslType /= Float && w.glslType /= Float && y.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in hline", glslType: Float, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max y.glslType w.glslType, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".y-" <> y.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./height)"
        s = "(1.-smoothstep(" <> edge0.string <> "," <> edge1 <> "," <> a <> "))"

line :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Vec2 Float -> Float (no reuse of any arguments)
line fxy xy1 xy2 w
  | fxy.glslType /= Vec2 || xy1.glslType /= Vec2 || xy2.glslType /= Vec2 || w.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in line", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }
  | otherwise = { string: "line(" <> xy1.string <> "," <> xy2.string <> "," <> w.string <> "," <> fxy.string <> ")", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }

iline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Vec2 Float -> Float (no reuse of any arguments)
iline fxy xy1 xy2 w
  | fxy.glslType /= Vec2 || xy1.glslType /= Vec2 || xy2.glslType /= Vec2 || w.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in line", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }
  | otherwise = { string: "iline(" <> xy1.string <> "," <> xy2.string <> "," <> w.string <> "," <> fxy.string <> ")", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }

-- \float linlin(vec2 r1, vec2 r2, float x) { return r2.x+((r2.y-r2.x)*(x-r1.x)/(r1.y-r1.x));}\

linlin :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Any -> Any, vec2 range arguments are re-used and should be pre-assigned
linlin r1 r2 x 
  | r1.glslType /= Vec2 || r2.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in linlin", glslType: Float, isSimple: false, deps: r1.deps <> r2.deps <> x.deps }
  | otherwise = { string: s, glslType: x.glslType, isSimple: false, deps: r1.deps <> r2.deps <> x.deps }
      where
        x' = "(" <> x.string <> "-" <> r1.string <> ".x)"
        r1size = "(" <> r1.string <> ".y-" <> r1.string <> ".x)"
        r2size = "(" <> r2.string <> ".y-" <> r2.string <> ".x)"
        s = "(" <> r2.string <> ".x+(" <> r2size <> "*" <> x' <> "/" <> r1size <> "))"




