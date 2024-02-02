module GLSLExpr where

-- This module provides types and functions to construct strongly-typed GLSL expressions
-- while keeping track of whether an expression is simple (and thus might avoid assignment),
-- it's type (and thus number of channels), and its dependency on any previously declared variables

import Prelude (class Eq, class Ord, class Show,(<>),(==),otherwise,(&&),($),map,(+))
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
zero = { string: "0.", glslType: Float, isSimple: true, deps: empty }

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

