module GLSLExpr where

-- This module provides types and functions to construct strongly-typed GLSL expressions
-- while keeping track of whether an expression is simple (and thus might avoid assignment),
-- it's type (and thus number of channels), and its dependency on any previously declared variables

import Prelude (class Eq, class Show,(<>),(==),otherwise,(&&))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Set (Set,empty)

data GLSLType = Float | Vec2 | Vec3 | Vec4

derive instance Eq GLSLType
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
