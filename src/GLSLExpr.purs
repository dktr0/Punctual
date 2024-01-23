module GLSLExpr where

-- This module provides types and functions to construct strongly-typed GLSL expressions
-- while keeping track of whether an expression is simple (and thus might avoid assignment),
-- it's type (and thus number of channels), and its dependency on any previously declared variables

import Prelude (class Eq, class Show, show, (<>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..))

data GLSLType = Float | Vec2 | Vec3 | Vec4

derive instance Eq GLSLType
derive instance Generic GLSLType _
instance Show GLSLType where
  show = genericShow

type GLSLExpr = {
  string :: String,
  glslType :: GLSLType,
  isSimple :: Boolean,
  deps :: List Int
  }
  
   
float :: Number -> GLSLExpr
float x = { string: show x, glslType: Float, isSimple: true, deps: Nil }

vec2 :: GLSLExpr -> GLSLExpr -> GLSLExpr
vec2 = binaryFunction "vec2" Vec2 

vec3 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vec3 = ternaryFunction "vec3" Vec3 

vec4 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
vec4 = quaternaryFunction "vec4" Vec4 

-- the List Int should be the index of the variable to which this refers as well as the index of all of its dependencies, etc
floatRef :: String -> List Int -> GLSLExpr
floatRef x ds = { string: x, glslType: Float, isSimple: true, deps: ds }

vec2Ref :: String -> List Int -> GLSLExpr
vec2Ref x ds = { string: x, glslType: Vec2, isSimple: true, deps: ds }

vec3Ref :: String -> List Int -> GLSLExpr
vec3Ref x ds = { string: x, glslType: Vec3, isSimple: true, deps: ds }

vec4Ref :: String -> List Int -> GLSLExpr
vec4Ref x ds = { string: x, glslType: Vec4, isSimple: true, deps: ds }


simpleUnaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr
simpleUnaryFunction funcName rType x = { string: funcName <> "(" <> x.string <> ")", glslType: rType, isSimple: x.isSimple, deps: x.deps }

unaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr
unaryFunction funcName rType x = { string: funcName <> "(" <> x.string <> ")", glslType: rType, isSimple: false, deps: x.deps }

binaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunction funcName rType x y = { string: funcName <> "(" <> x.string <> "," <> y.string <> ")", glslType: rType, isSimple: false, deps: x.deps <> y.deps }

ternaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
ternaryFunction funcName rType x y z = { string: funcName <> "(" <> x.string <> "," <> y.string <> "," <> z.string <> ")", glslType: rType, isSimple: false, deps: x.deps <> y.deps <> z.deps }

quaternaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
quaternaryFunction funcName rType w x y z = { string: funcName <> "(" <> w.string <> "," <> x.string <> "," <> y.string <> "," <> z.string <> ")", glslType: rType, isSimple: false, deps: w.deps <> x.deps <> y.deps <> z.deps }


_swizzle :: String -> GLSLType -> GLSLExpr -> GLSLExpr
_swizzle spec rType x = { string: x.string <> "." <> spec, glslType: rType, isSimple: x.isSimple, deps: x.deps }

swizzleX :: GLSLExpr -> GLSLExpr
swizzleX = _swizzle "x" Float 

swizzleY :: GLSLExpr -> GLSLExpr
swizzleY = _swizzle "y" Float

swizzleZ :: GLSLExpr -> GLSLExpr
swizzleZ = _swizzle "z" Float

swizzleW :: GLSLExpr -> GLSLExpr
swizzleW = _swizzle "w" Float

swizzleXY :: GLSLExpr -> GLSLExpr
swizzleXY = _swizzle "xy" Vec2

swizzleYZ :: GLSLExpr -> GLSLExpr
swizzleYZ = _swizzle "yz" Vec2

swizzleZW :: GLSLExpr -> GLSLExpr
swizzleZW = _swizzle "zw" Vec2

swizzleXYZ :: GLSLExpr -> GLSLExpr
swizzleXYZ = _swizzle "xyz" Vec3

swizzleYZW :: GLSLExpr -> GLSLExpr
swizzleYZW = _swizzle "yzw" Vec3

swizzleXYY :: GLSLExpr -> GLSLExpr
swizzleXYY = _swizzle "xyy" Vec3

swizzleXYYY :: GLSLExpr -> GLSLExpr
swizzleXYYY = _swizzle "xyyy" Vec4
 
swizzleXYZZ :: GLSLExpr -> GLSLExpr
swizzleXYZZ = _swizzle "xyzz" Vec4

