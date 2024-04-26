module Expr where

import Prelude (class Eq, class Ord, class Show,($),(<>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Channels
import Multi (Multi)

data GLSLType = Float | Vec2 | Vec3 | Vec4

derive instance Eq GLSLType
derive instance Ord GLSLType
derive instance Generic GLSLType _
instance Show GLSLType where
  show = genericShow

instance Channels GLSLType where
  channels Float = 1
  channels Vec2 = 2
  channels Vec3 = 3
  channels Vec4 = 4

glslTypeToString :: GLSLType -> String
glslTypeToString Float = "float"
glslTypeToString Vec2 = "vec2"
glslTypeToString Vec3 = "vec3"
glslTypeToString Vec4 = "vec4"

data Expr =
  ConstantFloat Number |
  Reference GLSLType String
  
instance Channels Expr where
  channels (ConstantFloat _) = 1
  channels (Reference t _) = channels t

_swizzle :: String -> GLSLType -> Expr -> Expr
_swizzle _ _ (ConstantFloat x) = ConstantFloat x -- swizzling a ConstantFloat is a no-op (not sure about this, but for now...)
_swizzle m t (Reference _ x) = Reference t $ x <> "." <> m

swizzleX :: Expr -> Expr
swizzleX = _swizzle "x" Float

swizzleY :: Expr -> Expr
swizzleY = _swizzle "y" Float

swizzleZ :: Expr -> Expr
swizzleZ = _swizzle "z" Float

swizzleW :: Expr -> Expr
swizzleW = _swizzle "w" Float

swizzleXY :: Expr -> Expr
swizzleXY = _swizzle "xy" Vec2

swizzleYZ :: Expr -> Expr
swizzleYZ = _swizzle "yz" Vec2

swizzleZW :: Expr -> Expr
swizzleZW = _swizzle "zw" Vec2

swizzleXYZ :: Expr -> Expr
swizzleXYZ = _swizzle "xyz" Vec3

swizzleYZW :: Expr -> Expr
swizzleYZW = _swizzle "yzw" Vec3

swizzleXYY :: Expr -> Expr
swizzleXYY = _swizzle "xyy" Vec3

swizzleXYYY :: Expr -> Expr
swizzleXYYY = _swizzle "xyyy" Vec4

swizzleXYZZ :: Expr -> Expr
swizzleXYZZ = _swizzle "xyzz" Vec4


type Exprs = Multi Expr

