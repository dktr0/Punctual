module Expr where

import Prelude (class Eq, class Ord, class Show)
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

type Exprs = Multi Expr

