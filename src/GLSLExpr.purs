module GLSLExpr where

-- This module provides types and functions to represent and manipulate the text of
-- strongly-typed GLSL expressions (with Purescript-compile-time type checking).

import Prelude (class Eq,class Show,(+),(<>),(==),otherwise,($),(&&))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable (class Foldable,foldl)

-- strictly typed representations
data Float = Float Boolean String
data Vec2 = Vec2 Boolean String
data Vec3 = Vec3 Boolean String
data Vec4 = Vec4 Boolean String

-- loosely typed representations
data GLSLExpr =
  FloatExpr Float |
  Vec2Expr Vec2 |
  Vec3Expr Vec3 |
  Vec4Expr Vec4

class GLSLType e where
  isSimple :: e -> Boolean
  string :: e -> String
  channels :: e -> Int
  create :: Boolean -> String -> e

-- classes to mark types that can be safely and simply cast to Vec2, 3, or 4
class GLSLType e <= ToVec2 e
class GLSLType e <= ToVec3 e
class GLSLType e <= ToVec4 e

toVec2 :: forall e. ToVec2 e => e -> Vec2
toVec2 e = Vec2 (isSimple e) ("vec2(" <> string e <> ")")

toVec3 :: forall e. ToVec3 e => e -> Vec3
toVec3 e = Vec3 (isSimple e) ("vec3(" <> string e <> ")")

toVec4 :: forall e. ToVec4 e => e -> Vec4
toVec4 e = Vec4 (isSimple e) ("vec4(" <> string e <> ")")


-- additional classes can be declared for conditions characteristic of GLSL's slightly-strict-slightly-loose typing
-- such as something that can be either a Float or a Vec4, things that can be any of the types, etc, then these
-- classes can be constraints on polymorphic functions we use to assemble fragment shader expressions.
-- (we'll still need some kind of type that unifies the four types Float,Vec2,Vec3,Vec4 though so we can have heterogenous collections.)

class GLSLType e <= FloatVec4 e
class GLSLType e <= Vec2Vec3Vec4 e
class GLSLType e <= Vec3Vec4 e

derive instance Eq Float
derive instance Generic Float _
instance Show Float where
  show = genericShow
instance GLSLType Float where
  isSimple (Float x _) = x
  string (Float _ x) = x
  channels (Float _ _) = 1
  create = Float
instance FloatVec4 Float

derive instance Eq Vec2
derive instance Generic Vec2 _
instance Show Vec2 where
  show = genericShow
instance GLSLType Vec2 where
  isSimple (Vec2 x _) = x
  string (Vec2 _ x) = x
  channels (Vec2 _ _) = 2
  create = Vec2
instance Vec2Vec3Vec4 Vec2

derive instance Eq Vec3
derive instance Generic Vec3 _
instance Show Vec3 where
  show = genericShow
instance GLSLType Vec3 where
  isSimple (Vec3 x _) = x
  string (Vec3 _ x) = x
  channels (Vec3 _ _) = 3
  create = Vec3
instance Vec2Vec3Vec4 Vec3
instance Vec3Vec4 Vec3

derive instance Eq Vec4
derive instance Generic Vec4 _
instance Show Vec4 where
  show = genericShow
instance GLSLType Vec4 where
  isSimple (Vec4 x _) = x
  string (Vec4 _ x) = x
  channels (Vec4 _ _) = 4
  create = Vec4
instance FloatVec4 Vec4
instance Vec2Vec3Vec4 Vec4
instance Vec3Vec4 Vec4

exprsChannels :: forall f g. Foldable f => GLSLType g => f g -> Int
exprsChannels = foldl (\n g -> n + channels g) 0


-- the isSimple flag is true only if the input expression is simple and the function is deemed simple
unaryFunction :: forall a b. GLSLType a => GLSLType b => String -> Boolean -> a -> b
unaryFunction func flag e = create (isSimple e && flag) $ func <> "(" <> string e <> ")"

binaryFunction :: forall a b c. GLSLType a => GLSLType b => GLSLType c => String -> a -> b -> c
binaryFunction func x y = create false $ func <> "(" <> string x <> "," <> string y <> ")"

ternaryFunction :: forall a b c d. GLSLType a => GLSLType b => GLSLType c => GLSLType d => String -> a -> b -> c -> d
ternaryFunction func x y z = create false $ func <> "(" <> string x <> "," <> string y <> "," <> string z <> ")"


-- (will need to remember that this works differently for comparison operators, which do
-- not require a cast from Float to be used in conjunction wih non-Float types, and which
-- have versions that are named functions instead of operators.)
binaryOperator :: forall a b c. GLSLType a => GLSLType b => GLSLType c => String -> a -> b -> c
binaryOperator op x y = create false $ "(" <> string x <> op <> string y <> ")"


swizzleX :: forall e. GLSLType e => e -> Float
swizzleX e
  | channels e == 1 = Float (isSimple e) (string e)
  | otherwise = Float (isSimple e) (string e <> ".x")

swizzleY :: forall e. Vec2Vec3Vec4 e => e -> Float
swizzleY e = Float (isSimple e) (string e <> ".y")

swizzleZ :: forall e. Vec3Vec4 e => e -> Float
swizzleZ e = Float (isSimple e) (string e <> ".z")

swizzleW :: Vec4 -> Float
swizzleW e = Float (isSimple e) (string e <> ".w")

swizzleXY :: forall e. Vec2Vec3Vec4 e => e -> Vec2
swizzleXY e
  | channels e == 2 = Vec2 (isSimple e) (string e)
  | otherwise = Vec2 (isSimple e) (string e <> ".xy")

swizzleYZ :: forall e. Vec3Vec4 e => e -> Vec2
swizzleYZ e = Vec2 (isSimple e) (string e <> ".yz")

swizzleZW :: Vec4 -> Vec2
swizzleZW e = Vec2 (isSimple e) (string e <> ".zw")

swizzleXYZ :: forall e. Vec3Vec4 e => e -> Vec3
swizzleXYZ e
  | channels e == 3 = Vec3 (isSimple e) (string e)
  | otherwise = Vec3 (isSimple e) (string e <> ".xyz")

swizzleYZW :: Vec4 -> Vec3
swizzleYZW e = Vec3 (isSimple e) (string e <> ".yzw")

swizzleXYY :: forall e. Vec2Vec3Vec4 e => e -> Vec3
swizzleXYY e = Vec3 (isSimple e) (string e <> ".xyy")

swizzleXYYY :: forall e. Vec2Vec3Vec4 e => e -> Vec4
swizzleXYYY e = Vec4 (isSimple e) (string e <> ".xyyy")

swizzleXYZZ :: forall e. Vec3Vec4 e => e -> Vec4
swizzleXYZZ e = Vec4 (isSimple e) (string e <> ".xyzz")
