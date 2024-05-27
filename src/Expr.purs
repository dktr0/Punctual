module Expr where

import Prelude (class Eq, class Ord, class Show,($),(<>),show,(+),(-),(*),(/),(<<<),flip,(==),(/=),(>),(>=),(<),(<=),map,identity,otherwise,(&&))
import Prelude as Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Number as Number
import Data.List.NonEmpty (NonEmptyList,head,tail,fromList,concat,cons,singleton)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (unfoldr1)
import Data.Semigroup.Foldable (class Foldable1,foldl1)

import Channels
import Number as Number

class Channels a <= Expr a where
  constant :: Number -> a
  expr :: String -> a
  isConstant :: a -> Boolean
  toExpr :: a -> String
  unaryFunction :: (Number -> Number) -> (String -> String) -> a -> a
  binaryFunction :: (Number -> Number -> Number) -> (String -> String -> String) -> a -> a -> a
  showType :: a -> String
  fromFloat :: Float -> a
  fromFloats :: NonEmptyList Float -> NonEmptyList a
  fromVec2s :: NonEmptyList Vec2 -> NonEmptyList a
  fromVec3s :: NonEmptyList Vec3 -> NonEmptyList a
  fromVec4s :: NonEmptyList Vec4 -> NonEmptyList a
  toFloats :: NonEmptyList a -> NonEmptyList Float
  toVec2s :: NonEmptyList a -> NonEmptyList Vec2
  toVec3s :: NonEmptyList a -> NonEmptyList Vec3
  toVec4s :: NonEmptyList a -> NonEmptyList Vec4  
  dotSum :: a -> Float

zero :: forall a. Expr a => a
zero = constant 0.0
  
mapConstant :: forall a. Expr a => (Number -> Number) -> a -> a
mapConstant f x = unaryFunction f identity x

mapString :: forall a. Expr a => (String -> String) -> a -> a
mapString f x = unaryFunction identity f x


data Float =
  FloatConstant Number |
  FloatExpr String

instance Expr Float where
  constant = FloatConstant
  expr = FloatExpr
  isConstant (FloatConstant _) = true
  isConstant (FloatExpr _) = false
  toExpr (FloatConstant x) = show x
  toExpr (FloatExpr x) = x
  unaryFunction f _ (FloatConstant x) = FloatConstant (f x)
  unaryFunction _ f (FloatExpr x) = FloatExpr (f x)
  binaryFunction f _ (FloatConstant x) (FloatConstant y) = FloatConstant (f x y)
  binaryFunction _ f x y = FloatExpr (f (toExpr x) (toExpr y))
  showType _ = "float"
  fromFloat = identity
  fromFloats = identity
  fromVec2s = vec2sToFloats
  fromVec3s = vec3sToFloats
  fromVec4s = vec4sToFloats
  toFloats = identity
  toVec2s = floatsToVec2s
  toVec3s = floatsToVec3s
  toVec4s = floatsToVec4s
  dotSum = identity
  
instance Channels Float where channels _ = 1

  
data Vec2 =
  Vec2Constant Number Number |
  Vec2Expr String

instance Expr Vec2 where
  constant x = Vec2Constant x x
  expr = Vec2Expr
  isConstant (Vec2Constant _ _) = true
  isConstant (Vec2Expr _) = false
  toExpr (Vec2Constant x y) = "vec2(" <> show x <> "," <> show y <> ")"
  toExpr (Vec2Expr x) = x
  unaryFunction f _ (Vec2Constant x y) = Vec2Constant (f x) (f y)
  unaryFunction _ f (Vec2Expr x) = Vec2Expr (f x)
  binaryFunction f _ (Vec2Constant x1 x2) (Vec2Constant y1 y2) = Vec2Constant (f x1 y1) (f x2 y2)
  binaryFunction _ f x y = Vec2Expr (f (toExpr x) (toExpr y))
  showType _ = "vec2"
  fromFloat (FloatConstant x) = Vec2Constant x x
  fromFloat (FloatExpr x) = Vec2Expr $ "vec2(" <> x <> ")"
  fromFloats = floatsToVec2s -- concat <<< unfoldr1 unconsFloatsToVec2s
  fromVec2s = identity
  fromVec3s = vec3sToVec2s
  fromVec4s = vec4sToVec2s
  toFloats = vec2sToFloats
  toVec2s = identity
  toVec3s = vec2sToVec3s
  toVec4s = vec2sToVec4s
  dotSum (Vec2Constant x y) = FloatConstant (x+y)
  dotSum x = FloatExpr $ "dot(" <> toExpr x <> ",vec2(1.))"
  
instance Channels Vec2 where channels _ = 2


data Vec3 =
  Vec3Constant Number Number Number |
  Vec3Expr String

instance Expr Vec3 where
  constant x = Vec3Constant x x x
  expr = Vec3Expr
  isConstant (Vec3Constant _ _ _) = true
  isConstant (Vec3Expr _) = false
  toExpr (Vec3Constant x y z) = "vec3(" <> show x <> "," <> show y <> "," <> show z <> ")"
  toExpr (Vec3Expr x) = x
  unaryFunction f _ (Vec3Constant x y z) = Vec3Constant (f x) (f y) (f z)
  unaryFunction _ f (Vec3Expr x) = Vec3Expr (f x)
  binaryFunction f _ (Vec3Constant x1 x2 x3) (Vec3Constant y1 y2 y3) = Vec3Constant (f x1 y1) (f x2 y2) (f x3 y3)
  binaryFunction _ f x y = Vec3Expr (f (toExpr x) (toExpr y))
  showType _ = "vec3"
  fromFloat (FloatConstant x) = Vec3Constant x x x
  fromFloat (FloatExpr x) = Vec3Expr $ "vec3(" <> x <> ")"
  fromFloats = floatsToVec3s
  fromVec2s = vec2sToVec3s
  fromVec3s = identity
  fromVec4s = vec4sToVec3s
  toFloats = vec3sToFloats
  toVec2s = vec3sToVec2s
  toVec3s = identity
  toVec4s = vec3sToVec4s
  dotSum (Vec3Constant x y z) = FloatConstant (x+y+z)
  dotSum x = FloatExpr $ "dot(" <> toExpr x <> ",vec3(1.))"

instance Channels Vec3 where channels _ = 3


data Vec4 =
  Vec4Constant Number Number Number Number |
  Vec4Expr String

instance Expr Vec4 where
  constant x = Vec4Constant x x x x
  expr = Vec4Expr
  isConstant (Vec4Constant _ _ _ _) = true
  isConstant (Vec4Expr _) = false
  toExpr (Vec4Constant x y z w) = "vec2(" <> show x <> "," <> show y <> "," <> show z <> "," <> show w <> ")"
  toExpr (Vec4Expr x) = x
  unaryFunction f _ (Vec4Constant x y z w) = Vec4Constant (f x) (f y) (f z) (f w)
  unaryFunction _ f (Vec4Expr x) = Vec4Expr (f x)
  binaryFunction f _ (Vec4Constant x1 x2 x3 x4) (Vec4Constant y1 y2 y3 y4) = Vec4Constant (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4)
  binaryFunction _ f x y = Vec4Expr (f (toExpr x) (toExpr y))
  showType _ = "vec4"
  fromFloat (FloatConstant x) = Vec4Constant x x x x
  fromFloat (FloatExpr x) = Vec4Expr $ "vec4(" <> x <> ")"
  fromFloats = floatsToVec4s -- concat <<< unfoldr1 unconsFloatsToVec4s
  fromVec2s = vec2sToVec4s -- concat <<< unfoldr1 unconsVec2sToVec4s
  fromVec3s = vec3sToVec4s -- concat <<< unfoldr1 unconsVec3sToVec4s
  fromVec4s = identity
  toFloats = vec4sToFloats
  toVec2s = vec4sToVec2s
  toVec3s = vec4sToVec3s
  toVec4s = identity
  dotSum (Vec4Constant x y z w) = FloatConstant (x+y+z+w)
  dotSum x = FloatExpr $ "dot(" <> toExpr x <> ",vec4(1.))"

instance Channels Vec4 where channels _ = 1

floatsToVec2s :: NonEmptyList Float -> NonEmptyList Vec2
floatsToVec2s = concat <<< unfoldr1 unconsFloatsToVec2s

floatsToVec3s :: NonEmptyList Float -> NonEmptyList Vec3
floatsToVec3s = concat <<< unfoldr1 unconsFloatsToVec3s

floatsToVec4s :: NonEmptyList Float -> NonEmptyList Vec4
floatsToVec4s = concat <<< unfoldr1 unconsFloatsToVec4s

vec2sToFloats :: NonEmptyList Vec2 -> NonEmptyList Float
vec2sToFloats = concat <<< map (\a -> swizzleX a `cons` singleton (swizzleY a))

vec2sToVec3s :: NonEmptyList Vec2 -> NonEmptyList Vec3
vec2sToVec3s = concat <<< unfoldr1 unconsVec2sToVec3s

vec2sToVec4s :: NonEmptyList Vec2 -> NonEmptyList Vec4
vec2sToVec4s = concat <<< unfoldr1 unconsVec2sToVec4s

vec3sToFloats :: NonEmptyList Vec3 -> NonEmptyList Float
vec3sToFloats = concat <<< map (\a -> swizzleX a `cons` (swizzleY a `cons` singleton (swizzleZ a)))

vec3sToVec2s :: NonEmptyList Vec3 -> NonEmptyList Vec2
vec3sToVec2s = concat <<< unfoldr1 unconsVec3sToVec2s

vec3sToVec4s :: NonEmptyList Vec3 -> NonEmptyList Vec4
vec3sToVec4s = concat <<< unfoldr1 unconsVec3sToVec4s

vec4sToFloats :: NonEmptyList Vec4 -> NonEmptyList Float
vec4sToFloats = concat <<< map (\a -> swizzleX a `cons` (swizzleY a `cons` (swizzleZ a `cons` singleton (swizzleZ a))))

vec4sToVec2s :: NonEmptyList Vec4 -> NonEmptyList Vec2
vec4sToVec2s = concat <<< map (\a -> swizzleXY a `cons` singleton (swizzleZW a))

vec4sToVec3s :: NonEmptyList Vec4 -> NonEmptyList Vec3
vec4sToVec3s = concat <<< unfoldr1 unconsVec4sToVec3s


-- Construction of Expr types by composition of smaller types

floatFloatToVec2 :: Float -> Float -> Vec2
floatFloatToVec2 (FloatConstant x) (FloatConstant y) = Vec2Constant x y
floatFloatToVec2 x y = Vec2Expr $ "vec2(" <> toExpr x <> "," <> toExpr y <> ")"

vec2FloatToVec3 :: Vec2 -> Float -> Vec3
vec2FloatToVec3 (Vec2Constant x y) (FloatConstant z) = Vec3Constant x y z
vec2FloatToVec3 x y = Vec3Expr $ "vec3(" <> toExpr x <> "," <> toExpr y <> ")"

floatVec2ToVec3 :: Float -> Vec2 -> Vec3
floatVec2ToVec3 (FloatConstant x) (Vec2Constant y z) = Vec3Constant x y z
floatVec2ToVec3 x y = Vec3Expr $ "vec3(" <> toExpr x <> "," <> toExpr y <> ")"

floatFloatFloatToVec3 :: Float -> Float -> Float -> Vec3
floatFloatFloatToVec3 (FloatConstant x) (FloatConstant y) (FloatConstant z) = Vec3Constant x y z
floatFloatFloatToVec3 x y z = Vec3Expr $ "vec3(" <> toExpr x <> "," <> toExpr y <> "," <> toExpr z <> ")"

floatFloatFloatFloatToVec4 :: Float -> Float -> Float -> Float -> Vec4
floatFloatFloatFloatToVec4 (FloatConstant w) (FloatConstant x) (FloatConstant y) (FloatConstant z) = Vec4Constant w x y z
floatFloatFloatFloatToVec4 w x y z = Vec4Expr $ "vec4(" <> toExpr w <> "," <> toExpr x <> "," <> toExpr y <> "," <> toExpr z <> ")"

vec2Vec2ToVec4 :: Vec2 -> Vec2 -> Vec4
vec2Vec2ToVec4 (Vec2Constant x y) (Vec2Constant z w) = Vec4Constant x y z w
vec2Vec2ToVec4 x y = Vec4Expr $ "vec4(" <> toExpr x <> "," <> toExpr y <> ")"

vec2FloatFloatToVec4 :: Vec2 -> Float -> Float -> Vec4
vec2FloatFloatToVec4 (Vec2Constant x y) (FloatConstant z) (FloatConstant w) = Vec4Constant x y z w
vec2FloatFloatToVec4 x y z = Vec4Expr $ "vec4(" <> toExpr x <> "," <> toExpr y <> "," <> toExpr z <> ")"

vec3FloatToVec4 :: Vec3 -> Float -> Vec4
vec3FloatToVec4 (Vec3Constant x y z) (FloatConstant w) = Vec4Constant x y z w
vec3FloatToVec4 x y = Vec4Expr $ "vec4(" <> toExpr x <> "," <> toExpr y <> ")"

floatVec3ToVec4 :: Float -> Vec3 -> Vec4
floatVec3ToVec4 (FloatConstant x) (Vec3Constant y z w) = Vec4Constant x y z w
floatVec3ToVec4 x y = Vec4Expr $ "vec4(" <> toExpr x <> "," <> toExpr y <> ")"


-- Construction of Expr types by uncons-ing lists of other Expr types

unconsFloatsToVec2s :: NonEmptyList Float -> Tuple (NonEmptyList Vec2) (Maybe (NonEmptyList Float))
unconsFloatsToVec2s xs = 
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ floatFloatToVec2 (head xs) zero) Nothing
    Just xs' -> Tuple (singleton $ floatFloatToVec2 (head xs) (head xs')) (fromList $ tail xs')
    
unconsFloatsToVec3s :: NonEmptyList Float -> Tuple (NonEmptyList Vec3) (Maybe (NonEmptyList Float))
unconsFloatsToVec3s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ floatFloatFloatToVec3 (head xs) zero zero) Nothing
    Just xs' -> 
      case fromList (tail xs') of
        Nothing -> Tuple (singleton $ floatFloatFloatToVec3 (head xs) (head xs') zero) Nothing
        Just xs'' -> Tuple (singleton $ floatFloatFloatToVec3 (head xs) (head xs') (head xs'')) (fromList $ tail xs'')
        
unconsFloatsToVec4s :: NonEmptyList Float -> Tuple (NonEmptyList Vec4) (Maybe (NonEmptyList Float))
unconsFloatsToVec4s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ floatFloatFloatFloatToVec4 (head xs) zero zero zero) Nothing
    Just xs' -> 
      case fromList (tail xs') of
        Nothing -> Tuple (singleton $ floatFloatFloatFloatToVec4 (head xs) (head xs') zero zero) Nothing
        Just xs'' ->
          case fromList (tail xs'') of
            Nothing -> Tuple (singleton $ floatFloatFloatFloatToVec4 (head xs) (head xs') (head xs'') zero) Nothing
            Just xs''' -> Tuple (singleton $ floatFloatFloatFloatToVec4 (head xs) (head xs') (head xs'') (head xs''')) (fromList $ tail xs''')

unconsVec3sToVec2s :: NonEmptyList Vec3 -> Tuple (NonEmptyList Vec2) (Maybe (NonEmptyList Vec3))
unconsVec3sToVec2s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (swizzleXY (head xs) `cons` singleton (floatFloatToVec2 (swizzleZ $ head xs) zero)) Nothing -- one vec3 makes two vec2s
    Just xs' -> 
      let a = swizzleXY $ head xs
          b = floatFloatToVec2 (swizzleZ (head xs)) (swizzleX (head xs'))
          c = swizzleYZ $ head xs'
      in Tuple (a `cons` (b `cons` singleton c)) (fromList $ tail xs') -- two vec3s make three vec2s, evenly

unconsVec2sToVec3s :: NonEmptyList Vec2 -> Tuple (NonEmptyList Vec3) (Maybe (NonEmptyList Vec2))
unconsVec2sToVec3s xs = 
  case fromList (tail xs) of
    -- one vec2s makes one vec3 via swizzleXYY
    Nothing -> Tuple (singleton $ vec2FloatToVec3 (head xs) zero) Nothing
    Just xs' -> 
      case fromList (tail xs') of
        Nothing -> -- two vec2s makes two vec3s (second one is swizzleY00)
          let a = vec2FloatToVec3 (head xs) (swizzleX (head xs'))
              b = floatFloatFloatToVec3 (swizzleY $ head xs') zero zero
          in Tuple (a `cons` singleton b) Nothing
        Just xs'' -> -- three vec2s makes two vec3s evenly
          let a = vec2FloatToVec3 (head xs) (swizzleX (head xs'))
              b = floatVec2ToVec3 (swizzleY (head xs')) (head xs'')
          in Tuple (a `cons` singleton b) (fromList $ tail xs'')

unconsVec4sToVec3s :: NonEmptyList Vec4 -> Tuple (NonEmptyList Vec3) (Maybe (NonEmptyList Vec4))
unconsVec4sToVec3s xs =
  case fromList (tail xs) of
    -- one vec4 makes two vec3s
    Nothing -> Tuple (swizzleXYZ (head xs) `cons` singleton (floatFloatFloatToVec3 (swizzleW $ head xs) zero zero)) Nothing
    Just xs' ->
      case fromList (tail xs') of
        Nothing -> -- two vec4s makes three vec3s
          let a = swizzleXYZ (head xs)
              b = floatVec2ToVec3 (swizzleW $ head xs) (swizzleXY $ head xs')
              c = vec2FloatToVec3 (swizzleZW $ head xs') zero
          in Tuple (a `cons` (b `cons` singleton c)) Nothing
        Just xs'' -> -- three vec4s makes four vec3s evenly
          let a = swizzleXYZ (head xs) 
              b = floatVec2ToVec3 (swizzleW $ head xs) (swizzleXY $ head xs') 
              c = vec2FloatToVec3 (swizzleZW $ head xs') (swizzleX $ head xs'')
              d = swizzleYZW $ head xs''
          in Tuple (a `cons` (b `cons` (c `cons` singleton d))) (fromList $ tail xs'')


unconsVec2sToVec4s :: NonEmptyList Vec2 -> Tuple (NonEmptyList Vec4) (Maybe (NonEmptyList Vec2))
unconsVec2sToVec4s xs = 
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ vec2FloatFloatToVec4 (swizzleXY $ head xs) zero zero) Nothing
    Just xs' -> 
      let x = vec2Vec2ToVec4 (head xs) (head xs')
      in Tuple (singleton x) (fromList $ tail xs')

unconsVec3sToVec4s :: NonEmptyList Vec3 -> Tuple (NonEmptyList Vec4) (Maybe (NonEmptyList Vec3))
unconsVec3sToVec4s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ vec3FloatToVec4 (swizzleXYZ $ head xs) zero) Nothing
    Just xs' ->
      case fromList (tail xs') of
        Nothing ->
          let a = vec3FloatToVec4 (head xs) (swizzleX $ head xs') 
              b = vec2FloatFloatToVec4 (swizzleYZ $ head xs') zero zero
          in Tuple (a `cons` singleton b) Nothing
        Just xs'' ->
          case fromList (tail xs'') of
            Nothing ->
              let a = vec3FloatToVec4 (head xs) (swizzleX $ head xs')
                  b = vec2Vec2ToVec4 (swizzleYZ $ head xs') (swizzleXY $ head xs'')
                  c = floatFloatFloatFloatToVec4 (swizzleZ $ head xs'') zero zero zero
              in Tuple (a `cons` (b `cons` singleton c)) Nothing
            Just xs''' ->
              let a = vec3FloatToVec4 (head xs) (swizzleX $ head xs')
                  b = vec2Vec2ToVec4 (swizzleYZ $ head xs') (swizzleXY $ head xs'')
                  c = floatVec3ToVec4 (swizzleZ $ head xs'') (head xs''')
              in Tuple (a `cons` (b `cons` singleton c)) (fromList $ tail xs''')

        
-- Swizzling

_swizzleExpr :: forall a. Expr a => forall b. Expr b => String -> a -> b 
_swizzleExpr sMask a = expr $ toExpr a <> "." <> sMask 

class SwizzleX a where
  swizzleX :: a -> Float

instance SwizzleX Float where
  swizzleX = identity

instance SwizzleX Vec2 where
  swizzleX (Vec2Constant x _) = FloatConstant x
  swizzleX e = _swizzleExpr "x" e

instance SwizzleX Vec3 where
  swizzleX (Vec3Constant x _ _) = FloatConstant x
  swizzleX e = _swizzleExpr "x" e
  
instance SwizzleX Vec4 where
  swizzleX (Vec4Constant x _ _ _) = FloatConstant x
  swizzleX e = _swizzleExpr "x" e
  
class SwizzleY a where
  swizzleY :: a -> Float
  swizzleXY :: a -> Vec2
  swizzleXYY :: a -> Vec3
  swizzleYYY :: a -> Vec3
  swizzleXYYY :: a -> Vec4

instance SwizzleY Vec2 where
  swizzleY (Vec2Constant _ y) = FloatConstant y
  swizzleY e = _swizzleExpr "y" e
  swizzleXY = identity
  swizzleXYY (Vec2Constant x y) = Vec3Constant x y y
  swizzleXYY e = _swizzleExpr "xyy" e
  swizzleYYY (Vec2Constant _ y) = Vec3Constant y y y
  swizzleYYY e = _swizzleExpr "yyy" e
  swizzleXYYY (Vec2Constant x y) = Vec4Constant x y y y
  swizzleXYYY e = _swizzleExpr "xyyy" e

instance SwizzleY Vec3 where
  swizzleY (Vec3Constant _ y _) = FloatConstant y
  swizzleY e = _swizzleExpr "y" e
  swizzleXY (Vec3Constant x y _) = Vec2Constant x y
  swizzleXY e = _swizzleExpr "xy" e
  swizzleXYY (Vec3Constant x y _) = Vec3Constant x y y
  swizzleXYY e = _swizzleExpr "xyy" e
  swizzleYYY (Vec3Constant _ y _) = Vec3Constant y y y
  swizzleYYY e = _swizzleExpr "yyy" e
  swizzleXYYY (Vec3Constant x y _) = Vec4Constant x y y y
  swizzleXYYY e = _swizzleExpr "xyyy" e

instance SwizzleY Vec4 where
  swizzleY (Vec4Constant _ y _ _) = FloatConstant y
  swizzleY e = _swizzleExpr "y" e
  swizzleXY (Vec4Constant x y _ _) = Vec2Constant x y
  swizzleXY e = _swizzleExpr "xy" e
  swizzleXYY (Vec4Constant x y _ _) = Vec3Constant x y y
  swizzleXYY e = _swizzleExpr "xyy" e
  swizzleYYY (Vec4Constant _ y _ _) = Vec3Constant y y y
  swizzleYYY e = _swizzleExpr "yyy" e
  swizzleXYYY (Vec4Constant x y _ _) = Vec4Constant x y y y
  swizzleXYYY e = _swizzleExpr "xyyy" e

class SwizzleZ a where
  swizzleZ :: a -> Float
  swizzleYZ :: a -> Vec2
  swizzleZZ :: a -> Vec2
  swizzleXYZ :: a -> Vec3
  swizzleXYZZ :: a -> Vec4
  swizzleYZZZ :: a -> Vec4
  swizzleZZZZ :: a -> Vec4

instance SwizzleZ Vec3 where
  swizzleZ (Vec3Constant _ _ z) = FloatConstant z
  swizzleZ e = _swizzleExpr "z" e
  swizzleYZ (Vec3Constant _ y z) = Vec2Constant y z
  swizzleYZ e = _swizzleExpr "z" e
  swizzleZZ (Vec3Constant _ _ z) = Vec2Constant z z
  swizzleZZ e = _swizzleExpr "zz" e
  swizzleXYZ = identity
  swizzleXYZZ (Vec3Constant x y z) = Vec4Constant x y z z
  swizzleXYZZ e = _swizzleExpr "xyzz" e
  swizzleYZZZ (Vec3Constant _ y z) = Vec4Constant y z z z
  swizzleYZZZ e = _swizzleExpr "yzzz" e
  swizzleZZZZ (Vec3Constant _ _ z) = Vec4Constant z z z z
  swizzleZZZZ e = _swizzleExpr "zzzz" e

instance SwizzleZ Vec4 where
  swizzleZ (Vec4Constant _ _ z _) = FloatConstant z
  swizzleZ e = _swizzleExpr "z" e
  swizzleYZ (Vec4Constant _ y z _) = Vec2Constant y z
  swizzleYZ e = _swizzleExpr "yz" e
  swizzleZZ (Vec4Constant _ _ z _) = Vec2Constant z z
  swizzleZZ e = _swizzleExpr "zz" e
  swizzleXYZ (Vec4Constant x y z _) = Vec3Constant x y z
  swizzleXYZ e = _swizzleExpr "xyz" e
  swizzleXYZZ (Vec4Constant x y z _) = Vec4Constant x y z z
  swizzleXYZZ e = _swizzleExpr "xyzz" e
  swizzleYZZZ (Vec4Constant _ y z _) = Vec4Constant y z z z
  swizzleYZZZ e = _swizzleExpr "yzzz" e
  swizzleZZZZ (Vec4Constant _ _ z _) = Vec4Constant z z z z
  swizzleZZZZ e = _swizzleExpr "zzzz" e

-- any swizzle with a W only works on Vec4, so no typeclass
swizzleW :: Vec4 -> Float
swizzleW (Vec4Constant _ _ _ w) = FloatConstant w
swizzleW (Vec4Expr e) = FloatExpr $ e <> ".w"

swizzleZW :: Vec4 -> Vec2
swizzleZW (Vec4Constant _ _ z w) = Vec2Constant z w
swizzleZW (Vec4Expr e) = Vec2Expr $ e <> ".zw"

swizzleWWW :: Vec4 -> Vec3
swizzleWWW (Vec4Constant _ _ _ w) = Vec3Constant w w w
swizzleWWW (Vec4Expr e) = Vec3Expr $ e <> ".www"

swizzleZWW :: Vec4 -> Vec3
swizzleZWW (Vec4Constant _ _ z w) = Vec3Constant z w w
swizzleZWW (Vec4Expr e) = Vec3Expr $ e <> ".zww"

swizzleYZW :: Vec4 -> Vec3
swizzleYZW (Vec4Constant _ y z w) = Vec3Constant y z w
swizzleYZW (Vec4Expr e) = Vec3Expr $ e <> ".yzw"

-- convenience functions for constructing functions over the string components of expressions

function1 :: String -> String -> String
function1 funcName x = funcName <> "(" <> x <> ")"

function2 :: String -> String -> String -> String
function2 funcName x y = funcName <> "(" <> x <> "," <> y <> ")"

binOp :: String -> String -> String -> String
binOp op x y = "(" <> x <> op <> y <> ")"


-- unary functions 

sin :: forall a. Expr a => a -> a
sin = unaryFunction Number.sin (function1 "sin")

squared :: forall a. Expr a => a -> a
squared = unaryFunction (\x -> x * x) (\x -> binOp "*" x x)

cos :: forall a. Expr a => a -> a
cos = unaryFunction Number.cos (function1 "cos")

abs :: forall a. Expr a => a -> a
abs = unaryFunction Number.abs (function1 "abs")

acos :: forall a. Expr a => a -> a
acos = unaryFunction Number.acos (function1 "acos")

ampdb :: forall a. Expr a => a -> a
ampdb = (flip divisionExprFloat) (FloatConstant 10.0) <<< productFloatExpr (FloatConstant 20.0) <<< log

log :: forall a. Expr a => a -> a
log = unaryFunction Number.log (function1 "log")

log2 :: forall a. Expr a => a -> a
log2 = unaryFunction Number.log2 (function1 "log2")

log10 :: forall a. Expr a => a -> a
log10 = unaryFunction Number.log10 (function1 "log10")

asin :: forall a. Expr a => a -> a
asin = unaryFunction Number.asin (function1 "asin")

atan :: forall a. Expr a => a -> a
atan = unaryFunction Number.atan (function1 "atan")

ceil :: forall a. Expr a => a -> a
ceil = unaryFunction Number.ceil (function1 "ceil")

cpsmidi :: forall a. Expr a => a -> a
cpsmidi = addFloatExpr (constant 69.0) <<< productFloatExpr (constant 12.0) <<< log2 <<< flip divisionExprFloat (constant 440.0)

dbamp :: forall a. Expr a => a -> a
dbamp = pow (constant 10.0) <<< flip divisionExprFloat (constant 20.0)

exp :: forall a. Expr a => a -> a
exp = unaryFunction Number.exp (function1 "exp")

floor :: forall a. Expr a => a -> a
floor = unaryFunction Number.floor (function1 "floor")

midicps :: forall a. Expr a => a -> a
midicps m = product (pow (division (difference m (constant 69.0)) (constant 12.0)) (constant 2.0)) (constant 440.0)

sign :: forall a. Expr a => a -> a
sign = unaryFunction Number.sign (function1 "sign")

sqrt :: forall a. Expr a => a -> a
sqrt = unaryFunction Number.sqrt (function1 "sqrt")

tan :: forall a. Expr a => a -> a
tan = unaryFunction Number.tan (function1 "tan")

fract :: forall a. Expr a => a -> a
fract = unaryFunction (\x -> Prelude.mod x 1.0) (function1 "fract")


-- Arithmetic operations

arithmeticOperator :: forall a. Expr a => (Number -> Number -> Number) -> String -> a -> a -> a
arithmeticOperator f op = binaryFunction f (binOp op)

add :: forall a. Expr a => a -> a -> a
add = arithmeticOperator (+) "+"

addFloatExpr :: forall b. Expr b => Float -> b -> b
addFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a + b') (\b' -> binOp "+" (show a) b') b
addFloatExpr (FloatExpr a) b = expr $ binOp "+" a (toExpr b)

addExprFloat :: forall a. Expr a => a -> Float -> a
addExprFloat = flip addFloatExpr

difference :: forall a. Expr a => a -> a -> a
difference = arithmeticOperator (-) "-"

differenceFloatExpr :: forall b. Expr b => Float -> b -> b
differenceFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a - b') (\b' -> binOp "-" (show a) b') b
differenceFloatExpr (FloatExpr a) b = expr $ binOp "-" a (toExpr b)

differenceExprFloat :: forall b. Expr b => b -> Float -> b
differenceExprFloat a (FloatConstant b) = unaryFunction (\a' -> a' - b) (\a' -> binOp "-" a' (show b)) a
differenceExprFloat a (FloatExpr b) = expr $ binOp "-" (toExpr a) b

product :: forall a. Expr a => a -> a -> a
product = arithmeticOperator (*) "*"

productFloatExpr :: forall b. Expr b => Float -> b -> b
productFloatExpr (FloatConstant a) b = unaryFunction (\x -> a * x) (\x -> binOp "*" (show a) x) b
productFloatExpr (FloatExpr a) b = expr $ binOp "*" a (toExpr b)

productExprFloat :: forall a. Expr a => a -> Float -> a
productExprFloat = flip productFloatExpr

division :: forall a. Expr a => a -> a -> a
division = arithmeticOperator (/) "/" -- TODO: this should be safe division to match the audio side!

-- TODO: this should be safe division to match the audio side!
divisionFloatExpr :: forall b. Expr b => Float -> b -> b
divisionFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a / b') (\b' -> binOp "/" (show a) b') b
divisionFloatExpr (FloatExpr a) b = expr $ binOp "/" a (toExpr b)

-- TODO: this should be safe division to match the audio side!
divisionExprFloat :: forall b. Expr b => b -> Float -> b
divisionExprFloat a (FloatConstant b) = unaryFunction (\a' -> a' / b) (\a' -> binOp "/" a' (show b)) a
divisionExprFloat a (FloatExpr b) = expr $ binOp "/" (toExpr a) b


min :: forall a. Expr a => a -> a -> a
min = binaryFunction Prelude.min (function2 "min")

max :: forall a. Expr a => a -> a -> a
max = binaryFunction Prelude.max (function2 "max")

-- maybe TODO later if some advantage to it appears: make variants for mismatched types where one argument is a float


pow :: forall a. Expr a => a -> a -> a
pow = binaryFunction Number.pow (function2 "pow")

mod :: forall a. Expr a => a -> a -> a
mod = binaryFunction Prelude.mod (function2 "mod")


comparisonOperator :: forall a. Expr a => (Number -> Number -> Boolean) -> String -> String -> a -> a -> a
comparisonOperator f funcName op x y 
  | channels x == 1 = binaryFunction (\a b -> booleanToNumber $ f a b) (\a b -> function1 (showType x) $ binOp op a b) x y
  | otherwise = binaryFunction (\a b -> booleanToNumber $ f a b) (\a b -> function1 (showType x) $ function2 funcName a b) x y

booleanToNumber :: Boolean -> Number
booleanToNumber true = 1.0
booleanToNumber false = 0.0

equal :: forall a. Expr a => a -> a -> a
equal = comparisonOperator (==) "equal" "==" 

notEqual :: forall a. Expr a => a -> a -> a
notEqual = comparisonOperator (/=) "notEqual" "!="

greaterThan :: forall a. Expr a => a -> a -> a
greaterThan = comparisonOperator (>) "greaterThan" ">"

greaterThanEqual :: forall a. Expr a => a -> a -> a
greaterThanEqual = comparisonOperator (>=) "greaterThanEqual" ">=" 

lessThan :: forall a. Expr a => a -> a -> a
lessThan = comparisonOperator (<) "lessThan" "<"

lessThanEqual ::forall a. Expr a => a -> a -> a
lessThanEqual = comparisonOperator (<=) "lessThanEqual" "<="


{-

-- miscellaneous functions over Expr:

-- caller should assign second argument because of multiple use in this definition
gate :: Expr -> Expr -> Expr
gate x y = product (lessThan x y) y

-- all arguments are multiply used and should be assigned by the caller
-- expectation is that first two arguments are floats (constant or otherwise), unsure what results would be if this is violated
between :: Expr -> Expr -> Expr -> Expr
between r1 r2 x = product (lessThan (min r1 r2) x) (difference (constant 1.0) (lessThan (max r1 r2) x))

-- expectation is that first two arguments are floats (constant or otherwise), unsure what results would be if this is violated
-- (no multiple use of arguments)
smoothStep :: Expr -> Expr -> Expr -> Expr
smoothStep (constant e0) (constant e1) (constant x) = constant $ Number.smoothStep e0 e1 x
smoothStep e0 e1 (constant x) = Reference Float $ "smoothstep(" <> show e0 <> "," <> show e1 <> "," <> show x <> ")"
smoothStep e0 e1 (Reference xType x) = Reference xType $ "smoothstep(" <> show e0 <> "," <> show e1 <> "," <> show x <> ")"
-}


clip :: forall a. Expr a => Float -> Float -> a -> a
clip e0 e1 x = clamp (min e0 e1) (max e0 e1) x

-- clamp is an "unchecked" clipping operation (weird results if min and max are out of order)
clamp :: forall a. Expr a => Float -> Float -> a -> a
clamp e0 e1 x = expr $ "clamp(" <> toExpr x <> "," <> toExpr e0 <> "," <> toExpr e1 <> ")"

{-
circle :: forall a. ConstantOrExpr a => Vec2 -> Vec2 -> a -> a
circle fxy xy diameter = smoothstep e0 e1 $ distance fxy xy - (diameter * constant 0.5) -- what about potential type mismatch in multiplication?
  where
    e0 = Vec2Expr "1.5/(res.x+res.y)"
    e1 = constant 0.0
-}

distance :: forall a. Expr a => a -> a -> Float
distance a b
  | isConstant a && isConstant b = sqrt $ dotSum $ squared $ difference a b
  | otherwise = expr $ "distance(" <> toExpr a <> "," <> toExpr b <> ")"
  
prox :: Vec2 -> Vec2 -> Float
prox a b = clamp (constant 0.0) (constant 1.0) $ division (difference (constant 2.828427) (distance a b)) (constant 2.828427)

{-
point :: GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2
point fxy xy = circle fxy xy d
  where d = { string: "((1./res.x)+(1./res.y))", glslType: Float, isSimple: false, deps: empty }
 
      
vline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
vline fxy x w 
  | x.glslType /= Float && w.glslType /= Float && x.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in vline", glslType: Float, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max x.glslType w.glslType, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".x-" <> x.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./res.x)"
        s = "(1.-smoothstep(" <> edge0.string <> "," <> edge1 <> "," <> a <> "))"

hline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
hline fxy y w 
  | y.glslType /= Float && w.glslType /= Float && y.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in hline", glslType: Float, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max y.glslType w.glslType, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".y-" <> y.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./res.y)"
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

mix :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
mix x y a
  | x.glslType /= y.glslType = { string: "!! Internal Punctual GLSL generation error in mix", glslType: Float, isSimple: false, deps: x.deps <> y.deps <> a.deps }
  | x.glslType /= a.glslType && a.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in mix", glslType: Float, isSimple: false, deps: x.deps <> y.deps <> a.deps }
  | otherwise = { string: s, glslType: t, isSimple: false, deps: x.deps <> y.deps <> a.deps }
      where
        s = "mix(" <> x.string <> "," <> y.string <> "," <> a.string <> ")"
        t = Prelude.max x.glslType a.glslType

seq :: NonEmptyList GLSLExpr -> GLSLExpr -> GLSLExpr -- all arguments are Float and return value is Float
seq steps y = { string: s, glslType: Float, isSimple: false, deps: fold (map _.deps steps) <> y.deps }
  where
    nSteps = length steps
    stepSize = 1.0 / toNumber nSteps
    firstStep = "(step(" <> y.string <> "," <> show stepSize <> ")*" <> (head steps).string <> ")"
    lastStep = "(step(" <> show (1.0-stepSize) <> "," <> y.string <> ")*" <> (last steps).string <> ")"
    middleStep n r = "((step(" <> show (stepSize*toNumber n) <> "," <> y.string <> ")-step(" <> show (stepSize*toNumber n + stepSize) <> "," <> y.string  <> "))*" <> r <> ")"
    middleStepExprs = case List.init (tail steps) of
                        Just xs -> xs
                        _ -> List.Nil
    middleStepNumbers = toList $ range 1 (nSteps - 2)
    middleStepValues = map _.string middleStepExprs
    middleSteps = List.zipWith middleStep middleStepNumbers middleStepValues
    s = intercalate "+" ((firstStep : middleSteps) `List.snoc` lastStep)
-}


fadeIn :: Number -> Number -> Float 
fadeIn t1 t2 = expr $ "clamp((_etime-" <> show t1 <> ")/(" <> show t2 <> "-" <> show t1 <> "),0.,1.)"

fadeOut :: Number -> Number -> Float
fadeOut t1 t2 = expr $ "clamp((" <> show t2 <> "-_etime)/(" <> show t2 <> "-" <> show t1 <> "),0.,1.)"


bipolar :: forall a. Expr a => a -> a
bipolar x = differenceExprFloat (productExprFloat x (constant 2.0)) (constant 1.0)

unipolar :: forall a. Expr a => a -> a
unipolar x = addExprFloat (productExprFloat x (constant 0.5)) (constant 0.5)

tile :: forall a. Expr a => a -> a -> a
tile fxy ab = bipolar $ fract $ product (unipolar fxy) ab

pi :: Float
pi = expr "PI"

px :: Float
px = expr "(2./res.x)"

py :: Float
py = expr "(2./res.y)"

pxy :: Vec2
pxy = expr "(2./res)"

aspect :: Float
aspect = expr "(res.x/res.y)"


sum :: forall f. Foldable1 f => forall a. Expr a => f a -> a
sum = foldl1 add 

textureFFT :: String -> Vec2 -> Float
textureFFT texName xy = FloatExpr $ "texture2D(" <> texName <> ",vec2(" <> toExpr xy <> ".x,0.)).x"

texture2D :: String -> Vec2 -> Vec3
texture2D texName xy = Vec3Expr $ "texture2D(" <> texName <> "," <> toExpr xy <> ").xyz"

blend :: Vec4 -> Vec4 -> Vec4
blend a b = Vec4Expr $ "mix(" <> toExpr a <> "," <> toExpr b <> "," <> toExpr (swizzleW b) <> ")"

rgbhsv :: Vec3 -> Vec3
rgbhsv x = Vec3Expr $ "rgbhsv(" <> toExpr x <> ")"

hsvrgb :: Vec3 -> Vec3
hsvrgb x = Vec3Expr $ "hsvrgb(" <> toExpr x <> ")"

cbrt :: forall a. Expr a => a -> a
cbrt = unaryFunction Number.cbrt f
  where f x = "(exp(log(abs(" <> x <> "))/3.)*sign(" <> x <> "))"

rtxy :: Vec2 -> Vec2
rtxy rt = floatFloatToVec2 x y   
  where
    r = swizzleX rt
    t = swizzleY rt
    x = product r (cos t)
    y = product r (sin t)

rtx :: Vec2 -> Float
rtx rt = product r (cos t) 
  where
    r = swizzleX rt
    t = swizzleY rt

rty :: Vec2 -> Float
rty rt = product r (sin t)
  where
    r = swizzleX rt
    t = swizzleY rt

xyrt :: Vec2 -> Vec2
xyrt xy = floatFloatToVec2 r t
  where
    x = swizzleX xy
    y = swizzleY xy
    r = sqrt (add (product x x) (product y y))
    t = atan (division y x)

xyr :: Vec2 -> Float
xyr xy = sqrt (add (product x x) (product y y)) 
  where
    x = swizzleX xy
    y = swizzleY xy

xyt :: Vec2 -> Float
xyt xy = atan (division y x)
  where
    x = swizzleX xy
    y = swizzleY xy

