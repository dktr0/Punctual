module Expr where

import Prelude (class Eq, class Ord, class Show,($),(<>),show,(+),(-),(*),(/),(<<<),flip,(==),(/=),(>),(>=),(<),(<=),map,identity,otherwise,(&&))
import Prelude as Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Number as Number
import Data.Int (toNumber)
import Data.List.NonEmpty (NonEmptyList,head,tail,fromList,concat,cons,singleton,length,zipWith)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (unfoldr1,range)
import Data.Semigroup.Foldable (class Foldable1,foldl1)

import Channels
import Number as Number

class Channels a <= Expr a where
  constant :: Number -> a
  expr :: String -> a
  isConstant :: a -> Boolean
  toExpr :: a -> String
  toExprSafe :: a -> String -- will ensure constants are wrapped in brackets as necessary for free combination in expressions
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

castExprs :: forall a b. Expr a => Expr b => NonEmptyList a -> NonEmptyList b
castExprs xs
  | channels (head xs) == 1 = fromFloats (toFloats xs)
  | channels (head xs) == 2 = fromVec2s (toVec2s xs)
  | channels (head xs) == 3 = fromVec3s (toVec3s xs)
  | otherwise = fromVec4s (toVec4s xs)

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
  toExprSafe (FloatConstant x) = showNumber x
  toExprSafe (FloatExpr x) = x
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
  toExprSafe x = toExpr x
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
  toExprSafe x = toExpr x
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
  toExpr (Vec4Constant x y z w) = "vec4(" <> show x <> "," <> show y <> "," <> show z <> "," <> show w <> ")"
  toExpr (Vec4Expr x) = x
  toExprSafe x = toExpr x
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


-- wraps negative numbers in brackets, to be used where necessary
showNumber :: Number -> String
showNumber x
  | x < 0.0 = "(" <> show x <> ")"
  | otherwise = show x


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

operatorFloatExpr :: forall a. Expr a => (Number -> Number -> Number) -> String -> Float -> a -> a
operatorFloatExpr f op (FloatConstant x) y = unaryFunction (f x) (\y' -> "(" <> showNumber x <> op <> y' <> ")") y
operatorFloatExpr _ op (FloatExpr x) y = expr $ "(" <> x <> op <> toExprSafe y <> ")"
  
operatorExprFloat :: forall a. Expr a => (Number -> Number -> Number) -> String -> a -> Float -> a
operatorExprFloat f op x (FloatConstant y) = unaryFunction (flip f y) (\x' -> "(" <> x' <> op <> showNumber y <> ")") x
operatorExprFloat _ op x (FloatExpr y) = expr $ "(" <> toExprSafe x <> op <> y <> ")"

arithmeticOperator :: forall a. Expr a => (Number -> Number -> Number) -> String -> a -> a -> a
arithmeticOperator f op = binaryFunction f (binOp op)

add :: forall a. Expr a => a -> a -> a
add = arithmeticOperator (+) "+"

addFloatExpr :: forall b. Expr b => Float -> b -> b
addFloatExpr = operatorFloatExpr (+) "+"
-- addFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a + b') (\b' -> binOp "+" (show a) b') b
-- addFloatExpr (FloatExpr a) b = expr $ binOp "+" a (toExpr b)

addExprFloat :: forall a. Expr a => a -> Float -> a
addExprFloat = operatorExprFloat (+) "+"
--addExprFloat = flip addFloatExpr

difference :: forall a. Expr a => a -> a -> a
difference = arithmeticOperator (-) "-"

differenceFloatExpr :: forall b. Expr b => Float -> b -> b
differenceFloatExpr = operatorFloatExpr (-) "-"
-- differenceFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a - b') (\b' -> binOp "-" (show a) b') b
-- differenceFloatExpr (FloatExpr a) b = expr $ binOp "-" a (toExpr b)

differenceExprFloat :: forall b. Expr b => b -> Float -> b
differenceExprFloat = operatorExprFloat (-) "-"
-- differenceExprFloat a (FloatConstant b) = unaryFunction (\a' -> a' - b) (\a' -> binOp "-" a' (show b)) a
-- differenceExprFloat a (FloatExpr b) = expr $ binOp "-" (toExpr a) b

product :: forall a. Expr a => a -> a -> a
product = arithmeticOperator (*) "*"

productFloatExpr :: forall b. Expr b => Float -> b -> b
productFloatExpr = operatorFloatExpr (*) "*"
-- productFloatExpr (FloatConstant a) b = unaryFunction (\x -> a * x) (\x -> binOp "*" (show a) x) b
-- productFloatExpr (FloatExpr a) b = expr $ binOp "*" a (toExpr b)

productExprFloat :: forall a. Expr a => a -> Float -> a
productExprFloat = operatorExprFloat (*) "*"
-- productExprFloat = flip productFloatExpr

division :: forall a. Expr a => a -> a -> a
division = arithmeticOperator (/) "/" -- TODO: this should be safe division to match the audio side!

divisionFloatExpr :: forall b. Expr b => Float -> b -> b
divisionFloatExpr = operatorFloatExpr (/) "/" -- TODO: this should be safe division to match the audio side!
-- divisionFloatExpr (FloatConstant a) b = unaryFunction (\b' -> a / b') (\b' -> binOp "/" (show a) b') b
-- divisionFloatExpr (FloatExpr a) b = expr $ binOp "/" a (toExpr b)

-- TODO: this should be safe division to match the audio side!
divisionExprFloat :: forall b. Expr b => b -> Float -> b
divisionExprFloat = operatorExprFloat (/) "/" -- TODO: this should be safe division to match the audio side!
-- divisionExprFloat a (FloatConstant b) = unaryFunction (\a' -> a' / b) (\a' -> binOp "/" a' (show b)) a
-- divisionExprFloat a (FloatExpr b) = expr $ binOp "/" (toExpr a) b


min :: forall a. Expr a => a -> a -> a
min = binaryFunction Prelude.min (function2 "min")

minFloatExpr :: forall a. Expr a => Float -> a -> a
minFloatExpr x y = min (fromFloat x) y
-- note: if we ever make minExprFloat it would be implemented differently since GLSL includes a variant of min that takes any type followed by a float

max :: forall a. Expr a => a -> a -> a
max = binaryFunction Prelude.max (function2 "max")

maxFloatExpr :: forall a. Expr a => Float -> a -> a
maxFloatExpr x y = max (fromFloat x) y
-- note: if we ever make maxExprFloat it would be implemented differently since GLSL includes a variant of max that takes any type followed by a float

pow :: forall a. Expr a => a -> a -> a
pow = binaryFunction Number.pow (function2 "pow")

powFloatExpr :: forall a. Expr a => Float -> a -> a
powFloatExpr x y = pow (fromFloat x) y

mod :: forall a. Expr a => a -> a -> a
mod = binaryFunction Prelude.mod (function2 "mod")

modFloatExpr :: forall a. Expr a => Float -> a -> a
modFloatExpr x y = mod (fromFloat x) y


comparisonOperator :: forall a. Expr a => (Number -> Number -> Boolean) -> String -> String -> a -> a -> a
comparisonOperator f funcName op x y 
  | channels x == 1 = binaryFunction (\a b -> booleanToNumber $ f a b) (\a b -> function1 (showType x) $ binOp op a b) x y
  | otherwise = binaryFunction (\a b -> booleanToNumber $ f a b) (\a b -> function1 (showType x) $ function2 funcName a b) x y

booleanToNumber :: Boolean -> Number
booleanToNumber true = 1.0
booleanToNumber false = 0.0

comparisonOperatorFloatExpr :: forall a. Expr a => (Number -> Number -> Boolean) -> String -> String -> Float -> a -> a
comparisonOperatorFloatExpr f funcName op x y 
  | channels y == 1 = binaryFunctionFloatExpr (\a b -> booleanToNumber $ f a b) (\a b -> function1 "float" $ binOp op a b) x y
  | otherwise = binaryFunctionFloatExpr (\a b -> booleanToNumber $ f a b) (\a b -> function1 (showType y) $ function2 funcName a b) x y

binaryFunctionFloatExpr :: forall a. Expr a => (Number -> Number -> Number) -> (String -> String -> String) -> Float -> a -> a
binaryFunctionFloatExpr f1 f2 (FloatConstant x) y = unaryFunction (f1 x) (f2 $ showNumber x) y
binaryFunctionFloatExpr _ f2 (FloatExpr x) y = expr $ f2 x (toExpr y)


equal :: forall a. Expr a => a -> a -> a
equal = comparisonOperator (==) "equal" "==" 

equalFloatExpr :: forall a. Expr a => Float -> a -> a
equalFloatExpr = comparisonOperatorFloatExpr (==) "equal" "==" 

notEqual :: forall a. Expr a => a -> a -> a
notEqual = comparisonOperator (/=) "notEqual" "!="

notEqualFloatExpr :: forall a. Expr a => Float -> a -> a
notEqualFloatExpr = comparisonOperatorFloatExpr (/=) "notEqual" "/=" 

greaterThan :: forall a. Expr a => a -> a -> a
greaterThan = comparisonOperator (>) "greaterThan" ">"

greaterThanFloatExpr :: forall a. Expr a => Float -> a -> a
greaterThanFloatExpr = comparisonOperatorFloatExpr (>) "greaterThan" ">" 

greaterThanEqual :: forall a. Expr a => a -> a -> a
greaterThanEqual = comparisonOperator (>=) "greaterThanEqual" ">=" 

greaterThanEqualFloatExpr :: forall a. Expr a => Float -> a -> a
greaterThanEqualFloatExpr = comparisonOperatorFloatExpr (>=) "greaterThanEqual" ">=" 

lessThan :: forall a. Expr a => a -> a -> a
lessThan = comparisonOperator (<) "lessThan" "<"

lessThanFloatExpr :: forall a. Expr a => Float -> a -> a
lessThanFloatExpr = comparisonOperatorFloatExpr (<) "lessThan" "<" 

lessThanEqual ::forall a. Expr a => a -> a -> a
lessThanEqual = comparisonOperator (<=) "lessThanEqual" "<="

lessThanEqualFloatExpr :: forall a. Expr a => Float -> a -> a
lessThanEqualFloatExpr = comparisonOperatorFloatExpr (<=) "lessThanEqual" "<=" 


-- second argument is multiply used and should be assigned by caller
gate :: forall a. Expr a => Float -> a -> a
gate x y = product (lessThan (fromFloat x) y) y

-- all arguments are multiply used and should be assigned by the caller
between :: forall a. Expr a => Vec2 -> a -> a
between r x = product (lessThan trueMin x) (difference (constant 1.0) (lessThan trueMax x))
  where
    r1 = swizzleX r
    r2 = swizzleY r
    trueMin = fromFloat (min r1 r2)
    trueMax = fromFloat (max r1 r2)

-- first argument is multiply used and should be assigned by the caller
clip :: forall a. Expr a => Vec2 -> a -> a
clip r x = clamp (min e0 e1) (max e0 e1) x
  where
    e0 = swizzleX r
    e1 = swizzleY r

-- clamp is an "unchecked" clipping operation (ie. weird results if min and max are out of order)
clamp :: forall a. Expr a => Float -> Float -> a -> a
clamp e0 e1 x = expr $ "clamp(" <> toExpr x <> "," <> toExpr e0 <> "," <> toExpr e1 <> ")"

-- first argument is multiply used and should be assigned by the caller
smoothStep :: forall a. Expr a => Vec2 -> a -> a
smoothStep r x = expr $ "smoothstep(" <> toExpr (swizzleX r) <> "," <> toExpr (swizzleY r) <> "," <> toExpr x <> ")"


distance :: forall a. Expr a => a -> a -> Float
distance a b
  | isConstant a && isConstant b = sqrt $ dotSum $ squared $ difference a b
  | otherwise = expr $ "distance(" <> toExpr a <> "," <> toExpr b <> ")"
  
prox :: Vec2 -> Vec2 -> Float
prox a b = clamp (constant 0.0) (constant 1.0) $ division (difference (constant 2.828427) (distance a b)) (constant 2.828427)

circle :: forall a. Expr a => Vec2 -> Vec2 -> a -> a
circle fxy xy diameter = smoothStep r $ differenceFloatExpr (distance fxy xy) (product diameter (constant 0.5))
  where
    r = floatFloatToVec2 (expr "1.5/(res.x+res.y)") (constant 0.0)

point :: Vec2 -> Vec2 -> Float 
point fxy xy = circle fxy xy d
  where d = expr "((1./res.x)+(1./res.y))"

-- third argument (width) is multiply used and should be pre-assigned by caller
vline :: forall a. Expr a => Vec2 -> a -> Float -> a
vline fxy x w = smoothStep es $ differenceExprFloat (abs (differenceFloatExpr (swizzleX fxy) x)) w
  where
    es = floatFloatToVec2 (constant 0.0) (min w (expr "3./res.x"))

-- third argument (height) is multiply used and should be pre-assigned by caller  
hline :: forall a. Expr a => Vec2 -> a -> Float -> a
hline fxy y h = smoothStep es $ differenceExprFloat (abs (differenceFloatExpr (swizzleY fxy) y)) h
  where
    es = floatFloatToVec2 (constant 0.0) (min h (expr "3./res.y"))

line :: Vec2 -> Vec2 -> Vec2 -> Float -> Float
line fxy xy1 xy2 w = expr $ "line(" <> toExpr xy1 <> "," <> toExpr xy2 <> "," <> toExpr w <> "," <> toExpr fxy <> ")" 

iline :: Vec2 -> Vec2 -> Vec2 -> Float -> Float
iline fxy xy1 xy2 w = expr $ "iline(" <> toExpr xy1 <> "," <> toExpr xy2 <> "," <> toExpr w <> "," <> toExpr fxy <> ")" 

linlin :: forall a. Expr a => Vec2 -> Vec2 -> a -> a
linlin r1 r2 x = addExprFloat x'' (swizzleX r2)
  where
    r1size = difference (swizzleY r1) (swizzleX r1)
    x' = divisionExprFloat (differenceExprFloat x (swizzleX r1)) r1size
    r2size = difference (swizzleY r2) (swizzleX r2)
    x'' = productExprFloat x' r2size
    
mix :: forall a. Expr a => a -> a -> a -> a
mix x y a = expr $ "mix(" <> toExpr x <> "," <> toExpr y <> "," <> toExpr a <> ")"

mixFloat :: forall a. Expr a => a -> a -> Float -> a
mixFloat x y a = expr $ "mix(" <> toExpr x <> "," <> toExpr y <> "," <> toExpr a <> ")"

seq :: Float -> NonEmptyList Float -> Float
seq y steps = sum allSteps
  where
    nSteps = length steps
    stepSize = FloatConstant $ 1.0 / toNumber nSteps
    step a b = expr $ "step(" <> toExpr a <> "," <> toExpr b <> ")"
    -- headStep = product (step y stepSize) (head steps)
    -- lastStep = product (step (difference (constant 1.0) stepSize) y) (last steps)
    stepF n r = product (difference (step (product stepSize n) y) (step (add (product stepSize n) stepSize) y)) r
    stepNumbers = map (FloatConstant <<< toNumber) $ range 0 (nSteps - 1) :: NonEmptyList Float
    allSteps = zipWith stepF stepNumbers steps

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

setX :: Vec2 -> Float -> Vec2
setX (Vec2Constant _ y) (FloatConstant x) = Vec2Constant x y
setX xy x = expr $ "vec2(" <> toExpr x <> "," <> toExpr (swizzleY xy) <> ")"

setY :: Vec2 -> Float -> Vec2
setY (Vec2Constant x _) (FloatConstant y) = Vec2Constant x y
setY xy y = expr $ "vec2(" <> toExpr (swizzleX xy) <> "," <> toExpr y <> ")" 

-- for each vec4 in a blend, xyz stay what they are, alpha is multiplied by provided fade expression
rgbaFade :: Float -> Vec4 -> Vec4
rgbaFade f v4 = vec3FloatToVec4 (swizzleXYZ v4) (product (swizzleW v4) f)

