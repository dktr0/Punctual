module Expr where

import Prelude (class Eq, class Ord, class Show,($),(<>),show,(+),(-),(*),(/),(<<<),flip,(==),(/=),(>),(>=),(<),(<=),map,identity)
import Prelude as Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Number as Number
import Data.List.NonEmpty (NonEmptyList,head,tail,fromList,concat,cons,singleton)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (unfoldr1)

import Channels
import Number as Number
import Multi (mapRows,Multi)

class Expr a where
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
  fromVec2s = concat <<< map (\a -> swizzleX a `cons` singleton (swizzleY a))
  fromVec3s = concat <<< map (\a -> swizzleX a `cons` (swizzleY a `cons` singleton (swizzleZ a)))
  fromVec4s = concat <<< map (\a -> swizzleX a `cons` (swizzleY a `cons` (swizzleZ a `cons` singleton (swizzleZ a))))

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
  fromFloats = concat <<< unfoldr1 unconsFloatsToVec2s
  fromVec2s = identity
  fromVec3s = concat <<< unfoldr1 unconsVec3sToVec2s
  fromVec4s = concat <<< map (\a -> swizzleXY a `cons` singleton (swizzleZW a))
  
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
  fromFloats = concat <<< unfoldr1 unconsFloatsToVec3s
  fromVec2s _ = singleton (constant 0.0) -- PLACEHOLDER/TODO
  fromVec3s = identity
  fromVec4s _ = singleton (constant 0.0) -- PLACEHOLDER/TODO

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
  fromFloats = concat <<< unfoldr1 unconsFloatsToVec4s
  fromVec2s _ = singleton (constant 0.0) -- PLACEHOLDER/TODO
  fromVec3s _ = singleton (constant 0.0) -- PLACEHOLDER/TODO
  fromVec4s = identity

instance Channels Vec4 where channels _ = 1


-- Construction of Expr types by composition of smaller types

floatFloatToVec2 :: Float -> Float -> Vec2
floatFloatToVec2 (FloatConstant x) (FloatConstant y) = Vec2Constant x y
floatFloatToVec2 x y = Vec2Expr $ "vec2(" <> toExpr x <> "," <> toExpr y <> ")"

floatFloatToVec3 :: Float -> Float -> Vec3
floatFloatToVec3 (FloatConstant x) (FloatConstant y) = Vec3Constant x y y
floatFloatToVec3 x y = Vec3Expr $ "vec3(" <> toExpr x <> ",vec2(" <> toExpr y <> "))"

floatFloatFloatToVec3 :: Float -> Float -> Float -> Vec3
floatFloatFloatToVec3 (FloatConstant x) (FloatConstant y) (FloatConstant z) = Vec3Constant x y z
floatFloatFloatToVec3 x y z = Vec3Expr $ "vec3(" <> toExpr x <> "," <> toExpr y <> "," <> toExpr z <> ")"

floatFloatToVec4 :: Float -> Float -> Vec4
floatFloatToVec4 (FloatConstant x) (FloatConstant y) = Vec4Constant x y y y
floatFloatToVec4 x y = Vec4Expr $ "vec4(" <> toExpr x <> ",vec3(" <> toExpr y <> "))"

floatFloatFloatToVec4 :: Float -> Float -> Float -> Vec4
floatFloatFloatToVec4 (FloatConstant x) (FloatConstant y) (FloatConstant z) = Vec4Constant x y z z
floatFloatFloatToVec4 x y z = Vec4Expr $ "vec4(" <> toExpr x <> "," <> toExpr y <> ",vec2(" <> toExpr z <> "))"

floatFloatFloatFloatToVec4 :: Float -> Float -> Float -> Float -> Vec4
floatFloatFloatFloatToVec4 (FloatConstant w) (FloatConstant x) (FloatConstant y) (FloatConstant z) = Vec4Constant w x y z
floatFloatFloatFloatToVec4 w x y z = Vec4Expr $ "vec4(" <> toExpr w <> "," <> toExpr x <> "," <> toExpr y <> "," <> toExpr z <> ")"


-- Construction of Expr types by uncons-ing lists of other Expr types

unconsFloatsToVec2s :: NonEmptyList Float -> Tuple (NonEmptyList Vec2) (Maybe (NonEmptyList Float))
unconsFloatsToVec2s xs = 
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ fromFloat $ head xs) Nothing
    Just xs' -> Tuple (singleton $ floatFloatToVec2 (head xs) (head xs')) (fromList $ tail xs')
    
unconsVec3sToVec2s :: NonEmptyList Vec3 -> Tuple (NonEmptyList Vec2) (Maybe (NonEmptyList Vec3))
unconsVec3sToVec2s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (swizzleXY (head xs) `cons` singleton (swizzleZZ (head xs))) Nothing -- one vec3 makes two vec2s
    Just xs' -> 
      let a = swizzleXY $ head xs
          b = floatFloatToVec2 (swizzleZ (head xs)) (swizzleX (head xs'))
          c = swizzleYZ $ head xs'
      in Tuple (a `cons` (b `cons` singleton c)) (fromList $ tail xs') -- two vec3s make three vec2s, evenly

unconsFloatsToVec3s :: NonEmptyList Float -> Tuple (NonEmptyList Vec3) (Maybe (NonEmptyList Float))
unconsFloatsToVec3s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ fromFloat $ head xs) Nothing
    Just xs' -> 
      case fromList (tail xs') of
        Nothing -> Tuple (singleton $ floatFloatToVec3 (head xs) (head xs')) Nothing
        Just xs'' -> Tuple (singleton $ floatFloatFloatToVec3 (head xs) (head xs') (head xs'')) (fromList $ tail xs'')
        
unconsFloatsToVec4s :: NonEmptyList Float -> Tuple (NonEmptyList Vec4) (Maybe (NonEmptyList Float))
unconsFloatsToVec4s xs =
  case fromList (tail xs) of
    Nothing -> Tuple (singleton $ fromFloat $ head xs) Nothing
    Just xs' -> 
      case fromList (tail xs') of
        Nothing -> Tuple (singleton $ floatFloatToVec4 (head xs) (head xs')) Nothing
        Just xs'' ->
          case fromList (tail xs'') of
            Nothing -> Tuple (singleton $ floatFloatFloatToVec4 (head xs) (head xs') (head xs'')) Nothing
            Just xs''' -> Tuple (singleton $ floatFloatFloatFloatToVec4 (head xs) (head xs') (head xs'') (head xs''')) (fromList $ tail xs''')


-- Swizzling

class SwizzleX a where
  swizzleX :: a -> Float

instance SwizzleX Float where
  swizzleX = identity

instance SwizzleX Vec2 where
  swizzleX (Vec2Constant x _) = FloatConstant x
  swizzleX (Vec2Expr x) = FloatExpr $ x <> ".x"

instance SwizzleX Vec3 where
  swizzleX (Vec3Constant x _ _) = FloatConstant x
  swizzleX (Vec3Expr x) = FloatExpr $ x <> ".x"

instance SwizzleX Vec4 where
  swizzleX (Vec4Constant x _ _ _) = FloatConstant x
  swizzleX (Vec4Expr x) = FloatExpr $ x <> ".x"

class SwizzleY a where
  swizzleY :: a -> Float
  swizzleXY :: a -> Vec2

instance SwizzleY Vec2 where
  swizzleY (Vec2Constant _ y) = FloatConstant y
  swizzleY (Vec2Expr e) = FloatExpr $ e <> ".y"
  swizzleXY = identity

instance SwizzleY Vec3 where
  swizzleY (Vec3Constant _ y _) = FloatConstant y
  swizzleY (Vec3Expr e) = FloatExpr $ e <> ".y"
  swizzleXY (Vec3Constant x y _) = Vec2Constant x y
  swizzleXY (Vec3Expr e) = Vec2Expr $ e <> ".xy"

instance SwizzleY Vec4 where
  swizzleY (Vec4Constant _ y _ _) = FloatConstant y
  swizzleY (Vec4Expr e) = FloatExpr $ e <> ".y"
  swizzleXY (Vec4Constant x y _ _) = Vec2Constant x y
  swizzleXY (Vec4Expr e) = Vec2Expr $ e <> ".xy"

class SwizzleZ a where
  swizzleZ :: a -> Float
  swizzleYZ :: a -> Vec2
  swizzleZZ :: a -> Vec2

instance SwizzleZ Vec3 where
  swizzleZ (Vec3Constant _ _ z) = FloatConstant z
  swizzleZ (Vec3Expr e) = FloatExpr $ e <> ".z"
  swizzleYZ (Vec3Constant _ y z) = Vec2Constant y z
  swizzleYZ (Vec3Expr e) = Vec2Expr $ e <> ".yz"
  swizzleZZ (Vec3Constant _ _ z) = Vec2Constant z z
  swizzleZZ (Vec3Expr e) = Vec2Expr $ e <> ".zz"

instance SwizzleZ Vec4 where
  swizzleZ (Vec4Constant _ _ z _) = FloatConstant z
  swizzleZ (Vec4Expr e) = FloatExpr $ e <> ".z"
  swizzleYZ (Vec4Constant _ y z _) = Vec2Constant y z
  swizzleYZ (Vec4Expr e) = Vec2Expr $ e <> ".yz"
  swizzleZZ (Vec4Constant _ _ z _) = Vec2Constant z z
  swizzleZZ (Vec4Expr e) = Vec2Expr $ e <> ".zz"

class SwizzleW a where
  swizzleW :: a -> Float
  swizzleZW :: a -> Vec2

instance SwizzleW Vec4 where
  swizzleW (Vec4Constant _ _ _ w) = FloatConstant w
  swizzleW (Vec4Expr e) = FloatExpr $ e <> ".w"
  swizzleZW (Vec4Constant _ _ z w) = Vec2Constant z w
  swizzleZW (Vec4Expr e) = Vec2Expr $ e <> ".zw"

{-
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
-}


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

-- TODO: need to implement powFloatExpr for this to compile
-- dbamp :: forall a. Expr a => a -> a
-- dbamp = powFloatExpr (constant 10.0) <<< flip divisionExprFloat (constant 20.0)

exp :: forall a. Expr a => a -> a
exp = unaryFunction Number.exp (function1 "exp")

floor :: forall a. Expr a => a -> a
floor = unaryFunction Number.floor (function1 "floor")

-- TODO: need to implement powExprFloat for this to compile
-- midicps :: forall a. Expr a => a -> a
-- midicps m = productExprFloat (powExprFloat (divisionExprFloat (differenceExprFloat m (constant 69.0)) (constant 12.0)) (constant 2.0)) (constant 440.0)

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

-- maybe TODO later: make variants for mismatched types where one argument is a float

{-

-- to polyfill pow and mod to the standard model, coerce unmatched float arguments to the other type (will simply be a cast)
-- this will produce invalid GLSL code (without warning/error) if different non-float GLSL types are mixed
powOrMod :: String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
powOrMod _ f (constant x) (constant y) = constant $ f x y
powOrMod f _ (constant x) (Reference Float y) = Reference Float $ f <> "(" <> show x <> "," <> y <> ")"
powOrMod f _ (constant x) (Reference yType y) = Reference yType $ f <> "(" <> show yType <> "(" <> show x <> ")," <> y <> ")"
powOrMod f _ (Reference Float x) (constant y) = Reference Float $ f <> "(" <> x <> "," <> show y <> ")"
powOrMod f _ (Reference xType x) (constant y) = Reference xType $ f <> "(" <> x <> "," <> show xType <> "(" <> show y <> "))"
powOrMod f _ (Reference xType x) (Reference _ y) = Reference xType $ f <> "(" <> x <> "," <> y <> ")"

pow :: Expr -> Expr -> Expr
pow = powOrMod "pow" Number.pow

mod :: Expr -> Expr -> Expr
mod = powOrMod "mod" Prelude.mod


comparisonOperator :: String -> String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
-- both arguments floats
comparisonOperator _ _ f (constant x) (constant y) = constant $ f x y
comparisonOperator f _ _ (constant x) (Reference Float y) = Reference Float $ "float(" <> show x <> f <> y <> ")"
comparisonOperator f _ _ (Reference Float x) (constant y) = Reference Float $ "float(" <> x <> f <> show y <> ")"
comparisonOperator f _ _ (Reference Float x) (Reference Float y) = Reference Float $ "float(" <> x <> f <> y <> ")"
-- only one argument is a float
comparisonOperator _ f _ (constant x) (Reference yType y) = Reference yType $ show yType <> "(" <> f <> "(" <> show yType <> "(" <> show x <> ")," <> y <> "))"
comparisonOperator _ f _ (Reference Float x) (Reference yType y) = Reference yType $ show yType <> "(" <> f <> "(" <> show yType <> "(" <> x <> ")," <> y <> "))"
comparisonOperator _ f _ (Reference xType x) (constant y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> show xType <> "(" <> show y <> ")))"
comparisonOperator _ f _ (Reference xType x) (Reference Float y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> show xType <> "(" <> y <> ")))"
-- no arguments are floats
comparisonOperator _ f _ (Reference xType x) (Reference _ y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> y <> "))"

booleanToNumber :: Boolean -> Number
booleanToNumber true = 1.0
booleanToNumber false = 0.0

equal :: Expr -> Expr -> Expr
equal = comparisonOperator "==" "equal" (map (map booleanToNumber) (==))

notEqual :: Expr -> Expr -> Expr
notEqual = comparisonOperator "!=" "notEqual" (map (map booleanToNumber) (/=))

greaterThan :: Expr -> Expr -> Expr
greaterThan = comparisonOperator ">" "greaterThan" (map (map booleanToNumber) (>))

greaterThanEqual :: Expr -> Expr -> Expr
greaterThanEqual = comparisonOperator ">=" "greaterThanEqual" (map (map booleanToNumber) (>=))

lessThan :: Expr -> Expr -> Expr
lessThan = comparisonOperator "<" "lessThan" (map (map booleanToNumber) (<))

lessThanEqual ::Expr -> Expr -> Expr
lessThanEqual = comparisonOperator "<=" "lessThanEqual" (map (map booleanToNumber) (<=))


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

distance :: forall a. Expr a => a -> a -> a
distance a b 
  | isConstant a && isConstant b = sqrt $ sum $ squared $ simpleBinaryFunction (-) (\_ _ -> "") a b
  | otherwise = exprConstructor $ "distance(" <> toExpr a <> "," <> toExpr b <> ")"

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


{-    
prox :: GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 -> Float
prox a b
  | a.glslType /= Vec2 || b.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in prox", glslType: Float, isSimple: false, deps: a.deps <> b.deps }
  | otherwise = { string: "clamp((2.828427-distance(" <> a.string <> "," <> b.string <> "))/2.828427,0.,1.)", glslType: Float, isSimple: false, deps: a.deps <> b.deps }
-}


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

{-
rgbhsv :: GLSLExpr -> GLSLExpr
rgbhsv x
  | x.glslType /= Vec3 = { string: "!! Internal Punctual GLSL generation error in rgbhsv", glslType: Float, isSimple: false, deps: x.deps }
  | otherwise = { string: "rgbhsv(" <> x.string <> ")", glslType: Vec3, isSimple: false, deps: x.deps }
  
hsvrgb :: GLSLExpr -> GLSLExpr
hsvrgb x
  | x.glslType /= Vec3 = { string: "!! Internal Punctual GLSL generation error in hsvrgb", glslType: Float, isSimple: false, deps: x.deps }
  | otherwise = { string: "hsvrgb(" <> x.string <> ")", glslType: Vec3, isSimple: false, deps: x.deps }
  
distance :: GLSLExpr -> GLSLExpr -> GLSLExpr
distance x y
  | x.glslType /= y.glslType = { string: "!! Internal Punctual GLSL generation error in distance", glslType: Float, isSimple: false, deps: x.deps <> y.deps }
  | otherwise = { string: "distance(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps }
-}

