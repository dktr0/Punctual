module GLSLExpr where

-- This module provides types and functions to represent and manipulate the text of
-- strongly-typed GLSL expressions (with Purescript-compile-time type checking).

import Prelude (class Eq,class Show,show,(+),(<>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable (class Foldable,foldl)

class GLSLExpr c where
  isSimple :: c -> Boolean
  string :: c -> String
  channels :: c -> Int

-- additional classes can be declared for conditions characteristic of GLSL's slightly-strict-slightly-loose typing
-- such as something that can be either a Float or a Vec4, things that can be any of the types, etc, then these
-- classes can be constraints on polymorphic functions we use to assemble fragment shader expressions.
-- (we'll still need some kind of type that unifies the four types Float,Vec2,Vec3,Vec4 though so we can have heterogenous collections.)

class FloatVec4 c

data Float = Float Boolean String
derive instance Eq Float
derive instance Generic Float _
instance Show Float where
  show = genericShow
instance GLSLExpr Float where
  isSimple (Float x _) = x
  string (Float _ x) = x
  channels (Float _ _) = 1
instance FloatVec4 Float

data Vec2 = Vec2 Boolean String
derive instance Eq Vec2
derive instance Generic Vec2 _
instance Show Vec2 where
  show = genericShow
instance GLSLExpr Vec2 where
  isSimple (Vec2 x _) = x
  string (Vec2 _ x) = x
  channels (Vec2 _ _) = 2
  
data Vec3 = Vec3 Boolean String
derive instance Eq Vec3
derive instance Generic Vec3 _
instance Show Vec3 where
  show = genericShow
instance GLSLExpr Vec3 where
  isSimple (Vec3 x _) = x
  string (Vec3 _ x) = x
  channels (Vec3 _ _) = 3
  
data Vec4 = Vec4 Boolean String
derive instance Eq Vec4
derive instance Generic Vec4 _
instance Show Vec4 where
  show = genericShow
instance GLSLExpr Vec4 where
  isSimple (Vec4 x _) = x
  string (Vec4 _ x) = x
  channels (Vec4 _ _) = 4
instance FloatVec4 Vec4
  
{-
exprsChannels :: forall f. Foldable f => f GLSLExpr -> Int
exprsChannels = foldl (\n e -> n + exprChannels e) 0 

-- a convenience function for making a no-dependency (simple) Float
stringToFloat :: String -> GLSLExpr
stringToFloat x = { glslType: Float, isSimple: true, string: x }

-- and another one for doing the same thing from a Purescript Number
numberToFloat :: Number -> GLSLExpr
numberToFloat x = { glslType: Float, isSimple: true, string: show x }

-- unsafe because performs no checking that the indicated cast is valid GLSL
-- considering: any benefit to making a safe cast function that can throw an Error?
-- *but actually even better would be to have compile-time type safety for this*
unsafeCast :: GLSLType -> GLSLExpr -> GLSLExpr
unsafeCast Float x = { glslType: Float, isSimple: x.isSimple, string: "float(" <> x.string <> ")" }
unsafeCast Vec2 x = { glslType: Vec2, isSimple: x.isSimple, string: "vec2(" <> x.string <> ")" }
unsafeCast Vec3 x = { glslType: Vec3, isSimple: x.isSimple, string: "vec3(" <> x.string <> ")" }
unsafeCast Vec4 x = { glslType: Vec4, isSimple: x.isSimple, string: "vec4(" <> x.string <> ")" }

unaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr
unaryFunction funcName t x = { glslType: t, isSimple: false, string: funcName <> "(" <> x.string <> ")" }

binaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunction funcName t x y = { glslType: t, isSimple: false, string: funcName <> "(" <> x.string <> "," <> y.string <> ")" }

binaryOperator :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryOperator op t x y =  { glslType: t, isSimple: false, string: "(" <> x.string <> op <> y.string <> ")" }

ternaryFunction :: String -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
ternaryFunction funcName t x y z = { glslType: t, isSimple: false, string: funcName <> "(" <> x.string <> "," <> y.string <> "," <> z.string <> ")" }

{-
-- for unary functions like sin(x) where any float type can be the argument
-- and the result type matches the type of the argument
unaryFunctionMatched :: Builder -> GLSLExpr -> GLSLExpr
unaryFunctionMatched funcName x = unaryFunction funcName (glslType x) x

-- for functions like pow that expect "matched" argument types and result in the same type
-- plus: when one of the arguments is GLFloat we can cast it to a matching type
binaryFunctionMatched :: Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunctionMatched b x y
  | glslType x == glslType y = binaryFunction b (glslType x) x y
  | glslType x == GLFloat = binaryFunction b (glslType y) (unsafeCast (glslType y) x) y
  | glslType y == GLFloat = binaryFunction b (glslType x) x (unsafeCast (glslType x) y)
  | otherwise = error "uh-oh! arguments to binaryFunctionMatched must be matched types, or one must be GLFloat"

-- for operators like "+" that expect "matched" argument types and result in the same type
-- plus: when one of the arguments is GLFloat we can cast it to a matching type
binaryOperatorMatched :: Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryOperatorMatched op x y
  | glslType x == glslType y = binaryOperator op (glslType x) x y
  | glslType x == GLFloat = binaryOperator op (glslType y) (unsafeCast (glslType y) x) y
  | glslType y == GLFloat = binaryOperator op (glslType x) x (unsafeCast (glslType x) y)
  | otherwise = error "uh-oh! arguments to binaryFunctionMatched must be matched types, or one must be GLFloat"

comparisonOperator :: Builder -> Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
comparisonOperator opName funcName x y
  | glslType x == GLFloat && glslType y == GLFloat = unsafeCast GLFloat $ binaryOperator opName GLFloat x y
  | glslType x == GLFloat = unsafeCast (glslType y) $ binaryFunctionMatched funcName x y
  | glslType y == GLFloat = unsafeCast (glslType x) $ binaryFunctionMatched funcName x y
  | glslType x == glslType y = unsafeCast (glslType x) $ binaryFunctionMatched funcName x y
  | otherwise = error "uhoh - comparisonOp called with mismatched/misaligned GLSLExpr types"


-- note: the swizzle_ definitions don't perform any type-checking on the expression
-- they are given. However, this is probably something we should add!

swizzleX :: GLSLExpr -> GLSLExpr
swizzleX x = GLSLExpr GLFloat False $ builder x <> ".x"

swizzleY :: GLSLExpr -> GLSLExpr
swizzleY x = GLSLExpr GLFloat False $ builder x <> ".y"

swizzleZ :: GLSLExpr -> GLSLExpr
swizzleZ x = GLSLExpr GLFloat False $ builder x <> ".z"

swizzleW :: GLSLExpr -> GLSLExpr
swizzleW x = GLSLExpr GLFloat False $ builder x <> ".w"

swizzleXY :: GLSLExpr -> GLSLExpr
swizzleXY x = GLSLExpr Vec2 False $ builder x <> ".xy"

swizzleYZ :: GLSLExpr -> GLSLExpr
swizzleYZ x = GLSLExpr Vec2 False $ builder x <> ".yz"

swizzleZW :: GLSLExpr -> GLSLExpr
swizzleZW x = GLSLExpr Vec2 False $ builder x <> ".zw"

swizzleXYZ :: GLSLExpr -> GLSLExpr
swizzleXYZ x = GLSLExpr Vec3 False $ builder x <> ".xyz"

swizzleYZW :: GLSLExpr -> GLSLExpr
swizzleYZW x = GLSLExpr Vec3 False $ builder x <> ".yzw"

swizzleXYY :: GLSLExpr -> GLSLExpr
swizzleXYY x = GLSLExpr Vec3 False $ builder x <> ".xyy"

swizzleXYYY :: GLSLExpr -> GLSLExpr
swizzleXYYY x = GLSLExpr Vec4 False $ builder x <> ".xyyy"

swizzleXYZZ :: GLSLExpr -> GLSLExpr
swizzleXYZZ x = GLSLExpr Vec4 False $ builder x <> ".xyzz"

-- convert any GLSLExpr to an GLSLExpr containing a GLFloat, by discarding "channels" after the first one
exprToGLFloat :: GLSLExpr -> GLSLExpr
exprToGLFloat x
  | glslType x == GLFloat = x
  | otherwise = swizzleX x

-- convert any GLSLExpr to an GLSLExpr containing a Vec2, by repeating or discarding channels
exprToVec2 :: GLSLExpr -> GLSLExpr
exprToVec2 x
  | glslType x == GLFloat = GLSLExpr Vec2 False $ "vec2(" <> builder x <> ")"
  | glslType x == Vec2 = x
  | otherwise = swizzleXY x

-- convert any GLSLExpr to an GLSLExpr containing a Vec3, by repeating or discarding channels
exprToVec3 :: GLSLExpr -> GLSLExpr
exprToVec3 x
  | glslType x == GLFloat = GLSLExpr Vec3 False $ "vec3(" <> builder x <> ")"
  | glslType x == Vec2 = swizzleXYY x
  | glslType x == Vec3 = x
  | otherwise = swizzleXYZ x

-- convert any GLSLExpr to an GLSLExpr containing a Vec4, by repeating channels
exprToVec4 :: GLSLExpr -> GLSLExpr
exprToVec4 x
  | glslType x == GLFloat = GLSLExpr Vec4 False $ "vec4(" <> builder x <> ")"
  | glslType x == Vec2 = swizzleXYYY x
  | glslType x == Vec3 = swizzleXYZZ x
  | otherwise = x

-- assemble a GLSLExpr containing a Vec2 by combining two GLSLExpr-s
exprExprToVec2 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec2 (GLSLExpr GLFloat _ x) (GLSLExpr GLFloat _ y) = GLSLExpr Vec2 False $ "vec2(" <> x <> "," <> y <> ")"
exprExprToVec2 _ _ = error "exprExprToVec2 called with inappropriate types"

-- assemble a GLSLExpr containing a Vec3 by combining two GLSLExpr-s
exprExprToVec3 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec3 (GLSLExpr GLFloat _ x) (GLSLExpr Vec2 _ y) = GLSLExpr Vec3 False $ "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 (GLSLExpr Vec2 _ x) (GLSLExpr GLFloat _ y) = GLSLExpr Vec3 False $ "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 _ _ = error "exprExprToVec3 called with inappropriate types"

-- assemble a GLSLExpr containing a Vec4 by combining two GLSLExpr-s
exprExprToVec4 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec4 (GLSLExpr GLFloat _ x) (GLSLExpr Vec3 _ y) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (GLSLExpr Vec2 _ x) (GLSLExpr Vec2 _ y) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (GLSLExpr Vec3 _ x) (GLSLExpr GLFloat _ y) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 _ _ = error "exprExprToVec4 called with inappropriate types"

exprExprExprToVec3 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprExprToVec3 (GLSLExpr GLFloat _ x) (GLSLExpr GLFloat _ y) (GLSLExpr GLFloat _ z) = GLSLExpr Vec3 False $ "vec3(" <> x <> "," <> y <> "," <> z <> ")"
exprExprExprToVec3 _ _ _ = error "exprExprExprToVec3 called with inappropriate types"

exprExprExprToVec4 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprExprToVec4 (GLSLExpr GLFloat _ x) (GLSLExpr GLFloat _ y) (GLSLExpr Vec2 _ z) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> "," <> z <> ")"
exprExprExprToVec4 (GLSLExpr GLFloat _ x) (GLSLExpr Vec2 _ y) (GLSLExpr GLFloat _ z) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> "," <> z <> ")"
exprExprExprToVec4 (GLSLExpr Vec2 _ x) (GLSLExpr GLFloat _ y) (GLSLExpr GLFloat _ z) = GLSLExpr Vec4 False $ "vec4(" <> x <> "," <> y <> "," <> z <> ")"
exprExprExprToVec4 _ _ _ = error "exprExprExprToVec4 called with inappropriate types"

exprExprExprExprToVec4 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprExprExprToVec4 (GLSLExpr GLFloat _ w) (GLSLExpr GLFloat _ x) (GLSLExpr GLFloat _ y) (GLSLExpr GLFloat _ z) = GLSLExpr Vec4 False $ "vec4(" <> w <> "," <> x <> "," <> y <> "," <> z <> ")"
exprExprExprExprToVec4 _ _ _ _ = error "exprExprExprExprToVec4 called with inappropriate types"

instance Num GLSLExpr where
  x + y = binaryOperatorMatched "+" x y
  x - y = binaryOperatorMatched "-" x y
  x * y = binaryOperatorMatched "*" x y
  abs x = unaryFunctionMatched "abs" x
  signum x = unaryFunctionMatched "sign" x
  fromInteger x = constantFloat $ fromIntegral x

instance Fractional GLSLExpr where
  fromRational = constantFloat . realToFrac
  x / y = binaryOperatorMatched "/" x y

lessThan :: GLSLExpr -> GLSLExpr -> GLSLExpr
lessThan = comparisonOperator "<" "lessThan"

lessThanEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
lessThanEqual = comparisonOperator "<=" "lessThanEqual"

greaterThan :: GLSLExpr -> GLSLExpr -> GLSLExpr
greaterThan = comparisonOperator ">" "greaterThan"

greaterThanEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
greaterThanEqual = comparisonOperator ">=" "greaterThanEqual"

equal :: GLSLExpr -> GLSLExpr -> GLSLExpr
equal = comparisonOperator "==" "equal"

notEqual :: GLSLExpr -> GLSLExpr -> GLSLExpr
notEqual = comparisonOperator "!=" "notEqual"

log2 :: GLSLExpr -> GLSLExpr
log2 = unaryFunctionMatched "log2"

log :: GLSLExpr -> GLSLExpr
log = unaryFunctionMatched "log"

pow :: GLSLExpr -> GLSLExpr -> GLSLExpr
pow x y = binaryFunctionMatched "pow" x y

mod :: GLSLExpr -> GLSLExpr -> GLSLExpr
mod x y = binaryFunctionMatched "mod" x y

distance :: GLSLExpr -> GLSLExpr -> GLSLExpr
distance x y = binaryFunction "distance" GLFloat x y

smoothstep :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
smoothstep edge0 edge1 x = ternaryFunction "smoothstep" (glslType x) edge0 edge1 x

glslMin :: GLSLExpr -> GLSLExpr -> GLSLExpr
glslMin = binaryFunctionMatched "min"

glslMax :: GLSLExpr -> GLSLExpr -> GLSLExpr
glslMax = binaryFunctionMatched "max"
-}
