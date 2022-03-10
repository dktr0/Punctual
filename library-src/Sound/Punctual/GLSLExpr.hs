{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSLExpr where

-- This module provides types and functions to represent and manipulate the text of
-- GLSL expressions.

import TextShow


-- GLSL is a strongly-typed language, so we'll need a type to represent GLSL types:

data GLSLType = Vec4 | Vec3 | Vec2 | GLFloat deriving (Eq, Show)

instance TextShow GLSLType where
  showb Vec4 = "vec4"
  showb Vec3 = "vec3"
  showb Vec2 = "vec2"
  showb GLFloat = "float"

-- Next, we define the type GLSLExpr which represents a strongly-typed GLSL expression
-- together with a representation of its dependencies.

data GLSLExpr = GLSLExpr {
  glslType :: GLSLType,
  isAssignment :: Bool, -- True if expr is just name of an assigned variable
  builder :: Builder
  } deriving (Show)

exprChannels :: GLSLExpr -> Int
exprChannels (GLSLExpr Vec4 _ _) = 4
exprChannels (GLSLExpr Vec3 _ _) = 3
exprChannels (GLSLExpr Vec2 _ _) = 2
exprChannels (GLSLExpr GLFloat _ _) = 1

exprsChannels :: [GLSLExpr] -> Int
exprsChannels xs = sum $ fmap exprChannels xs

-- a convenience function for making a no-dependency GLFloat
glFloat :: Builder -> GLSLExpr
glFloat b = GLSLExpr GLFloat True b

-- and another one for doing the same thing from a Haskell Double
constantFloat :: Double -> GLSLExpr
constantFloat x = GLSLExpr GLFloat True (showb x)

-- unsafe because performs no checking that the indicated cast is valid GLSL
unsafeCast :: GLSLType -> GLSLExpr -> GLSLExpr
unsafeCast t x = GLSLExpr t False $ f t <> "(" <> builder x <> ")"
  where
    f GLFloat = "float"
    f Vec2 = "vec2"
    f Vec3 = "vec3"
    f Vec4 = "vec4"

unaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr
unaryFunction funcName t x = GLSLExpr t False $ funcName <> "(" <> builder x <> ")"

binaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunction funcName t e1 e2 = GLSLExpr t False $ funcName <> "(" <> builder e1 <> "," <> builder e2 <> ")"

binaryOperator :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryOperator op t e1 e2 = GLSLExpr t False $ "(" <> builder e1 <> op <> builder e2 <> ")"

ternaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
ternaryFunction funcName t e1 e2 e3 = GLSLExpr t False $ funcName <> "(" <> builder e1 <> "," <> builder e2 <> "," <> builder e3 <> ")"


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

distance :: GLSLExpr -> GLSLExpr -> GLSLExpr
distance x y = binaryFunction "distance" GLFloat x y

smoothstep :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
smoothstep edge0 edge1 x = ternaryFunction "smoothstep" (glslType x) edge0 edge1 x

glslMin :: GLSLExpr -> GLSLExpr -> GLSLExpr
glslMin = binaryFunctionMatched "min"

glslMax :: GLSLExpr -> GLSLExpr -> GLSLExpr
glslMax = binaryFunctionMatched "max"
