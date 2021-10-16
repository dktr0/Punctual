{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSLExpr where

-- This module provides types and functions to represent and manipulate the text of
-- GLSL expressions.

import TextShow
import Data.Set as Set

-- As we combine expressions in a series of GLSL statements, they may refer
-- to variables defined in previous statements. Keeping track of this will later allow us
-- to determine when some statements can be optimized away. We call these Deps
-- (for "dependencies") and represent the dependencies of a given expression like this:

type Deps = Set Int


-- GLSL is a strongly-typed language, so we'll need a type to represent GLSL types:

data GLSLType = Vec4 | Vec3 | Vec2 | GLFloat deriving (Eq, Show)


-- Next, we define the type GLSLExpr which represents a strongly-typed GLSL expression
-- together with a representation of its dependencies.

data GLSLExpr = GLSLExpr {
  glslType :: GLSLType,
  builder :: Builder,
  deps :: Deps
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
glFloat b = GLSLExpr GLFloat b Set.empty

-- and another one for doing the same thing from a Haskell Double
constantFloat :: Double -> GLSLExpr
constantFloat x = GLSLExpr GLFloat (showb x) Set.empty

-- unsafe because performs no checking that the indicated cast is valid GLSL
unsafeCast :: GLSLType -> GLSLExpr -> GLSLExpr
unsafeCast t x = GLSLExpr { glslType = t, builder = f t <> "(" <> builder x <> ")", deps = deps x }
  where
    f GLFloat = "float"
    f Vec2 = "vec2"
    f Vec3 = "vec3"
    f Vec4 = "vec4"

unaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr
unaryFunction funcName t x = GLSLExpr { glslType = t, builder = b, deps = deps x }
  where b = funcName <> "(" <> builder x <> ")"

binaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryFunction funcName t e1 e2 = GLSLExpr { glslType = t, builder = b, deps = Set.union (deps e1) (deps e2) }
  where b = funcName <> "(" <> builder e1 <> "," <> builder e2 <> ")"

binaryOperator :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryOperator op t e1 e2 = GLSLExpr { glslType = t, builder = b, deps = Set.union (deps e1) (deps e2) }
  where b = "(" <> builder e1 <> op <> builder e2 <> ")"

ternaryFunction :: Builder -> GLSLType -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
ternaryFunction funcName t e1 e2 e3 = GLSLExpr { glslType = t, builder = b, deps = Set.union (deps e1) $ Set.union (deps e2) (deps e3) }
  where b = funcName <> "(" <> builder e1 <> "," <> builder e2 <> "," <> builder e3 <> ")"


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
swizzleX x = GLSLExpr GLFloat (builder x <> ".x") $ deps x

swizzleY :: GLSLExpr -> GLSLExpr
swizzleY x = GLSLExpr GLFloat (builder x <> ".y") $ deps x

swizzleZ :: GLSLExpr -> GLSLExpr
swizzleZ x = GLSLExpr GLFloat (builder x <> ".z") $ deps x

swizzleW :: GLSLExpr -> GLSLExpr
swizzleW x = GLSLExpr GLFloat (builder x <> ".w") $ deps x

swizzleXY :: GLSLExpr -> GLSLExpr
swizzleXY x = GLSLExpr Vec2 (builder x <> ".xy") $ deps x

swizzleYZ :: GLSLExpr -> GLSLExpr
swizzleYZ x = GLSLExpr Vec2 (builder x <> ".yz") $ deps x

swizzleZW :: GLSLExpr -> GLSLExpr
swizzleZW x = GLSLExpr Vec2 (builder x <> ".zw") $ deps x

swizzleXYZ :: GLSLExpr -> GLSLExpr
swizzleXYZ x = GLSLExpr Vec3 (builder x <> ".xyz") $ deps x

swizzleYZW :: GLSLExpr -> GLSLExpr
swizzleYZW x = GLSLExpr Vec3 (builder x <> ".yzw") $ deps x

swizzleXYY :: GLSLExpr -> GLSLExpr
swizzleXYY x = GLSLExpr Vec3 (builder x <> ".xyy") $ deps x

swizzleXYYY :: GLSLExpr -> GLSLExpr
swizzleXYYY x = GLSLExpr Vec4 (builder x <> ".xyyy") $ deps x

swizzleXYZZ :: GLSLExpr -> GLSLExpr
swizzleXYZZ x = GLSLExpr Vec4 (builder x <> ".xyzz") $ deps x

-- convert any GLSLExpr to an GLSLExpr containing a GLFloat, by discarding "channels" after the first one
exprToGLFloat :: GLSLExpr -> GLSLExpr
exprToGLFloat x
  | glslType x == GLFloat = x
  | otherwise = swizzleX x

-- convert any GLSLExpr to an GLSLExpr containing a Vec2, by repeating or discarding channels
exprToVec2 :: GLSLExpr -> GLSLExpr
exprToVec2 x
  | glslType x == GLFloat = GLSLExpr Vec2 ("vec2(" <> builder x <> ")") $ deps x
  | glslType x == Vec2 = x
  | otherwise = swizzleXY x

-- convert any GLSLExpr to an GLSLExpr containing a Vec3, by repeating or discarding channels
exprToVec3 :: GLSLExpr -> GLSLExpr
exprToVec3 x
  | glslType x == GLFloat = GLSLExpr Vec3 ("vec3(" <> builder x <> ")") $ deps x
  | glslType x == Vec2 = swizzleXYY x
  | glslType x == Vec3 = x
  | otherwise = swizzleXYZ x

-- convert any GLSLExpr to an GLSLExpr containing a Vec4, by repeating channels
exprToVec4 :: GLSLExpr -> GLSLExpr
exprToVec4 x
  | glslType x == GLFloat = GLSLExpr Vec4 ("vec4(" <> builder x <> ")") $ deps x
  | glslType x == Vec2 = swizzleXYYY x
  | glslType x == Vec3 = swizzleXYZZ x
  | otherwise = x



-- assemble a GLSLExpr containing a Vec2 by combining two GLSLExpr-s
exprExprToVec2 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec2 (GLSLExpr GLFloat x xDeps) (GLSLExpr GLFloat y yDeps) = GLSLExpr Vec2 b $ Set.union xDeps yDeps
  where b = "vec2(" <> x <> "," <> y <> ")"
exprExprToVec2 _ _ = error "exprExprToVec2 called with inappropriate types"

-- assemble a GLSLExpr containing a Vec3 by combining two GLSLExpr-s
exprExprToVec3 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec3 (GLSLExpr GLFloat x xDeps) (GLSLExpr Vec2 y yDeps) = GLSLExpr Vec3 b $ Set.union xDeps yDeps
  where b = "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 (GLSLExpr Vec2 x xDeps) (GLSLExpr GLFloat y yDeps) = GLSLExpr Vec3 b (Set.union xDeps yDeps)
  where b = "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 _ _ = error "exprExprToVec3 called with inappropriate types"

-- assemble a GLSLExpr containing a Vec4 by combining two GLSLExpr-s
exprExprToVec4 :: GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprToVec4 (GLSLExpr GLFloat x xDeps) (GLSLExpr Vec3 y yDeps) = GLSLExpr Vec4 b $ Set.union xDeps yDeps
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (GLSLExpr Vec2 x xDeps) (GLSLExpr Vec2 y yDeps) = GLSLExpr Vec4 b $ Set.union xDeps yDeps
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (GLSLExpr Vec3 x xDeps) (GLSLExpr GLFloat y yDeps) = GLSLExpr Vec4 b $ Set.union xDeps yDeps
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 _ _ = error "exprExprToVec4 called with inappropriate types"

exprExprExprToVec3 :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
exprExprExprToVec3 (GLSLExpr GLFloat x xDeps) (GLSLExpr GLFloat y yDeps) (GLSLExpr GLFloat z zDeps) = GLSLExpr {
  glslType = Vec3,
  builder = "vec3(" <> x <> "," <> y <> "," <> z <> ")",
  deps = Set.union xDeps $ Set.union yDeps zDeps
  }


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

log2 :: GLSLExpr -> GLSLExpr
log2 = unaryFunctionMatched "log2"

log :: GLSLExpr -> GLSLExpr
log = unaryFunctionMatched "log"

pow :: GLSLExpr -> GLSLExpr -> GLSLExpr
pow x y = binaryFunctionMatched "pow" x y

distance :: GLSLExpr -> GLSLExpr -> GLSLExpr
distance x y = binaryFunction "distance" GLFloat x y
