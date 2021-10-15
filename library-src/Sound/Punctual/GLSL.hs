{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSL where

-- This module provides types and functions to represent and manipulate the text of
-- GLSL fragment shaders, including the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")
-- A fundamental goal is elegantly bridging the gap between GLSL's 4 basic float types
-- and the multi-channel expressions of Punctual (which are not limited to 1-4 channels).

import Data.Text.Lazy as T hiding (zipWith,length,cycle)
import TextShow
import Data.Set as Set
import Data.IntMap as IntMap
import Control.Monad
import Control.Monad.State
import Data.Foldable as Foldable hiding (length)


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

-- As we write a fragment shader, basically we will be accumulating GLSLExpr(s) that are
-- assigned to variables in the underlying GLSL types. So we make a monad to represent
-- this sequential accumulation. The key operation in this monad is 'assign' which
-- assigns the contents of a GLSLExpr to an automagically-named GLSL variable, and returns
-- the (n.b. different) GLSLExpr that would be used to access that variable in subsequent operations.

type GLSL = State (IntMap GLSLExpr)

runGLSL :: GLSL a -> (a,IntMap GLSLExpr)
runGLSL x = runState x IntMap.empty

assign :: GLSLExpr -> GLSL GLSLExpr
assign x = do
  m <- get
  let n = IntMap.size m -- *note* this assumes items are not removed from the map prior to any assignment
  put $ IntMap.insert n x m
  return $ GLSLExpr {
    glslType = glslType x,
    builder = "_" <> showb n,
    deps = Set.insert n (deps x)
  }


instance Num GLSLExpr where
  x + y = binaryExprOp "+" x y
  x - y = binaryExprOp "-" x y
  x * y = binaryExprOp "*" x y
  abs x = unaryExprFunction "abs" x
  signum x = unaryExprFunction "sign" x
  fromInteger x = constantFloat $ fromIntegral x


-- produce a new GLSLExpr of the same underlying type by applying a function a -> a
unaryExprFunction :: Builder -> GLSLExpr -> GLSLExpr
unaryExprFunction funcName x = GLSLExpr {
  glslType = glslType x,
  builder = funcName <> "(" <> builder x <> ")",
  deps = deps x
  }

-- produce a new GLSLExpr by combining two GLSLExpr-s, eg. via arithmetic operators
-- note that result type follows same logic as binaryExprOp/binaryExprOpType below...
binaryExprFunction :: Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryExprFunction funcName e1 e2 = GLSLExpr {
  glslType = binaryExprOpType (glslType e1) (glslType e2),
  builder = funcName <> "(" <> builder e1 <> "," <> builder e2 <> ")",
  deps = Set.union (deps e1) (deps e2)
  }

-- produce a new GLSLExpr by combining two GLSLExpr-s, eg. via arithmetic operators
binaryExprOp :: Builder -> GLSLExpr -> GLSLExpr -> GLSLExpr
binaryExprOp op e1 e2 = GLSLExpr {
  glslType = binaryExprOpType (glslType e1) (glslType e2),
  builder = "(" <> builder e1 <> op <> builder e2 <> ")",
  deps = Set.union (deps e1) (deps e2)
  }

binaryExprOpType :: GLSLType -> GLSLType -> GLSLType
binaryExprOpType GLFloat GLFloat = GLFloat
binaryExprOpType Vec2 Vec2 = Vec2
binaryExprOpType GLFloat Vec2 = Vec2
binaryExprOpType Vec2 GLFloat = Vec2
binaryExprOpType Vec3 Vec3 = Vec3
binaryExprOpType GLFloat Vec3 = Vec3
binaryExprOpType Vec3 GLFloat = Vec3
binaryExprOpType Vec4 Vec4 = Vec4
binaryExprOpType GLFloat Vec4 = Vec4
binaryExprOpType Vec4 GLFloat = Vec4
binaryExprOpType _ _ = error "binaryExprOpType called with mismatched vector types"

exprChannels :: GLSLExpr -> Int
exprChannels (GLSLExpr Vec4 _ _) = 4
exprChannels (GLSLExpr Vec3 _ _) = 3
exprChannels (GLSLExpr Vec2 _ _) = 2
exprChannels (GLSLExpr GLFloat _ _) = 1

exprsChannels :: [GLSLExpr] -> Int
exprsChannels xs = sum $ fmap exprChannels xs

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

-- align: given a GLSLType and a list of GLSLExpr-s, produce a new list of
-- GLSLExpr-s where every expression is of the provided type, potentially
-- repeating "channels" at the end in order to fill out the last item.

align :: GLSLType -> [GLSLExpr] -> GLSL [GLSLExpr]
align _ [] = return []
align t xs = do
  (x,xs') <- splitAligned t xs
  xs'' <- align t xs'
  return (x:xs'')

-- alignMax: given a list of GLSLExpr-s regroup them to use the biggest
-- underlying types, eg. 7 channels would be a Vec4 followed by a Vec3
alignMax :: [GLSLExpr] -> GLSL [GLSLExpr]
alignMax xs
  | exprsChannels xs == 0 = return []
  | exprsChannels xs == 1 = (pure . fst) <$> splitAligned GLFloat xs
  | exprsChannels xs == 2 = (pure . fst) <$> splitAligned Vec2 xs
  | exprsChannels xs == 3 = (pure . fst) <$> splitAligned Vec3 xs
  | exprsChannels xs >= 4 = do
      (y,ys) <- splitAligned Vec4 xs
      ys' <- alignMax ys
      return (y:ys')

type AlignHint = Maybe GLSLType

alignHint :: AlignHint -> [GLSLExpr] -> GLSL [GLSLExpr]
alignHint Nothing xs = return xs -- or alignMax???
alignHint (Just t) xs = alignNoExtension t xs

-- like align, but doesn't repeat/extend to fill out types
-- instead, when there is not enough "data" to fill out a type
-- it uses alignMax above to at least produce the largest type possible
alignNoExtension :: GLSLType  -> [GLSLExpr] -> GLSL [GLSLExpr]
alignNoExtension _ [] = return []
alignNoExtension Vec3 xs | exprsChannels xs > 3 = do
  (x,xs') <- splitAligned Vec3 xs
  xs'' <- alignNoExtension Vec3 xs'
  return (x:xs'')
alignNoExtension Vec2 xs | exprsChannels xs >= 2  = do
    (x,xs') <- splitAligned Vec2 xs
    xs'' <- alignNoExtension Vec2 xs'
    return (x:xs'')
alignNoExtension Vec3 xs | exprsChannels xs >= 3 = do
  (x,xs') <- splitAligned Vec3 xs
  xs'' <- alignNoExtension Vec3 xs'
  return (x:xs'')
alignNoExtension Vec4 xs | exprsChannels xs >= 4 = do
  (x,xs') <- splitAligned Vec4 xs
  xs'' <- alignNoExtension Vec4 xs'
  return (x:xs'')
alignNoExtension _ xs = alignMax xs

-- splitAligned: given a GLSLType and a non-empty list of GLSLExpr-s, "pop" stuff from the
-- head of the list in such a way that a specific GLSLType is guaranteed, returning
-- both a GLSLExpr that is guaranteed to be of the requested type, and a list that
-- represents stuff not consumed by this "alignment" operation.

splitAligned :: GLSLType -> [GLSLExpr] -> GLSL (GLSLExpr,[GLSLExpr])
splitAligned _ [] = error "splitAligned called with empty list"

-- 1. when the requested type is at the head of the list, simply separate head & tail of list
splitAligned t (x:xs) | t == glslType x = return (x,xs)

-- 2. when the requested item can be constructed exactly from items at the head of the list in various ways, do that
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr GLFloat _ _):z@(GLSLExpr GLFloat _ _):xs) = do
  return (exprExprExprToVec3 x y z, xs)
-- *** TODO: there are many more patterns that should be matched here.

-- 3. when the list has one item that is smaller than requested type, repeat channels to provide type
splitAligned Vec2 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec2 x,[])
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec3 x,[])
splitAligned Vec3 (x@(GLSLExpr Vec2 _ _):[]) = return (exprToVec3 x,[])
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec4 x,[])
splitAligned Vec4 (x@(GLSLExpr Vec2 _ _):[]) = return (exprToVec4 x,[])
splitAligned Vec4 (x@(GLSLExpr Vec3 _ _):[]) = return (exprToVec4 x,[])


-- 4. when the requested item is smaller than the type at head of list, split it by assigning and swizzling
splitAligned GLFloat (x@(GLSLExpr Vec2 _ _):xs) = do
  x' <- assign x
  return (swizzleX x',(swizzleY x'):xs)
splitAligned GLFloat (x@(GLSLExpr Vec3 _ _):xs) = do
  x' <- assign x
  return (swizzleX x',(swizzleYZ x'):xs)
splitAligned GLFloat (x@(GLSLExpr Vec4 _ _):xs) = do
  x' <- assign x
  return (swizzleX x',(swizzleYZW x'):xs)
splitAligned Vec2 (x@(GLSLExpr Vec3 _ _):xs) = do
  x' <- assign x
  return (swizzleXY x',(swizzleZ x'):xs)
splitAligned Vec2 (x@(GLSLExpr Vec4 _ _):xs) = do
  x' <- assign x
  return (swizzleXY x',(swizzleZW x'):xs)
splitAligned Vec3 (x@(GLSLExpr Vec4 _ _):xs) = do
  x' <- assign x
  return (swizzleXYZ x',(swizzleW x'):xs)

-- 3. when the requested item is larger than the type at the head of the list (n>=2), call splitAligned
-- recursively to provide a second value that, combined with the first, produces requested type
splitAligned Vec2 (x@(GLSLExpr GLFloat _ _):xs) = do
  (y,xs') <- splitAligned GLFloat xs
  return (exprExprToVec2 x y,xs')
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):xs) = do
  (y,xs') <- splitAligned Vec2 xs
  return (exprExprToVec3 x y,xs')
splitAligned Vec3 (x@(GLSLExpr Vec2 _ _):xs) = do
  (y,xs') <- splitAligned GLFloat xs
  return (exprExprToVec3 x y,xs')
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):xs) = do
  (y,xs') <- splitAligned Vec3 xs
  return (exprExprToVec4 x y,xs')
splitAligned Vec4 (x@(GLSLExpr Vec2 _ _):xs) = do
  (y,xs') <- splitAligned Vec2 xs
  return (exprExprToVec4 x y,xs')
splitAligned Vec4 (x@(GLSLExpr Vec3 _ _):xs) = do
  (y,xs') <- splitAligned GLFloat xs
  return (exprExprToVec4 x y,xs')

-- this could use a better name...
alignExprs :: [GLSLExpr] -> [GLSLExpr] -> GLSL ([GLSLExpr],[GLSLExpr])
alignExprs x y = do
  let xChnls = exprsChannels x
  let yChnls = exprsChannels y
  let xIsModel = (xChnls > yChnls ) || ((xChnls == yChnls) && (length x <= length y))
  case xIsModel of
    True -> do
      y' <- alignToModel x (cycle y)
      return (x,y')
    False -> do
      x' <- alignToModel y (cycle x)
      return (x',y)

-- so could this (use a better name)...
alignExprsOptimized :: [GLSLExpr] -> [GLSLExpr] -> GLSL ([GLSLExpr],[GLSLExpr])
alignExprsOptimized x y
  | exprsChannels x == 1 = return (Prelude.replicate (length y) $ Prelude.head x,y)
  | exprsChannels y == 1 = return (x,Prelude.replicate (length x) $ Prelude.head y)
  | otherwise = do
  let xChnls = exprsChannels x
  let yChnls = exprsChannels y
  let xIsModel = (xChnls > yChnls ) || ((xChnls == yChnls) && (length x <= length y))
  case xIsModel of
    True -> do
      y' <- alignToModel x (cycle y)
      return (x,y')
    False -> do
      x' <- alignToModel y (cycle x)
      return (x',y)

-- this could also use a better name...
alignToModel :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
alignToModel [] _ = return []
alignToModel _ [] = error "alignToModel ran out of expressions in second argument"
alignToModel (m@(GLSLExpr GLFloat _ _):ms) xs = do
  (x',xs') <- splitAligned GLFloat xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(GLSLExpr Vec2 _ _):ms) xs = do
  (x',xs') <- splitAligned Vec2 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(GLSLExpr Vec3 _ _):ms) xs = do
  (x',xs') <- splitAligned Vec3 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(GLSLExpr Vec4 _ _):ms) xs = do
  (x',xs') <- splitAligned Vec4 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''


-- texture access is always assigned to variable, since it is an expensive operation
-- note: position arguments are bipolar (an implicit/internal conversion to unipolar is baked in)
texture2D :: Builder -> [GLSLExpr] -> GLSL [GLSLExpr]
texture2D n xy = do
  xy' <- align Vec2 xy
  mapM assign $ fmap (\x -> GLSLExpr Vec3 ("texture2D(" <> n <> ",fract(" <> builder x <> "*0.5+0.5)).xyz") (deps x)) xy'

-- not actually used anywhere yet?
addExprs :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
addExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (+) xs' ys'

-- not actually used anywhere yet?
subtractExprs :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
subtractExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (-) xs' ys'

-- not actually used anywhere yet? (apart from 'test' below)
multiplyExprs :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
multiplyExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (*) xs' ys'


test :: GLSL [GLSLExpr]
test = do
  w <- multiplyExprs [constantFloat 0.3,constantFloat 0.5,constantFloat 0.7] [constantFloat 2.0]
  -- ** TODO need to make Fractional instance for GLSLExpr to cleanup line above
  ts <- texture2D (showb (7 :: Int)) w
  align GLFloat ts

realizeAssignment :: Int -> GLSLExpr -> Builder
realizeAssignment n (GLSLExpr GLFloat b _) = "float _" <> showb n <> "=" <> b <> ";\n"
realizeAssignment n (GLSLExpr Vec2 b _) = "vec2 _" <> showb n <> "=" <> b <> ";\n"
realizeAssignment n (GLSLExpr Vec3 b _) = "vec3 _" <> showb n <> "=" <> b <> ";\n"
realizeAssignment n (GLSLExpr Vec4 b _) = "vec4 _" <> showb n <> "=" <> b <> ";\n"

realizeAssignments :: IntMap GLSLExpr -> Builder
realizeAssignments xs = Foldable.fold $ IntMap.mapWithKey realizeAssignment xs

realizeExpr :: GLSLExpr -> Builder
realizeExpr (GLSLExpr GLFloat b _) = showb b <> "\n"
realizeExpr (GLSLExpr Vec2 b _) = showb b <> "\n"
realizeExpr (GLSLExpr Vec3 b _) = showb b <> "\n"
realizeExpr (GLSLExpr Vec4 b _) = showb b <> "\n"

prettyPrint :: ([GLSLExpr],IntMap GLSLExpr) -> Text
prettyPrint (xs,vs) = toLazyText $ v <> x
  where v = Foldable.fold $ IntMap.mapWithKey realizeAssignment vs
        x = Foldable.fold $ fmap realizeExpr xs
