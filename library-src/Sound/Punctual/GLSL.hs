{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSL where

-- This module provides types and functions to represent and manipulate the text of
-- GLSL fragment shaders, including the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")
-- A fundamental goal is elegantly bridging the gap between GLSL's 4 basic float types
-- and the multi-channel expressions of Punctual (which are not limited to 1-4 channels).

import Data.Text as T hiding (zipWith,length)
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


-- Next, we define the type Expr which could represent any of the basic GLSL types, as
-- a combination of a (Text) Builder that represents the GLSL text of a given expression,
-- together with a representation of its dependencies.

data Expr =
  Vec4 Builder Deps |
  Vec3 Builder Deps |
  Vec2 Builder Deps |
  GLFloat Builder Deps
  deriving (Show)

builder :: Expr -> Builder
builder (Vec4 x _) = x
builder (Vec3 x _) = x
builder (Vec2 x _) = x
builder (GLFloat x _) = x

deps :: Expr -> Deps
deps (Vec4 _ x) = x
deps (Vec3 _ x) = x
deps (Vec2 _ x) = x
deps (GLFloat _ x) = x

-- As we write a fragment shader, basically we will be accumulating Expr(s) that are
-- assigned to variables in the underlying GLSL types. So we make a monad to represent
-- this sequential accumulation. The key operation in this monad is 'assign' which
-- assigns the contents of an Expr to an automagically-named GLSL variable, and returns
-- the (n.b. different) Expr that would be used to access that variable in subsequent operations.

type GLSL = State (IntMap Expr)

runGLSL :: GLSL a -> (a,IntMap Expr)
runGLSL x = runState x IntMap.empty

assign :: Expr -> GLSL Expr
assign x = do
  m <- get
  let n = IntMap.size m -- *note* this assumes items are not removed from the map prior to any assignment
  put $ IntMap.insert n x m
  return $ case x of
    (Vec4 _ deps) -> Vec4 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (Vec3 _ deps) -> Vec3 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (Vec2 _ deps) -> Vec2 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (GLFloat _ deps) -> GLFloat ("_" <> showb n) $ Set.union deps $ Set.singleton n



instance Num Expr where
  x + y = binaryExprOp "+" x y
  x - y = binaryExprOp "-" x y
  x * y = binaryExprOp "*" x y
  abs x = exprFunction "abs" x
  signum x = exprFunction "sign" x
  fromInteger x = constantFloat $ fromIntegral x

-- produce a new Expr of the same underlying type by applying a function a -> a
exprFunction :: Builder -> Expr -> Expr
exprFunction b (GLFloat x xDeps) = GLFloat (b <> "(" <> x <> ")") xDeps
exprFunction b (Vec2 x xDeps) = Vec2 (b <> "(" <> x <> ")") xDeps
exprFunction b (Vec3 x xDeps) = Vec3 (b <> "(" <> x <> ")") xDeps
exprFunction b (Vec4 x xDeps) = Vec4 (b <> "(" <> x <> ")") xDeps

binaryExprOp :: Builder -> Expr -> Expr -> Expr
binaryExprOp op (GLFloat a aDeps) (GLFloat b bDeps) = GLFloat ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec2 a aDeps) (Vec2 b bDeps) = Vec2 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (GLFloat a aDeps) (Vec2 b bDeps) = Vec2 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec2 a aDeps) (GLFloat b bDeps) = Vec2 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec3 a aDeps) (Vec3 b bDeps) = Vec3 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (GLFloat a aDeps) (Vec3 b bDeps) = Vec3 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec3 a aDeps) (GLFloat b bDeps) = Vec3 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec4 a aDeps) (Vec4 b bDeps) = Vec4 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (GLFloat a aDeps) (Vec4 b bDeps) = Vec4 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op (Vec4 a aDeps) (GLFloat b bDeps) = Vec4 ("(" <> a <> op <> b <> ")") $ Set.union aDeps bDeps
binaryExprOp op _ _ = error "binaryExprOp called with mismatched vector types"

exprChannels :: Expr -> Int
exprChannels (Vec4 _ _) = 4
exprChannels (Vec3 _ _) = 3
exprChannels (Vec2 _ _) = 2
exprChannels (GLFloat _ _) = 1

exprsChannels :: [Expr] -> Int
exprsChannels xs = sum $ fmap exprChannels xs

swizzleX x = GLFloat (builder x <> ".x") (deps x)
swizzleY x = GLFloat (builder x <> ".y") (deps x)
swizzleZ x = GLFloat (builder x <> ".z") (deps x)
swizzleW x = GLFloat (builder x <> ".w") (deps x)
swizzleXY x = Vec2 (builder x <> ".xy") (deps x)
swizzleYZ x = Vec2 (builder x <> ".yz") (deps x)
swizzleZW x = Vec2 (builder x <> ".zw") (deps x)
swizzleXYZ x = Vec3 (builder x <> ".xyz") (deps x)
swizzleYZW x = Vec3 (builder x <> ".yzw") (deps x)
swizzleXYY x = Vec3 (builder x <> ".xyy") (deps x)
swizzleXYYY x = Vec4 (builder x <> ".xyyy") (deps x)
swizzleXYZZ x = Vec4 (builder x <> ".xyzz") (deps x)

exprToGLFloat :: Expr -> Expr
exprToGLFloat x@(GLFloat _ _) = x
exprToGLFloat x@(Vec2 _ _) = swizzleX x
exprToGLFloat x@(Vec3 _ _) = swizzleX x
exprToGLFloat x@(Vec4 _ _) = swizzleX x

exprToVec2 :: Expr -> Expr
exprToVec2 (GLFloat b deps) = Vec2 ("vec2(" <> b <> ")") deps
exprToVec2 x@(Vec2 _ _) = x
exprToVec2 x@(Vec3 _ _) = swizzleXY x
exprToVec2 x@(Vec4 _ _) = swizzleXY x

exprToVec3 :: Expr -> Expr
exprToVec3 (GLFloat b deps) = Vec3 ("vec3(" <> b <> ")") deps
exprToVec3 x@(Vec2 _ _) = swizzleXYY x
exprToVec3 x@(Vec3 _ _) = x
exprToVec3 x@(Vec4 _ _) = swizzleXYZ x

exprToVec4 :: Expr -> Expr
exprToVec4 (GLFloat b deps) = Vec4 ("vec4(" <> b <> ")") deps
exprToVec4 x@(Vec2 _ _) = swizzleXYYY x
exprToVec4 x@(Vec3 _ _) = swizzleXYZZ x
exprToVec4 x@(Vec4 _ _) = x

exprExprToVec2 :: Expr -> Expr -> Expr
exprExprToVec2 (GLFloat x xDeps) (GLFloat y yDeps) = Vec2 b (Set.union xDeps yDeps)
  where b = "vec2(" <> x <> "," <> y <> ")"
exprExprToVec2 _ _ = error "exprExprToVec2 called with inappropriate types"

exprExprToVec3 :: Expr -> Expr -> Expr
exprExprToVec3 (GLFloat x xDeps) (Vec2 y yDeps) = Vec3 b (Set.union xDeps yDeps)
  where b = "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 (Vec2 x xDeps) (GLFloat y yDeps) = Vec3 b (Set.union xDeps yDeps)
  where b = "vec3(" <> x <> "," <> y <> ")"
exprExprToVec3 _ _ = error "exprExprToVec3 called with inappropriate types"

exprExprToVec4 :: Expr -> Expr -> Expr
exprExprToVec4 (GLFloat x xDeps) (Vec3 y yDeps) = Vec4 b (Set.union xDeps yDeps)
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (Vec2 x xDeps) (Vec2 y yDeps) = Vec4 b (Set.union xDeps yDeps)
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 (Vec3 x xDeps) (GLFloat y yDeps) = Vec4 b (Set.union xDeps yDeps)
  where b = "vec4(" <> x <> "," <> y <> ")"
exprExprToVec4 _ _ = error "exprExprToVec4 called with inappropriate types"


-- given any Expr, coerce it into a [Expr] containing a GLFloat for each channel of the
-- provided value, regardless of the original underlying types.
exprToGLFloats :: Expr -> GLSL [Expr]
exprToGLFloats x@(GLFloat _ _) = return [x]
exprToGLFloats x@(Vec2 _ _) = do
  x' <- assign x
  return [swizzleX x',swizzleY x']
exprToGLFloats x@(Vec3 _ _) = do
  x' <- assign x
  return [swizzleX x',swizzleY x',swizzleZ x']
exprToGLFloats x@(Vec4 _ _) = do
  x' <- assign x
  return [swizzleX x',swizzleY x',swizzleZ x',swizzleW x']


constantFloat :: Double -> Expr
constantFloat x = GLFloat (showb x) Set.empty

sumExprs :: [Expr] -> Expr
sumExprs [] = error "sumExprs can't be used with empty [Expr]"
sumExprs xs = Foldable.foldr1 (+) xs

toGLFloat :: [Expr] -> GLSL Expr
toGLFloat [] = error "toGLFloat can't be used with empty [Expr]"
toGLFloat xs = toGLFloats xs >>= (return . sumExprs)

toVec2 :: [Expr] -> GLSL Expr
toVec2 [] = error "toVec2 can't be used with empty [Expr]"
toVec2 xs = toVec2s xs >>= (return . sumExprs)

toVec3 :: [Expr] -> GLSL Expr
toVec3 [] = error "toVec3 can't be used with empty [Expr]"
toVec3 xs = toVec3s xs >>= (return . sumExprs)

toVec4 :: [Expr] -> GLSL Expr
toVec4 [] = error "toVec4 can't be used with empty [Expr]"
toVec4 xs = toVec4s xs >>= (return . sumExprs)


-- whatever the input types are, divide their channels into individual GLFloats
-- if the input Exprs is empty, so is the result
toGLFloats :: [Expr] -> GLSL [Expr]
toGLFloats [] = return []
toGLFloats (x@(GLFloat _ _):xs) = do
  xs' <- toGLFloats xs
  return (x:xs')
toGLFloats (x@(Vec2 _ _):xs) = do
  x' <- assign x
  xs' <- toGLFloats xs
  return $ (swizzleX x'):(swizzleY x'):xs'
toGLFloats (x@(Vec3 _ _):xs) = do
  x' <- assign x
  xs' <- toGLFloats xs
  return $ (swizzleX x'):(swizzleY x'):(swizzleZ x'):xs'
toGLFloats (x@(Vec4 _ _):xs) = do
  x' <- assign x
  xs' <- toGLFloats xs
  return $ (swizzleX x'):(swizzleY x'):(swizzleZ x'):(swizzleW x'):xs'


-- whatever the input types are, produce Vec2s/Vec3s/Vec4s by realigning into groups of 2/3/4
-- "missing channels" to complete an incomplete final group are repetitions of last value
-- if the input is empty, so is the result
toVec2s :: [Expr] -> GLSL [Expr]
toVec2s [] = return []
toVec2s xs = do
  (x,xs') <- takeVec2 xs
  xs'' <- toVec2s xs'
  return $ x : xs''

toVec3s :: [Expr] -> GLSL [Expr]
toVec3s [] = return []
toVec3s xs = do
  (x,xs') <- takeVec3 xs
  xs'' <- toVec3s xs'
  return $ x : xs''

toVec4s :: [Expr] -> GLSL [Expr]
toVec4s [] = return []
toVec4s xs = do
  (x,xs') <- takeVec4 xs
  xs'' <- toVec4s xs'
  return $ x : xs''

takeGLFloat :: [Expr] -> GLSL (Expr,[Expr])
takeGLFloat [] = error "takeGLFloat called with empty list"
takeGLFloat (x@(GLFloat _ _):xs) = return (x,xs)
takeGLFloat (x@(Vec2 _ _):xs) = do
  x' <- assign x
  let a = swizzleX x'
  let b = swizzleY x'
  return (a,b:xs)
takeGLFloat (x@(Vec3 _ _):xs) = do
  x' <- assign x
  let a = swizzleX x'
  let b = swizzleYZ x'
  return (a,b:xs)
takeGLFloat (x@(Vec4 _ _):xs) = do
  x' <- assign x
  let a = swizzleX x'
  let b = swizzleYZW x'
  return (a,b:xs)

takeVec2 :: [Expr] -> GLSL (Expr,[Expr])
takeVec2 [] = error "takeVec2 called with empty list"
takeVec2 (x@(GLFloat _ _):[]) = return (exprToVec2 x,[])
takeVec2 (x@(GLFloat _ _):xs) = do
  (y,xs') <- takeGLFloat xs
  return (exprExprToVec2 x y,xs')
takeVec2 (x@(Vec2 _ _):xs) = return (x,xs)
takeVec2 (x@(Vec3 _ _):xs) = do
  x' <- assign x
  let a = swizzleXY x'
  let b = swizzleZ x'
  return (a,b:xs)
takeVec2 (x@(Vec4 _ _):xs) = do
  x' <- assign x
  let a = swizzleXY x'
  let b = swizzleZW x'
  return (a,b:xs)

takeVec3 :: [Expr] -> GLSL (Expr,[Expr])
takeVec3 [] = error "takeVec3 called with empty list"
takeVec3 (x@(GLFloat _ _):[]) = return (exprToVec3 x,[])
takeVec3 (x@(GLFloat _ _):xs) = do
  (y,xs') <- takeVec2 xs
  return (exprExprToVec3 x y,xs')
takeVec3 (x@(Vec2 _ _):[]) = return (exprToVec3 x,[])
takeVec3 (x@(Vec2 _ _):xs) = do
  (y,xs') <- takeGLFloat xs
  return (exprExprToVec3 x y,xs')
takeVec3 (x@(Vec3 _ _):xs) = return (x,xs)
takeVec3 (x@(Vec4 _ _):xs) = do
  x' <- assign x
  let a = swizzleXYZ x'
  let b = swizzleW x'
  return (a,b:xs)

takeVec4 :: [Expr] -> GLSL (Expr,[Expr])
takeVec4 [] = error "takeVec4 called with empty list"
takeVec4 (x@(GLFloat _ _):[]) = return (exprToVec4 x,[])
takeVec4 (x@(GLFloat _ _):xs) = do
  (y,xs') <- takeVec3 xs
  return (exprExprToVec4 x y,xs')
takeVec4 (x@(Vec2 _ _):[]) = return (exprToVec4 x,[])
takeVec4 (x@(Vec2 _ _):xs) = do
  (y,xs') <- takeVec2 xs
  return (exprExprToVec4 x y,xs')
takeVec4 (x@(Vec3 _ _):[]) = return (exprToVec4 x,[])
takeVec4 (x@(Vec3 _ _):xs) = do
  (y,xs') <- takeGLFloat xs
  return (exprExprToVec4 x y,xs')
takeVec4 (x@(Vec4 _ _):xs) = return (x,xs)


alignExprs :: [Expr] -> [Expr] -> GLSL ([Expr],[Expr])
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

alignToModel :: [Expr] -> [Expr] -> GLSL [Expr]
alignToModel [] _ = return []
alignToModel _ [] = error "alignToModel ran out of expressions in second argument"
alignToModel (m@(GLFloat _ _):ms) xs = do
  (x',xs') <- takeGLFloat xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(Vec2 _ _):ms) xs = do
  (x',xs') <- takeVec2 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(Vec3 _ _):ms) xs = do
  (x',xs') <- takeVec3 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''
alignToModel (m@(Vec4 _ _):ms) xs = do
  (x',xs') <- takeVec4 xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''


-- texture access is always assigned to variable, since it is an expensive operation
texture2D :: Int -> [Expr] -> GLSL [Expr]
texture2D n xs = do
  xs' <- toVec2s xs
  mapM assign $ fmap (\x -> Vec3 ("texture2D(" <> showb n <> "," <> builder x <> ").xyz") (deps x)) xs'

addExprs :: [Expr] -> [Expr] -> GLSL [Expr]
addExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (+) xs' ys'

subtractExprs :: [Expr] -> [Expr] -> GLSL [Expr]
subtractExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (-) xs' ys'

multiplyExprs :: [Expr] -> [Expr] -> GLSL [Expr]
multiplyExprs xs ys = do
  (xs',ys') <- alignExprs xs ys
  return $ zipWith (*) xs' ys'

test :: GLSL [Expr]
test = do
  let x = constantFloat 0.3
  let y = constantFloat 0.5
  let z = constantFloat 0.7
  w <- multiplyExprs [x,y,z] [constantFloat 2.0]
  texture2D 7 w
