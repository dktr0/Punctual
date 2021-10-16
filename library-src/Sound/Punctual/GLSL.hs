{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSL where

-- This module defines the monad GLSL for representing computations that
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

import Sound.Punctual.GLSLExpr


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

-- on the basis of assign, we can define a series of different "alignment" functions
-- that manipulate the underlying representation of a list of GLSLExpr-s...

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
