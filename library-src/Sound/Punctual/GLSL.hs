{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Sound.Punctual.GLSL where

-- This module defines the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")
-- A fundamental goal is elegantly bridging the gap between GLSL's 4 basic float types
-- and the multi-channel expressions of Punctual (which are not limited to 1-4 channels).

import TextShow
import Data.Map as Map
import Control.Monad.State
import Data.Foldable as Foldable hiding (length)

import Sound.Punctual.GLSLExpr


-- GLSL is a monad representing computations in a GLSL fragment shader
-- a computation "accumulates" the following:
-- -a count of auto-assigned variables, so that new assignments can use that as necessary
-- -a Map from variable names to GLSLType-s
-- -accumulated Builder text of fragment shader code

type GLSLState = (Int,Map Builder GLSLType,Builder)

type GLSL = State GLSLState

runGLSL :: GLSL a -> (a,Builder)
runGLSL x = (a,b)
  where (a,(_,_,b)) = runState x (0,Map.empty,"")

assign :: GLSLExpr -> GLSL GLSLExpr
assign x
  | isAssignment x = return x -- don't re-assign
  | otherwise = do
  (c,m,b) <- get
  let varName = "_" <> showb c
  let varType = glslType x
  let m' = Map.insert varName varType m
  let newLine = showb varType <> " " <> varName <> "=" <> builder x <> ";\n"
  let b' = b <> newLine
  put (c+1,m',b')
  return $ GLSLExpr varType True varName

-- write code to the accumulated Builder without adding a new variable assignment
write :: Builder -> GLSL ()
write x = do
  (c,m,b) <- get
  put (c,m,b <> x)

-- create a variable of a given type and initialize it to 0. then conditionally
-- execute a block of other code (GLSL GLSLExpr) assigning its result to the
-- previously created variable.
assignConditional :: Builder -> GLSL GLSLExpr -> GLSL GLSLExpr
assignConditional condition x = mdo -- ?? will this work ?? if not, I guess we do some kind of sandboxed run of x to extract type...
  let t = glslType x'
  r <- assign $ unsafeCast t $ constantFloat 0
  write $ "if(" <> condition <> ") {\n"
  x' <- x
  write $ builder r <> "=" <> builder x' <> "\n}\n"
  return r

{-
g :: GLSLType -> Double -> Double -> GLSL GLSLExpr -> GLSL GLSLExpr -> GLSL GLSLExpr
g t t1 t2 x1 x2 = do
  x1' <- assignConditional t ("t<" <> showb t2) x1
  x2' <- assignConditional t ("t>=" <> showb t1) x2
  assign $ (x1' * xFadeOld ...) + (x2' * xFadeNew ...)
-}



-- on the basis of assign, we can define a series of different "alignment" functions
-- that manipulate the underlying representation of a list of GLSLExpr-s...

-- align: given a GLSLType and a list of GLSLExpr-s, produce a new list of
-- GLSLExpr-s where every expression is of the provided type, potentially
-- repeating "channels" at the end in order to fill out the last item.

align :: GLSLType -> [GLSLExpr] -> GLSL [GLSLExpr]
align _ [] = return []
align t (x:xs) | glslType x == t = do
  xs' <- align t xs
  return (x:xs')
align GLFloat (x@(GLSLExpr Vec2 _ _):xs) = do
  x' <- assign x
  xs' <- align GLFloat xs
  return (swizzleX x' : swizzleY x' : xs')
align GLFloat (x@(GLSLExpr Vec3 _ _):xs) = do
  x' <- assign x
  xs' <- align GLFloat xs
  return (swizzleX x' : swizzleY x' : swizzleZ x' : xs')
align GLFloat (x@(GLSLExpr Vec4 _ _):xs) = do
  x' <- assign x
  xs' <- align GLFloat xs
  return (swizzleX x' : swizzleY x' : swizzleZ x' : swizzleW x' : xs')
-- there are more optimized cases to match above
-- but for now they can all be covered with the catch-all definition below
-- which leads to unnecessary extra layers of swizzling and/or assignment:
align t xs = do
  (x,xs') <- splitAligned t xs
  xs'' <- align t xs'
  return (x:xs'')


-- | alignRGBA aligns to groups of 4 channels (vec4) as follows:
-- 1 channel: repeat as channel 2 and 3, set channel 4 (alpha) to 1
-- 2 channels: repeat channel 2 as 3, set channel 4 (alpha) to 1
-- 3 channels: set channel 4 (alpha) to 1
-- 4 channels: identity

alignRGBA :: [GLSLExpr] -> GLSL [GLSLExpr]
alignRGBA xs
  | exprsChannels xs == 0 = return []
  | exprsChannels xs == 1 = return [ exprExprToVec4 (exprToVec3 (Prelude.head xs)) $ constantFloat 1.0 ]
  | exprsChannels xs == 2 = do
      x' <- align Vec2 xs >>= (assign . Prelude.head)
      return [ exprExprToVec4 (exprExprToVec3 x' $ swizzleY x') $ constantFloat 1.0 ]
  | exprsChannels xs == 3 = return [ exprExprToVec4 (Prelude.head xs) $ constantFloat 1.0]
  | exprsChannels xs >= 4 = do
      (y,ys) <- splitAligned Vec4 xs
      ys' <- alignRGBA ys
      return (y:ys')


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

-- 1. when the requested item can be constructed exactly from one or more items at the head of the list in various ways, do that
splitAligned GLFloat (x@(GLSLExpr GLFloat _ _):xs) = return (x,xs)
splitAligned Vec2 (x@(GLSLExpr Vec2 _ _):xs) = return (x,xs)
splitAligned Vec2 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr GLFloat _ _):xs) = return (exprExprToVec2 x y, xs)
splitAligned Vec3 (x@(GLSLExpr Vec3 _ _):xs) = return (x,xs)
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr GLFloat _ _):z@(GLSLExpr GLFloat _ _):xs) = return (exprExprExprToVec3 x y z, xs)
splitAligned Vec3 (x@(GLSLExpr Vec2 _ _):y@(GLSLExpr GLFloat _ _):xs) = return (exprExprToVec3 x y, xs)
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr Vec2 _ _):xs) = return (exprExprToVec3 x y, xs)
splitAligned Vec4 (x@(GLSLExpr Vec4 _ _):xs) = return (x,xs)
splitAligned Vec4 (w@(GLSLExpr GLFloat _ _):x@(GLSLExpr GLFloat _ _):y@(GLSLExpr GLFloat _ _):z@(GLSLExpr GLFloat _ _):xs) = return (exprExprExprExprToVec4 w x y z, xs)
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr GLFloat _ _):z@(GLSLExpr Vec2 _ _):xs) = return (exprExprExprToVec4 x y z, xs)
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr Vec2 _ _):z@(GLSLExpr GLFloat _ _):xs) = return (exprExprExprToVec4 x y z, xs)
splitAligned Vec4 (x@(GLSLExpr Vec2 _ _):y@(GLSLExpr GLFloat _ _):z@(GLSLExpr GLFloat _ _):xs) = return (exprExprExprToVec4 x y z, xs)
splitAligned Vec4 (x@(GLSLExpr Vec2 _ _):y@(GLSLExpr Vec2 _ _):xs) = return (exprExprToVec4 x y, xs)
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):y@(GLSLExpr Vec3 _ _):xs) = return (exprExprToVec4 x y, xs)
splitAligned Vec4 (x@(GLSLExpr Vec3 _ _):y@(GLSLExpr GLFloat _ _):xs) = return (exprExprToVec4 x y, xs)

-- 2. when the list has one item that is smaller than requested type, repeat channels to provide type
splitAligned Vec2 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec2 x,[])
splitAligned Vec3 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec3 x,[])
splitAligned Vec3 (x@(GLSLExpr Vec2 _ _):[]) = return (exprToVec3 x,[])
splitAligned Vec4 (x@(GLSLExpr GLFloat _ _):[]) = return (exprToVec4 x,[])
splitAligned Vec4 (x@(GLSLExpr Vec2 _ _):[]) = return (exprToVec4 x,[])
splitAligned Vec4 (x@(GLSLExpr Vec3 _ _):[]) = return (exprToVec4 x,[])

-- 3. when the requested item is smaller than the type at head of list, split it by assigning and swizzling
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

-- 4. when the requested item is larger than the type at the head of the list (n>=2), call splitAligned
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
  
  
-- | mix takes two [GLSLExpr] which might represent different numbers of channels
-- it mixes them together to produce a result which has as many channels as the input
-- with the most channels, effectively treating the shorter input as 0 when channels "run out"
-- the result is aligned to the mode of whichever input was (originally) longer
mix :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
mix xs ys = do
  let xChnls = exprsChannels xs
  let yChnls = exprsChannels ys
  let nChnls = max xChnls yChnls
  let xs' = extendWithZeros nChnls xs
  let ys' = extendWithZeros nChnls ys
  let xIsModel = nChnls == xChnls 
  (xs'',ys'') <- case xIsModel of
    True -> do
      ys'' <- alignToModel xs' ys'
      pure (xs',ys'')
    False -> do
      xs'' <- alignToModel ys' xs'
      pure (xs'',ys') 
  pure $ zipWith (+) xs'' ys''
  

extendWithZeros :: Int -> [GLSLExpr] -> [GLSLExpr]
extendWithZeros nChnls xs = xs'
  where
    xChnls = exprsChannels xs
    xs' = xs ++ Prelude.replicate (nChnls - xChnls) 0

    
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
alignToModel (m:ms) xs = do
  (x',xs') <- splitAligned (glslType m) xs
  xs'' <- alignToModel ms xs'
  return $ x' : xs''


-- texture access is always assigned to variable, since it is an expensive operation
-- note: position arguments are bipolar (an implicit/internal conversion to unipolar is baked in)
texture2D :: Builder -> [GLSLExpr] -> GLSL [GLSLExpr]
texture2D n xy = do
  xy' <- align Vec2 xy
  let f x = GLSLExpr Vec3 False $ "tex(" <> n <> "," <> builder x <> ")"
  mapM assign $ fmap f xy'
