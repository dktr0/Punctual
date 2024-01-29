module GLSL where

-- This module defines the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")
-- A fundamental goal is elegantly bridging the gap between GLSL's 4 basic float types
-- and the multi-channel expressions of Punctual (which are not limited to 1-4 channels).

import Prelude (pure,(<>),otherwise,bind,discard,show,(+),($),(==),(<<<),(>>=))
import Data.Map (Map,insert)
import Data.Maybe (Maybe(..))
import Data.List (List,(:))
import Data.List.NonEmpty (NonEmptyList,singleton,concat,cons,head,tail,fromList)
import Control.Monad.State (State,get,put)
import Data.Traversable (traverse)
import Data.Set as Set

import GLSLExpr

type GLSLState = {
  nextIndex :: Int,
  exprs :: Map Int GLSLExpr,
  fxys :: NonEmptyList GLSLExpr,
  imgMap :: Map String Int,
  vidMap :: Map String Int
  }

type GLSL = State GLSLState

assign :: GLSLExpr -> GLSL GLSLExpr
assign x
  | x.isSimple = pure x -- don't assign/reassign expressions that are marked simple
  | otherwise = do
      s <- get
      put { nextIndex: s.nextIndex+1, exprs: insert s.nextIndex x s.exprs, fxys: s.fxys, imgMap: s.imgMap, vidMap: s.vidMap }
      pure $ { string: "_" <> show s.nextIndex, glslType: x.glslType, isSimple: true, deps: Set.insert s.nextIndex x.deps }


_swizzle :: String -> GLSLType -> GLSLExpr -> GLSL GLSLExpr
_swizzle spec rType x = do
  x' <- assign x -- as per definition of assign, this won't reassign expressions marked simple, so x' is simple
  pure { string: x'.string <> "." <> spec, glslType: rType, isSimple: true, deps: x'.deps }

swizzleX :: GLSLExpr -> GLSL GLSLExpr
swizzleX = _swizzle "x" Float

swizzleY :: GLSLExpr -> GLSL GLSLExpr
swizzleY = _swizzle "y" Float

swizzleZ :: GLSLExpr -> GLSL GLSLExpr
swizzleZ = _swizzle "z" Float

swizzleW :: GLSLExpr -> GLSL GLSLExpr
swizzleW = _swizzle "w" Float

swizzleXY :: GLSLExpr -> GLSL GLSLExpr
swizzleXY = _swizzle "xy" Vec2

swizzleYZ :: GLSLExpr -> GLSL GLSLExpr
swizzleYZ = _swizzle "yz" Vec2

swizzleZW :: GLSLExpr -> GLSL GLSLExpr
swizzleZW = _swizzle "zw" Vec2

swizzleXYZ :: GLSLExpr -> GLSL GLSLExpr
swizzleXYZ = _swizzle "xyz" Vec3

swizzleYZW :: GLSLExpr -> GLSL GLSLExpr
swizzleYZW = _swizzle "yzw" Vec3

swizzleXYY :: GLSLExpr -> GLSL GLSLExpr
swizzleXYY = _swizzle "xyy" Vec3

swizzleXYYY :: GLSLExpr -> GLSL GLSLExpr
swizzleXYYY = _swizzle "xyyy" Vec4

swizzleXYZZ :: GLSLExpr -> GLSL GLSLExpr
swizzleXYZZ = _swizzle "xyzz" Vec4


-- expression must be Vec2
texture2D :: String -> GLSLExpr -> GLSL GLSLExpr
texture2D texName x = assign { string: "texture2D(" <> texName <> "," <> x.string <> "*0.5+0.5).x", glslType: Float, isSimple: false, deps: x.deps }

-- variant of texture2D specialized for one-dimensions textures, such as those used for fft and ifft
textureFFT :: String -> GLSLExpr -> GLSL GLSLExpr
textureFFT texName x = assign { string: "texture2D(" <> texName <> ",vec2(" <> x.string <> "*0.5+0.5,0.)).x", glslType: Float, isSimple: false, deps: x.deps }


alignFloat :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignFloat xs = traverse splitIntoFloats xs >>= (pure <<< concat)

splitIntoFloats :: GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
splitIntoFloats e
  | e.glslType == Float = pure $ singleton e
  | e.glslType == Vec2 = do
      e' <- assign e -- pre-assigning ahead of swizzles so that multiple swizzles don't multiply assign e
      x <- swizzleX e'
      y <- swizzleY e'
      pure $ x `cons` singleton y
  | e.glslType == Vec3 = do
      e' <- assign e
      x <- swizzleX e'
      y <- swizzleY e'
      z <- swizzleZ e'
      pure (x `cons` (y `cons` singleton z))
  | otherwise = do
      e' <- assign e
      x <- swizzleX e'
      y <- swizzleY e'
      z <- swizzleZ e'
      w <- swizzleW e'
      pure (x `cons` (y `cons` (z `cons` singleton w)))

unconsFloat :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsFloat xs
  | _.glslType (head xs) == Float = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Vec2 = do
      x <- assign $ head xs
      a <- swizzleX x
      b <- swizzleY x
      pure { head: a, tail: b : tail xs}
  | _.glslType (head xs) == Vec3 = do
      x <- assign $ head xs
      a <- swizzleX x
      b <- swizzleYZ x
      pure { head: a, tail: b : tail xs}
  | otherwise = do
      x <- assign $ head xs
      a <- swizzleX x
      b <- swizzleYZW x
      pure { head: a, tail: b : tail xs}


alignVec2 :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec2 = unfoldWith unconsVec2

alignVec2NoExtend :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec2NoExtend = unfoldWith unconsVec2NoExtend

alignVec3 :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec3 = unfoldWith unconsVec3

alignVec3NoExtend :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec3NoExtend = unfoldWith unconsVec3NoExtend

alignVec4NoExtend :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec4NoExtend = unfoldWith unconsVec4NoExtend

unfoldWith :: (NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }) -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
unfoldWith f xs = do
  x <- f xs
  case fromList x.tail of
    Nothing -> pure $ singleton $ x.head
    Just xs' -> do
      t'' <- unfoldWith f xs'
      pure $ x.head `cons` t''

unconsVec2 :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec2 xs
  | _.glslType (head xs) == Vec2 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Float = do
      case fromList (tail xs) of
        Nothing -> pure { head: vec2unary (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec2binary (head xs) y.head, tail: y.tail}
  | _.glslType (head xs) == Vec3 = do
      x <- assign $ head xs
      a <- swizzleXY x
      b <- swizzleZ x
      pure { head: a, tail: b : tail xs}
  | otherwise = do
      x <- assign $ head xs
      a <- swizzleXY x
      b <- swizzleZW x
      pure { head: a, tail: b : tail xs}

unconsVec2NoExtend :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec2NoExtend xs
  | _.glslType (head xs) == Vec2 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Float = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec2binary (head xs) y.head, tail: y.tail }
  | _.glslType (head xs) == Vec3 = do
      x <- assign $ head xs
      a <- swizzleXY x
      b <- swizzleZ x
      pure { head: a, tail: b : tail xs }
  | otherwise {- Vec4 -} = do
      x <- assign $ head xs
      a <- swizzleXY x
      b <- swizzleZW x
      pure { head: a, tail: b : tail xs }

unconsVec3 :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec3 xs
  | _.glslType (head xs) == Vec3 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Float = do
      case fromList (tail xs) of
        Nothing -> pure { head: vec3unary (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsVec2 xs'
          pure { head: vec3binary (head xs) y.head, tail: y.tail}
  | _.glslType (head xs) == Vec2 = do
      case fromList (tail xs) of
        Nothing -> pure { head: vec3unary (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec3binary (head xs) y.head, tail: y.tail}  
  | otherwise {- Vec4 -} = do
      x <- assign $ head xs
      a <- swizzleXYZ x
      b <- swizzleW x
      pure { head: a, tail: b : tail xs}      
      
unconsVec3NoExtend :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec3NoExtend xs
  | _.glslType (head xs) == Vec3 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Float = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsVec2NoExtend xs' -- might be Float, Vec2
          case _.glslType y.head of
            Float -> pure { head: vec2binary (head xs) y.head, tail: y.tail }
            _ {- Vec2 -} -> pure { head: vec3binary (head xs) y.head, tail: y.tail }
  | _.glslType (head xs) == Vec2 = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec3binary (head xs) y.head, tail: y.tail }
  | otherwise {- Vec4 -} = do
      x <- assign (head xs)
      a <- swizzleXYZ x
      b <- swizzleW x
      pure { head: a, tail: b : tail xs }        
      
unconsVec4NoExtend :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec4NoExtend xs
  | _.glslType (head xs) == Vec4 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Vec3 = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec4binary (head xs) y.head, tail: y.tail }
  | _.glslType (head xs) == Vec2 = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsVec2NoExtend xs' -- might be Float or Vec2...
          case _.glslType y.head of
            Float -> pure { head: vec3binary (head xs) y.head, tail: y.tail }
            _ {- Vec2 -} -> pure { head: vec4binary (head xs) y.head, tail: y.tail }
  | otherwise {- Float -} = do
      case fromList (tail xs) of
        Nothing -> pure { head: head xs, tail: tail xs }
        Just xs' -> do
          y <- unconsVec3NoExtend xs' -- might be Float, Vec2, or Vec3...
          case _.glslType y.head of
            Float -> pure { head: vec2binary (head xs) y.head, tail: y.tail }
            Vec2 -> pure { head: vec3binary (head xs) y.head, tail: y.tail }
            _ {- Vec3 -} -> pure { head: vec4binary (head xs) y.head, tail: y.tail }
            


-- | alignRGBA aligns to groups of 4 channels (vec4) as follows:
-- 1 channel: repeat as channel 2 and 3, set channel 4 (alpha) to 1
-- 2 channels: repeat channel 2 as 3, set channel 4 (alpha) to 1
-- 3 channels: set channel 4 (alpha) to 1
-- 4 channels: identity

alignRGBA :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignRGBA xs = alignVec4NoExtend xs >>= traverse exprRGBA

exprRGBA :: GLSLExpr -> GLSL GLSLExpr
exprRGBA x
  | x.glslType == Vec4 = pure x
  | x.glslType == Float = pure $ vec4binary (vec3unary x) one
  | x.glslType == Vec2 = do
      x' <- assign x
      y <- swizzleY x'
      pure $ vec4ternary x y one
  | otherwise {- Vec3 -} = pure $ vec4binary x one


{-
delete if alignRGBA works
  | exprsChannels xs == 1 = return [ exprExprToVec4 (exprToVec3 (Prelude.head xs)) $ constantFloat 1.0 ]
  | exprsChannels xs == 2 = do
      x' <- align Vec2 xs >>= (assign . Prelude.head)
      return [ exprExprToVec4 (exprExprToVec3 x' $ swizzleY x') $ constantFloat 1.0 ]
  | exprsChannels xs == 3 = return [ exprExprToVec4 (Prelude.head xs) $ constantFloat 1.0]
  | exprsChannels xs >= 4 = do
      (y,ys) <- splitAligned Vec4 xs
      ys' <- alignRGBA ys
      return (y:ys')
-}

{-
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
-}



{-

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


-- used by Punctual's 'zip' operation
zipGLSLExpr :: [GLSLExpr] -> [GLSLExpr] -> GLSL [GLSLExpr]
zipGLSLExpr xs ys = do
  xs' <- align GLFloat xs
  ys' <- align GLFloat ys
  pure $ Foldable.concat $ zipWith (\a b -> [a,b]) xs' ys' -- Haskell-style zip, excess elements in longer list discarded
  -}
