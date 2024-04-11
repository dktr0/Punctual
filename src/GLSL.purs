module GLSL where

-- This module defines the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")
-- A fundamental goal is elegantly bridging the gap between GLSL's 4 basic float types
-- and the multi-channel expressions of Punctual (which are not limited to 1-4 channels).

import Prelude (pure,(<>),otherwise,bind,discard,show,(+),($),(==),(<<<),(>>=),(<$>),(>=),(/),(-))
import Prelude as Prelude
import Data.Map (Map,insert,empty)
import Data.Maybe (Maybe(..))
import Data.List (List,(:))
import Data.List.NonEmpty (NonEmptyList,singleton,concat,cons,head,tail,fromList)
import Control.Monad.State (State,get,put,runState,modify_)
import Data.Traversable (traverse,for)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (replicate1)

import GLSLExpr

type GLSLState = {
  nextIndex :: Int,
  exprs :: Map Int GLSLExpr,
  imgMap :: Map String Int,
  vidMap :: Map String Int,
  webGl2 :: Boolean,
  fxy :: GLSLExpr,
  time :: GLSLExpr,
  beat :: GLSLExpr,
  etime :: GLSLExpr,
  ebeat :: GLSLExpr
  }

type GLSL = State GLSLState

runGLSL :: forall a. Boolean -> Map String Int -> Map String Int -> GLSL a -> Tuple a GLSLState
runGLSL webGl2 imgMap vidMap x = runState x {
  nextIndex: 0,
  exprs: empty,
  imgMap,
  vidMap,
  webGl2,
  fxy: defaultFxy, 
  time: defaultTime,
  beat: defaultBeat,
  etime: defaultETime,
  ebeat: defaultEBeat
  }
  
assign :: GLSLExpr -> GLSL GLSLExpr
assign x
  | x.isSimple = pure x -- don't assign/reassign expressions that are marked simple
  | otherwise = assignForced x

assignForced :: GLSLExpr -> GLSL GLSLExpr
assignForced x = do
  s <- get
  put $ s { nextIndex = s.nextIndex+1, exprs = insert s.nextIndex x s.exprs }
  pure $ { string: "_" <> show s.nextIndex, glslType: x.glslType, isSimple: true, deps: Set.insert s.nextIndex x.deps }


withFxys :: NonEmptyList GLSLExpr -> GLSL Exprs -> GLSL Exprs
withFxys fxys a = do
  cachedFxy <- _.fxy <$> get
  rs <- for fxys $ \fxy -> do
    modify_ $ \s -> s { fxy = fxy }
    a
  modify_ $ \s -> s { fxy = cachedFxy }
  pure $ concat rs

withAlteredTime :: NonEmptyList { time :: GLSLExpr, beat :: GLSLExpr, etime :: GLSLExpr, ebeat :: GLSLExpr }  -> GLSL Exprs -> GLSL Exprs
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs
  

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


-- expression must be Vec2, assumes unipolar coordinates (this facilitates upstream use of larger types, since conversions to unipolar can happen on larger types that are then force-assigned)
texture2D :: String -> GLSLExpr -> GLSL GLSLExpr
texture2D texName x = assign { string: "texture2D(" <> texName <> "," <> x.string <> ").xyz", glslType: Vec3, isSimple: false, deps: x.deps }

-- variant of texture2D specialized for one-dimensional, single-channel textures, such as those used for fft and ifft
-- assumes unipolar coordinates (this facilitates upstream use of larger types, since conversions to unipolar can happen on larger types that are then force-assigned)
textureFFT :: String -> GLSLExpr -> GLSL GLSLExpr
textureFFT texName x = assign { string: "texture2D(" <> texName <> ",vec2(" <> x.string <> ",0.)).x", glslType: Float, isSimple: false, deps: x.deps }


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

unconsGLSL :: GLSLType -> NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsGLSL Float = unconsFloat
unconsGLSL Vec2 = unconsVec2
unconsGLSL Vec3 = unconsVec3
unconsGLSL Vec4 = unconsVec4

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
      
align :: GLSLType -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
align Float = alignFloat
align Vec2 = alignVec2
align Vec3 = alignVec3
align Vec4 = alignVec4

alignNoExtend :: GLSLType -> NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignNoExtend Float = alignFloat
alignNoExtend Vec2 = alignVec2NoExtend
alignNoExtend Vec3 = alignVec3NoExtend
alignNoExtend Vec4 = alignVec4NoExtend

alignVec2 :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec2 = unfoldWith unconsVec2

alignVec2NoExtend :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec2NoExtend = unfoldWith unconsVec2NoExtend

alignVec3 :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec3 = unfoldWith unconsVec3

alignVec3NoExtend :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec3NoExtend = unfoldWith unconsVec3NoExtend

alignVec4 :: NonEmptyList GLSLExpr -> GLSL (NonEmptyList GLSLExpr)
alignVec4 = unfoldWith unconsVec4

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
        Nothing -> pure { head: coerceVec2 (head xs), tail: tail xs}
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
        Nothing -> pure { head: coerceVec3 (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsVec2 xs'
          pure { head: vec3binary (head xs) y.head, tail: y.tail}
  | _.glslType (head xs) == Vec2 = do
      case fromList (tail xs) of
        Nothing -> pure { head: coerceVec3 (head xs), tail: tail xs}
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

unconsVec4 :: NonEmptyList GLSLExpr -> GLSL { head :: GLSLExpr, tail :: List GLSLExpr }
unconsVec4 xs
  | _.glslType (head xs) == Vec4 = pure { head: head xs, tail: tail xs}
  | _.glslType (head xs) == Float = do
      case fromList (tail xs) of
        Nothing -> pure { head: coerceVec4 (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsVec3 xs'
          pure { head: vec4binary (head xs) y.head, tail: y.tail}
  | _.glslType (head xs) == Vec2 = do
      case fromList (tail xs) of
        Nothing -> pure { head: coerceVec4 (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsVec2 xs'
          pure { head: vec4binary (head xs) y.head, tail: y.tail}
  | otherwise {- Vec3 -} = do
      case fromList (tail xs) of
        Nothing -> pure { head: coerceVec4 (head xs), tail: tail xs}
        Just xs' -> do
          y <- unconsFloat xs'
          pure { head: vec4binary (head xs) y.head, tail: y.tail}


-- TODO: this needs to be extended to optimize cases such as 4 floats in a row which can be directly combined into a vec4 instead of being assembled into vec3 and vec2 first... ditto for unconsVec3NoExtend
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
  | x.glslType == Float = pure $ vec4binary (forceCast Vec3 x) one
  | x.glslType == Vec2 = do
      x' <- assign x
      y <- swizzleY x'
      pure $ vec4ternary x y one
  | otherwise {- Vec3 -} = pure $ vec4binary x one

-- given two Exprs, align them (without extending) using unconsAligned (larger types fractured to match smaller boundaries)
-- then zip them with a binary function a -> a -> a
zipWithAAA :: forall a. (GLSLExpr -> GLSLExpr -> a) -> Exprs -> Exprs -> GLSL (NonEmptyList a)
zipWithAAA f xs ys = do
  r <- unconsAligned xs ys
  let z =  f r.headX r.headY
  case fromList r.tailX of
    Nothing -> pure $ singleton z
    Just tx -> do
      case fromList r.tailY of
        Nothing -> pure $ singleton z
        Just ty -> do
          t <- zipWithAAA f tx ty
          pure $ z `cons` t

-- given three Exprs, align them (without extending) using unconsAligned3 (larger types fractured to match smaller boundaries)
-- then zip them with a function a -> a -> a -> a
zipWithAAAA :: forall a. (GLSLExpr -> GLSLExpr -> GLSLExpr -> a) -> Exprs -> Exprs -> Exprs -> GLSL (NonEmptyList a)
zipWithAAAA f xs ys zs = do
  r <- unconsAligned3 xs ys zs
  let a = f r.headX r.headY r.headZ
  case fromList r.tailX of
    Nothing -> pure $ singleton a
    Just tx -> do
      case fromList r.tailY of
        Nothing -> pure $ singleton a
        Just ty -> do
          case fromList r.tailZ of
            Nothing -> pure $ singleton a
            Just tz -> do
              t <- zipWithAAAA f tx ty tz
              pure $ a `cons` t

-- given the head of two Exprs, find which one is the smaller type, and use that type to uncons both inputs
unconsAligned :: Exprs -> Exprs -> GLSL { headX :: GLSLExpr, headY :: GLSLExpr, tailX :: List GLSLExpr, tailY :: List GLSLExpr }
unconsAligned xs ys = do
  let t = Prelude.min (head xs).glslType (head ys).glslType
  xs' <- unconsGLSL t xs
  ys' <- unconsGLSL t ys
  pure { headX: xs'.head, headY: ys'.head, tailX: xs'.tail, tailY: ys'.tail }

-- given the head of three Exprs, find which one is the smaller type, and use that type to uncons all three inputs
unconsAligned3 :: Exprs -> Exprs -> Exprs -> GLSL { headX :: GLSLExpr, headY :: GLSLExpr, headZ :: GLSLExpr, tailX :: List GLSLExpr, tailY :: List GLSLExpr, tailZ :: List GLSLExpr }
unconsAligned3 xs ys zs = do
  let t = Prelude.min (Prelude.min (head xs).glslType (head ys).glslType) (head zs).glslType
  xs' <- unconsGLSL t xs
  ys' <- unconsGLSL t ys
  zs' <- unconsGLSL t zs
  pure { headX: xs'.head, headY: ys'.head, headZ: zs'.head, tailX: xs'.tail, tailY: ys'.tail, tailZ: zs'.tail }
      
-- extend provided Exprs so that it has the specified number of channels, by repeating from the beginning
-- (or cut off channels if the provided Exprs has more channels than requested)
extend :: Int -> Exprs -> GLSL Exprs
extend n xs
  | n == exprsChannels xs = pure xs
  | otherwise = do
      let m = ((n-1) / exprsChannels xs) + 1
      takeChannels n $ concat $ replicate1 m $ xs

extendAligned :: Exprs -> Exprs -> GLSL (NonEmptyList (Tuple GLSLExpr GLSLExpr))
extendAligned xs ys = do
  let n = Prelude.max (exprsChannels xs) (exprsChannels ys)
  xs' <- extend n xs
  ys' <- extend n ys
  zipWithAAA Tuple xs' ys'

-- take n channels from the provided Exprs, or just all of the provided Exprs if they have less channels than
takeChannels :: Int -> Exprs -> GLSL Exprs
takeChannels n xs
  | exprChannels (head xs) >= n = do -- pass-through or truncate if head of list is equal or larger than requested channels
      case n of
        1 -> pure $ singleton $ coerceFloat $ head xs
        2 -> pure $ singleton $ coerceVec2 $ head xs
        3 -> pure $ singleton $ coerceVec3 $ head xs
        _ -> pure $ singleton $ coerceVec4 $ head xs
  | otherwise = do -- head of list is smaller than requested channels
      let x = head xs
      case fromList (tail xs) of
        Nothing -> pure $ singleton x -- ...and is last item, so just pass it through
        Just txs -> do
          txs' <- takeChannels (n - exprChannels x) txs
          pure $ x `cons` txs'


osc :: GLSLExpr -> GLSL GLSLExpr -- Any -> Any
osc f = do
  t <- _.time <$> get
  pure $ sin $ product (product (product pi (float 2.0)) t) f
  
phasor :: GLSLExpr -> GLSL GLSLExpr -- Any -> Any
phasor f = do
  t <- _.time <$> get
  pure $ fract $ product t f 

tri :: GLSLExpr -> GLSL GLSLExpr -- Any -> Any
tri f = do
  p <- phasor f
  pure $ { string: "(1.-(4.*abs(" <> p.string <> "-0.5)))", glslType: f.glslType, isSimple: f.isSimple, deps: f.deps }

saw :: GLSLExpr -> GLSL GLSLExpr -- Any -> Any
saw f = bipolar <$> phasor f

sqr :: GLSLExpr -> GLSL GLSLExpr -- Any -> Any
sqr f = bipolar <$> greaterThanEqual (float 0.5) <$> phasor f


