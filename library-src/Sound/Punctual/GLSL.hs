{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.GLSL where

-- This module provides types and functions to represent and manipulate the text of
-- GLSL fragment shaders, including the monad GLSL for representing computations that
-- accumulate GLSL variable definitions (such as "vec4 _0 = vec4(1.0,2.0,3.0,4.0);")

import Data.Text as T
import TextShow
import Data.Set as Set
import Data.IntMap as IntMap
import Control.Monad
import Control.Monad.State
import Data.Foldable as Foldable


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

exprChannels :: Expr -> Int
exprChannels (Vec4 _ _) = 4
exprChannels (Vec3 _ _) = 3
exprChannels (Vec2 _ _) = 2
exprChannels (GLFloat _ _) = 1

-- given an Expr, coerce it into an Expr containing a single GLFloat that is the sum
-- of its channels. when the provided value is not already a GLFloat we do this with
-- a dot product for efficiency (ie. to avoid repeating the provided value's Builder)
exprToGLFloat :: Expr -> Expr
exprToGLFloat (GLFloat b deps) = GLFloat b deps
exprToGLFloat (Vec2 b deps) = GLFloat ("dot(" <> b <> ",vec2(1.))") deps
exprToGLFloat (Vec3 b deps) = GLFloat ("dot(" <> b <> ",vec3(1.))") deps
exprToGLFloat (Vec4 b deps) = GLFloat ("dot(" <> b <> ",vec4(1.))") deps

-- given an Expr, coerce it into an Expr containing a GLFloat for each channel of the
-- provided value, regardless of the original underlying types. (Note that this probably
-- makes very inefficient shader code because of the repetition of the provided Builder.)
exprToGLFloats :: Expr -> [Expr]
exprToGLFloats (GLFloat b deps) = [GLFloat b deps]
exprToGLFloats (Vec2 b deps) = [GLFloat (b <> ".x") deps,GLFloat (b <> ".y") deps]
exprToGLFloats (Vec3 b deps) = [GLFloat (b <> ".x") deps,GLFloat (b <> ".y") deps,GLFloat (b <> ".z") deps]
exprToGLFloats (Vec4 b deps) = [GLFloat (b <> ".x") deps,GLFloat (b <> ".y") deps,GLFloat (b <> ".z") deps,GLFloat (b <> ".w") deps]

-- given an Expr, coerce it into an Expr containing a Vec3:
--  GLFloat is repeated
--  last value of Vec2 is repeated
--  Vec3 is identity function
--  Vec4 drops last component
exprToVec3 :: Expr -> Expr
exprToVec3 (GLFloat b deps) = Vec3 ("vec3(" <> b <> ")") deps
exprToVec3 (Vec2 b deps) = Vec3 (b <> ".xyy") deps
exprToVec3 (Vec3 b deps) = Vec3 b deps
exprToVec3 (Vec4 b deps) = Vec3 (b <> ".xyz") deps


-- Now that we have Expr, we can use it to define the type Exprs which allows us to
-- represent n-channel Punctual signals as lists of this looser type - our n-channel
-- signals will be composed of some number of 1-4 channel GLSL types:

newtype Exprs = Exprs [Expr] deriving (Show)

exprsChannels :: Exprs -> Int
exprsChannels (Exprs xs) = sum $ fmap exprChannels xs

constantFloat :: Double -> Exprs
constantFloat x = Exprs [GLFloat (showb x) Set.empty]

-- whatever the input types are, sum all of their channels to a single GLFloat (as an Exprs)
-- if the input Exprs is empty, so is the result
-- this might be used, for example, in the implementation of Punctual's 'mono' function
toGLFloat :: Exprs -> Exprs
toGLFloat (Exprs []) = Exprs []
toGLFloat (Exprs xs) = Exprs [Foldable.foldr1 f $ fmap exprToGLFloat xs]
  where f (GLFloat a aDeps) (GLFloat b bDeps) = GLFloat ("(" <> a <> "+" <> b <> ")") $ Set.union aDeps bDeps

-- whatever the input types are, divide their channels into individual GLFloats
-- if the input Exprs is empty, so is the result
-- note: this probably results in inefficient shader code, so we should try to avoid using this...
toGLFloats :: Exprs -> Exprs
toGLFloats (Exprs []) = Exprs []
toGLFloats (Exprs xs) = Exprs $ Prelude.concat $ fmap exprToGLFloats xs

-- *** TODO ***
-- whatever the input types are, produce a single Vec3 by summing channels in groups of 3
-- "missing channels" to complete a group of 3 are treated as 0
-- toVec3 :: Exprs -> Exprs

-- *** TODO ***
-- whatever the input types are, produce Vec3s by realigning into groups of 3
-- "missing channels" to complete a group of 3 are repetitions of the last actual value
-- toVec3s :: Exprs -> Exprs

-- *** TODO ***
-- whatever the input types are, re-align the data using Vec3s as much as possible
-- for example, 9 channels would become 3 Vec3s
-- 10 channels would become 3 Vec3s followed by a single GLFloat
-- alignVec3s :: Exprs -> Exprs

data Alignment = AlignVec2 | AlignVec3 | AlignVec4 | AlignLeft | AlignRight | AlignMostCompact

-- *** TODO ***
alignExprs :: Alignment -> Exprs -> Exprs -> (Exprs,Exprs)
alignExprs AlignVec2 x y = ...
alignExprs AlignVec3 x y = ...
alignExprs AlignVec4 x y = ...
alignExprs AlignLeft x y = ...
alignExprs AlignRight x y = ...
alignExprs AlignMostCompact x y = ...


addExprs :: Alignment -> Exprs -> Exprs -> Exprs
addExprs a x y =
  let (Exprs xs,Exprs ys) = alignExprs a x y
  zipWith (  ) xs ys

-- subtractExprs :: Alignment -> Exprs -> Exprs -> Exprs

-- multiplyExprs :: Alignment -> Exprs -> Exprs -> Exprs



-- As we write a fragment shader, basically we will be accumulating Expr(s) that are
-- assigned to variables in the underlying GLSL types. So we make a monad to represent
-- this sequential accumulation. The key operation in this monad is 'assign' which
-- assigns the contents of an Exprs to automagically-named GLSL variables, and returns
-- the (n.b. different) Exprs that would be used to access those variables in subsequent operations.

type GLSL = State (IntMap Expr)

runGLSL :: GLSL a -> (a,IntMap Expr)
runGLSL x = runState x IntMap.empty

assign :: Exprs -> GLSL Exprs
assign (Exprs xs) = mapM assignExpr xs >>= (return . Exprs)

assignExpr :: Expr -> GLSL Expr
assignExpr x = do
  m <- get
  let n = IntMap.size m -- *note* this assumes items are not removed from the map prior to any assignment
  put $ IntMap.insert n x m
  return $ case x of
    (Vec4 _ deps) -> Vec4 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (Vec3 _ deps) -> Vec3 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (Vec2 _ deps) -> Vec2 ("_" <> showb n) $ Set.union deps $ Set.singleton n
    (GLFloat _ deps) -> GLFloat ("_" <> showb n) $ Set.union deps $ Set.singleton n
