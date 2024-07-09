module Multi where

import Prelude (class Applicative, class Apply, class Eq, class Functor, class Show, map, pure, show, ($), (<>), (==), (<$>), (<<<))
import Data.Semigroup (class Semigroup, append)
import Data.List.NonEmpty (NonEmptyList, cons, intercalate, singleton)
import Data.List.NonEmpty as L
import Control.Apply (lift2,lift3)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Traversable (class Traversable,traverse,sequenceDefault)
import Data.Unfoldable1 (class Unfoldable1, replicate1, unfoldr1)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import NonEmptyList (multi,zipWithEqualLength,unconsTuple)
import MultiMode (MultiMode(..))

newtype Multi a = Multi (NonEmptyList (NonEmptyList a)) -- outer dimension is rows, inner dimension is columns

flatten :: forall a. Multi a -> NonEmptyList a
flatten (Multi xs) = L.concat xs

fromNonEmptyList :: forall a. NonEmptyList a -> Multi a
fromNonEmptyList = Multi <<< pure

-- to be used, for example, in implementation of SignalList
fromNonEmptyListMulti :: forall a. NonEmptyList (Multi a) -> Multi a
fromNonEmptyListMulti xs = Multi $ multi $ map flatten xs -- NonEmptyList (NonEmptyList a)

-- to be used, for example, in implementation of Seq
semiFlatten :: forall a. Multi a -> NonEmptyList (NonEmptyList a)
semiFlatten (Multi xs) = xs

instance Eq a => Eq (Multi a) where
  eq (Multi xs) (Multi ys) = xs == ys

instance Show a => Show (Multi a) where
  show (Multi xs) = "Multi (" <> show xs <> ")"

instance Functor Multi where
  map f (Multi xs) = Multi $ map (map f) xs

-- note: provided function is expected to guarantee that size of resulting rows is a deterministic function of size argument rows
mapRows :: forall a b. (NonEmptyList a -> NonEmptyList b) -> Multi a -> Multi b
mapRows f (Multi xs) = Multi $ map f xs

instance Apply Multi where
  apply fs xs = Multi $ map (\x -> map (\f -> f x) (flatten fs)) (flatten xs)
  -- i.e. xs are "latest" combinatorial addition, so they are the rows of the result
  
applyPairwise :: forall a b. Multi (a -> b) -> Multi a -> Multi b
applyPairwise fs xs = fromNonEmptyList $ zipWithEqualLength ($) (flatten fs) (flatten xs)

instance Applicative Multi where
  pure = Multi <<< pure <<< pure 

instance Semigroup (Multi a) where
  append xs ys = Multi $ pure $ flatten xs <> flatten ys
  
instance Foldable Multi where
  foldl f b xs = foldl f b $ flatten xs 
  foldr f b xs = foldr f b $ flatten xs
  foldMap = foldMapDefaultL 

instance Traversable Multi where
  traverse f (Multi xs) = Multi <$> traverse (traverse f) xs
  sequence = sequenceDefault

instance Unfoldable1 Multi where
  unfoldr1 f b = case f b of
                   Tuple a Nothing -> pure a
                   Tuple a (Just r) -> pure a <> unfoldr1 f r
  
-- to be used, for example, in implementation of Zip
zip :: forall a. Multi a -> Multi a -> Multi a
zip xs ys = Multi $ zipWithEqualLength (\x y -> x `cons` singleton y) (flatten xs) (flatten ys)

-- note: the provided Multis should have the same dimensions, or else all hell breaks loose
concat :: forall a. NonEmptyList (Multi a) -> Multi a
concat xs = Multi $ map flatten xs

-- to be used, for example, in implementation of Rep
rep :: forall a. Int -> Multi a -> Multi a
rep n x = Multi $ replicate1 n (flatten x)

combine :: forall a b c. (a -> b -> c) -> MultiMode -> Multi a -> Multi b -> Multi c
combine f Combinatorial = lift2 f
combine f Pairwise = combinePairwise f

combine3 :: forall a b c d. (a -> b -> c -> d) -> MultiMode -> Multi a -> Multi b -> Multi c -> Multi d
combine3 f Combinatorial a b c = lift3 f a b c
combine3 f Pairwise a b c = combinePairwise ($) (combinePairwise f a b) c

combinePairwise :: forall a b c. (a -> b -> c) -> Multi a -> Multi b -> Multi c
combinePairwise f xs ys = fromNonEmptyList $ zipWithEqualLength f (flatten xs) (flatten ys)

toTuples :: forall a. Multi a -> Multi (Tuple a a)
toTuples xs = unfoldr1 unconsTuple (flatten xs)

pp :: forall a. Show a => Multi a -> String
pp (Multi xss) = "[" <> intercalate "," (map (\xs -> "[" <> intercalate "," (map show xs) <> "]" ) xss) <> "]"
