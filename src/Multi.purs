module Multi where

import Prelude (class Applicative, class Apply, class Eq, class Functor, class Show, map, pure, show, ($), (<>), (==))
import Data.Semigroup (class Semigroup, append)
import Data.List.NonEmpty (NonEmptyList,concat,singleton,cons,intercalate)
import Control.Apply (lift2)
import Data.Unfoldable1 (class Unfoldable1, replicate1)
import Data.Tuple (Tuple(..))

import NonEmptyList (multi,zipWithEqualLength)
import MultiMode (MultiMode(..))

data Multi a = Multi (NonEmptyList (NonEmptyList a)) -- micro-dimension represents 'most recent' variation
  
flatten :: forall a. Multi a -> NonEmptyList a
flatten (Multi xs) = concat $ multi xs

instance Eq a => Eq (Multi a) where
  eq (Multi xs) (Multi ys) = xs == ys

instance Show a => Show (Multi a) where
  show (Multi xs) = "Multi (" <> show xs <> ")"

instance Functor Multi where
  map f (Multi xs) = Multi $ map (map f) xs

instance Apply Multi where
  apply fs xs = Multi $ map (\f -> map f (flatten xs)) (flatten fs)
    
instance Applicative Multi where
  pure x = Multi $ singleton $ singleton x

instance Semigroup (Multi a) where
  append (Multi xs) (Multi ys) = Multi $ append xs ys
  
instance Unfoldable1 Multi where
  unfoldr1 f b = pure a
    where Tuple a _ = f b

-- to be used, for example, in implementation of SignalList
simplify :: forall a. NonEmptyList (Multi a) -> Multi a
simplify xs = Multi $ map flatten xs

-- to be used, for example, in implementation of Zip
zip :: forall a. Multi a -> Multi a -> Multi a
zip xs ys = Multi $ zipWithEqualLength (\x y -> x `cons` singleton y) (flatten xs) (flatten ys)
    
-- to be used, for example, in implementation of Rep
rep :: forall a. Int -> Multi a -> Multi a
rep n (Multi xs) = Multi $ concat $ replicate1 n xs
    
-- to be used, for example, in implementation of Seq
semiFlatten :: forall a. Multi a -> NonEmptyList (NonEmptyList a)
semiFlatten (Multi xs) = xs

combine :: forall a b c. (a -> b -> c) -> MultiMode -> Multi a -> Multi b -> Multi c
combine f Combinatorial = lift2 f
combine f Pairwise = combinePairwise f

combinePairwise :: forall a b c. (a -> b -> c) -> Multi a -> Multi b -> Multi c
combinePairwise f (Multi xss) (Multi yss) = Multi $ zipWithEqualLength (\xs ys -> zipWithEqualLength f xs ys) xss yss

pp :: forall a. Show a => Multi a -> String
pp (Multi xss) = "[" <> intercalate "," (map (\xs -> "[" <> intercalate "," (map show xs) <> "]" ) xss) <> "]"

