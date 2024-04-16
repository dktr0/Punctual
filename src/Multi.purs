module Multi where

import Prelude (class Functor,($),map,flip,class Eq,(==),class Show,(<>),show,(<<<),class Apply,apply,class Applicative)
import Data.List.NonEmpty (NonEmptyList,concat,singleton,cons,intercalate)
import Control.Apply (lift2)

import NonEmptyList (multi)
import MultiMode (MultiMode(..))

data Multi a = 
  Multi1 (NonEmptyList a) | -- 1-dimensional, represents variants or channels
  Multi2 (NonEmptyList (NonEmptyList a)) -- micro-dimension represents 'recent' variation
  
instance Eq a => Eq (Multi a) where
  eq (Multi1 xs) (Multi1 ys) = xs == ys
  eq (Multi2 xs) (Multi2 ys) = xs == ys
  eq _ _ = false

instance Show a => Show (Multi a) where
  show (Multi1 xs) = "Multi1 (" <> show xs <> ")"
  show (Multi2 xs) = "Multi2 (" <> show xs <> ")"

instance Functor Multi where
  map f (Multi1 xs) = Multi1 $ map f xs
  map f (Multi2 xs) = Multi2 $ map (map f) xs

instance Apply Multi where
  apply (Multi1 fs) (Multi1 xs) = Multi2 $ map (\f -> map f xs) fs
  apply fs xs = apply (Multi1 $ flatten fs) (Multi1 $ flatten xs) 
  
flatten :: forall a. Multi a -> NonEmptyList a
flatten (Multi1 xs) = xs
flatten (Multi2 xs) = concat $ multi xs

instance Applicative Multi where
  pure x = Multi1 $ singleton x

-- to be used, for example, in implementation of seq
semiFlatten :: forall a. Multi a -> NonEmptyList (NonEmptyList a)
semiFlatten (Multi1 xs) = singleton xs
semiFlatten (Multi2 xs) = xs

atomAtomAtom :: forall a b c. (a -> b -> c) -> Multi a -> Multi b -> Multi c
atomAtomAtom = lift2

test1 :: Multi Int
test1 = Multi1 (1 `cons` (2 `cons` singleton 3))

test2 :: Multi Int
test2 = Multi1 (5 `cons` singleton 7)

pp :: forall a. Show a => Multi a -> String
pp (Multi1 xs) = "[" <> intercalate "," (map show xs) <> "]"
pp (Multi2 xs) = "[" <> intercalate "," (map (pp <<< Multi1) xs) <> "]"
