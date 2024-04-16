module Multi where

import Prelude (class Functor,($),map,flip)
import Data.List.NonEmpty (NonEmptyList)

data Multi a = Atom a | Multi (NonEmptyList (Multi a))

instance Functor Multi where
  map f (Atom a) = Atom (f a)
  map f (Multi xs) = Multi $ map (map f) xs
 
-- combinatorial: structure comes from first argument ("from the left"), unless it is an atom (no structure)
atomAtomAtomC :: forall a b c. (a -> b -> c) -> Multi a -> Multi b -> Multi c
atomAtomAtomC f (Atom a) b = map (f a) b
atomAtomAtomC f a (Atom b) = map (flip f b) a
atomAtomAtomC f (Multi a) b = Multi $ map (\a' -> atomAtomAtomC f a' b) a
  
