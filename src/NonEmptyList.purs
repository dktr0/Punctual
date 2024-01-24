module NonEmptyList where

import Prelude (max,($),(+),(/))
import Data.List.NonEmpty (NonEmptyList,length,concat,zipWith)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (replicate1)

-- utility functions over PureScript's NonEmptyList

extendToEqualLength :: forall a b. NonEmptyList a -> NonEmptyList b -> Tuple (NonEmptyList a) (NonEmptyList b)
extendToEqualLength xs ys = Tuple (extendByRepetition n xs) (extendByRepetition n ys)
  where n = max (length xs) (length ys)
    
extendByRepetition :: forall a. Int -> NonEmptyList a -> NonEmptyList a
extendByRepetition n xs = zipWith (\x _ -> x) xs' ys
  where
    m = (n / length xs) + 1
    xs' = concat $ replicate1 m xs 
    ys = replicate1 n 1 -- NonEmptyList Int of correct length

