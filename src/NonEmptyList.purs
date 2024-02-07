module NonEmptyList where

-- utility functions over PureScript's NonEmptyList

import Prelude (max,($),(+),(/),bind,pure,map,(>=))
import Data.List.NonEmpty (NonEmptyList,length,concat,zipWith,singleton,fromList,init,tail,head)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (replicate1,unfoldr1)
import Data.List as List
import Data.Maybe (Maybe(..))

import MultiMode (MultiMode(..))

extendToEqualLength :: forall a b. NonEmptyList a -> NonEmptyList b -> Tuple (NonEmptyList a) (NonEmptyList b)
extendToEqualLength xs ys = Tuple (extendByRepetition n xs) (extendByRepetition n ys)
  where n = max (length xs) (length ys)
    
extendByRepetition :: forall a. Int -> NonEmptyList a -> NonEmptyList a
extendByRepetition n xs = zipWith (\x _ -> x) xs' ys
  where
    m = (n / length xs) + 1
    xs' = concat $ replicate1 m xs 
    ys = replicate1 n 1 -- NonEmptyList Int of correct length

-- if there's only one item in the list, returns a tuple containing that twice
everyAdjacentPair :: forall a. NonEmptyList a -> NonEmptyList (Tuple a a)
everyAdjacentPair xs
  = case fromList (List.zip (init xs) (tail xs)) of
      Just xs' -> xs'
      Nothing -> singleton $ Tuple (head xs) (head xs)

-- if there's only one item in the list, returns a tuple containing that twice
everyPair :: forall a. NonEmptyList a -> NonEmptyList (Tuple a a)
everyPair xs = concat $ unfoldr1 everyPairUnfolder xs

{-
unfoldr1 :: forall t a b. Unfoldable1 t => (b -> Tuple a (Maybe b)) -> b -> t a
a :: NonEmptyList (Tuple c c)
b :: NonEmptyList c
-}

everyPairUnfolder :: forall a. NonEmptyList a -> Tuple (NonEmptyList (Tuple a a)) (Maybe (NonEmptyList a))
everyPairUnfolder xs = Tuple a mb
  where
    mys = fromList $ map (Tuple (head xs)) (tail xs) -- Maybe (NonEmptyList a)
    a = case mys of
          Nothing -> singleton $ Tuple (head xs) (head xs)
          Just ys -> ys
    mb = case (fromList $ tail xs) of
           Nothing -> Nothing
           Just t -> if length t >= 2 then Just t else Nothing 
    
        
combineCombinatorial :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combineCombinatorial f xs ys = do -- in NonEmptyList monad
  x <- xs
  y <- ys
  pure $ f x y

combinePairwise :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combinePairwise f xs ys = zipWith f xs' ys'
  where
    n = max (length xs) (length ys)
    xs' = extendByRepetition n xs
    ys' = extendByRepetition n ys
    
combine :: forall a b c. MultiMode -> (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combine Combinatorial = combineCombinatorial
combine Pairwise = combinePairwise

