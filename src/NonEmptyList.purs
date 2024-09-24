module NonEmptyList where

-- utility functions over PureScript's NonEmptyList

import Prelude (max,($),(+),(/),bind,pure,map,(>=),(==),otherwise,(<<<))
import Data.List.NonEmpty (NonEmptyList,length,concat,zipWith,singleton,fromList,init,tail,head,cons,drop,catMaybes,cons',toList)
import Data.Tuple (Tuple(..))
import Data.Foldable (indexl)
import Data.Semigroup.Foldable (maximum)
import Data.Unfoldable1 (replicate1,unfoldr1)
import Data.List as List
import Data.Maybe (Maybe(..))

import MultiMode (MultiMode(..))

-- a variation on the standard zipWith - extends both list arguments to equal length before zipWith-ing them
zipWithEqualLength :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
zipWithEqualLength f xs ys = zipWith f xs' ys'
  where
    Tuple xs' ys' = extendToEqualLength xs ys

extendToEqualLength :: forall a b. NonEmptyList a -> NonEmptyList b -> Tuple (NonEmptyList a) (NonEmptyList b)
extendToEqualLength xs ys = Tuple (extendByRepetition n xs) (extendByRepetition n ys)
  where n = max (length xs) (length ys)

extendNonEmptyListsByRepetition :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList (NonEmptyList a)
extendNonEmptyListsByRepetition xss = map (extendByRepetition n) xss
  where n = maximum $ map length xss 
    
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
    
combine :: forall a b c. MultiMode -> (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combine Combinatorial = combineCombinatorial
combine Pairwise = combinePairwise
        
combineCombinatorial :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combineCombinatorial f xs ys = do
  x <- xs
  y <- ys
  pure $ f x y

combinePairwise :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
combinePairwise f xs ys 
  | length xs == 1 = map (f (head xs)) ys
  | length ys == 1 = map (\x -> f x (head ys)) xs
  | otherwise = do -- extend xs and ys to equal length in channels
      let Tuple xs' ys' = extendToEqualLength xs ys
      zipWith f xs' ys'

combine3 :: forall a b c d. MultiMode -> (a -> b -> c -> d) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c -> NonEmptyList d
combine3 Combinatorial = combine3Combinatorial
combine3 Pairwise = combine3Pairwise

combine3Combinatorial :: forall a b c d. (a -> b -> c -> d) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c -> NonEmptyList d
combine3Combinatorial f xs ys zs = do -- in NonEmptyList monad
  x <- xs
  y <- ys
  z <- zs
  pure $ f x y z
  
combine3Pairwise :: forall a b c d. (a -> b -> c -> d) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c -> NonEmptyList d
combine3Pairwise f xs ys zs = zipWith ($) (zipWith f xs' ys') zs'
  where
    n = max (max (length xs) (length ys)) (length zs)
    xs' = extendByRepetition n xs
    ys' = extendByRepetition n ys
    zs' = extendByRepetition n zs

-- TODO: rename this 'combinatorial' to complement 'pairwise' below
multi :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList (NonEmptyList a)
multi xs = do 
  case fromList (tail xs) of 
    Nothing -> map singleton $ head xs
    Just t -> do -- in NonEmptyList monad
      x <- head xs
      y <- multi t 
      pure $ x `cons` y 

unconsTuple :: forall a. NonEmptyList a -> Tuple (Tuple a a) (Maybe (NonEmptyList a))
unconsTuple xs =
  let a = head xs
      b = case indexl 1 xs of
            Just x -> x
            Nothing -> a
      h = Tuple a b
  in case fromList (drop 2 xs) of
       Nothing -> Tuple h Nothing
       Just t -> Tuple h (Just t)


pairwise :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList (NonEmptyList a)
pairwise = transpose <<< extendNonEmptyListsByRepetition

transpose :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList (NonEmptyList a)
transpose xss = cons' (map head xss) (List.catMaybes $ map fromList transposedList)
  where transposedList = List.transpose $ toList $ map tail xss -- :: List (List a)
