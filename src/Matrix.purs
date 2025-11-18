module Matrix where

import Prelude (class Applicative, class Apply, class Eq, class Functor, class Show, map, pure, show, ($), (*), (<>), (==), (<$>), (<<<), (&&), otherwise, (>>>),max,(-))
import Data.Semigroup (class Semigroup)
import Data.List.NonEmpty (NonEmptyList, cons, intercalate, length, head, fromList, catMaybes, tail, drop)
import Data.List.NonEmpty as L
import Control.Apply (lift2,lift3)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Traversable (class Traversable,traverse,sequenceDefault)
import Data.Unfoldable1 (class Unfoldable1, replicate1, unfoldr1)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.Unfoldable (replicate)

import NonEmptyList (multi,zipWithEqualLength,unconsTuple)
import MultiMode (MultiMode(..))
import Channels (class Channels)

newtype Matrix a = Matrix (NonEmptyList (NonEmptyList a)) -- outer dimension is rows, inner dimension is columns

singleton :: forall a. a -> Matrix a
singleton x = Matrix $ pure $ pure x

isSingleton :: forall a. Matrix a -> Boolean
isSingleton (Matrix xs) = length xs == 1 && length (head xs) == 1

matrixHead :: forall a. Matrix a -> a
matrixHead (Matrix xs) = head (head xs)

flatten :: forall a. Matrix a -> NonEmptyList a
flatten (Matrix xs) = L.concat xs

fromNonEmptyList :: forall a. NonEmptyList a -> Matrix a
fromNonEmptyList = Matrix <<< pure

-- to be used, for example, in implementation of SignalList
fromNonEmptyListMulti :: forall a. NonEmptyList (Matrix a) -> Matrix a
fromNonEmptyListMulti xs
  | length xs == 1 = head xs
  | otherwise = Matrix $ multi $ map flatten xs -- NonEmptyList (NonEmptyList a)

fromMatrixMatrix :: forall a. Matrix (Matrix a) -> Matrix a
fromMatrixMatrix x = Matrix $ flatten $ map flatten x

-- to be used, for example, in implementation of Seq
semiFlatten :: forall a. Matrix a -> NonEmptyList (NonEmptyList a)
semiFlatten (Matrix xs) = xs

instance Eq a => Eq (Matrix a) where
  eq (Matrix xs) (Matrix ys) = xs == ys

instance Show a => Show (Matrix a) where
  show x = pp x
  -- show (Matrix xs) = "Matrix (" <> show xs <> ")"

instance Functor Matrix where
  map f (Matrix xs) = Matrix $ map (map f) xs

-- note: provided function is expected to guarantee that size of resulting rows is a deterministic function of size argument rows
mapRows :: forall a b. (NonEmptyList a -> NonEmptyList b) -> Matrix a -> Matrix b
mapRows f (Matrix xs) = Matrix $ map f xs

-- a challenge for later: can this be implemented without flattening (at least under some circumstances)?
instance Apply Matrix where
  apply fs xs
    | isSingleton fs = map (matrixHead fs) xs -- if fs is singleton, structure of xs is retained
    | isSingleton xs = map (\f -> f (matrixHead xs)) fs -- if xs is singleton, structure of fs is retained
    | otherwise = Matrix $ map (\f -> map f (flatten xs)) (flatten fs) -- if neither are singletons, rows reflect flattened fs and columns reflect flattened xs

applyPairwise :: forall a b. Matrix (a -> b) -> Matrix a -> Matrix b
applyPairwise fs xs = fromNonEmptyList $ zipWithEqualLength ($) (flatten fs) (flatten xs)

instance Applicative Matrix where
  pure = Matrix <<< pure <<< pure

instance Semigroup (Matrix a) where
  append xs ys = Matrix $ pure $ flatten xs <> flatten ys

instance Foldable Matrix where
  foldl f b xs = foldl f b $ flatten xs
  foldr f b xs = foldr f b $ flatten xs
  foldMap = foldMapDefaultL

instance Traversable Matrix where
  traverse f (Matrix xs) = Matrix <$> traverse (traverse f) xs
  sequence = sequenceDefault

instance Unfoldable1 Matrix where
  unfoldr1 f b = case f b of
                   Tuple a Nothing -> pure a
                   Tuple a (Just r) -> pure a <> unfoldr1 f r

-- to be used, for example, in implementation of Zip
zip :: forall a. Matrix a -> Matrix a -> Matrix a
zip xs ys = Matrix $ zipWithEqualLength (\x y -> x `cons` L.singleton y) (flatten xs) (flatten ys)

-- note: the provided Multis should have the same dimensions, or else all hell breaks loose
-- ? when rows are only of 1 item do they need to be combined into a single row
concat :: forall a. NonEmptyList (Matrix a) -> Matrix a
concat xs = Matrix $ map flatten xs

-- to be used, for example, in implementation of Rep
rep :: forall a. Int -> Matrix a -> Matrix a
rep n x = fromNonEmptyList $ L.concat $ replicate1 n (flatten x)

combine :: forall a b c. (a -> b -> c) -> MultiMode -> Matrix a -> Matrix b -> Matrix c
combine f Combinatorial = lift2 f
combine f Pairwise = combinePairwise f

combine3 :: forall a b c d. (a -> b -> c -> d) -> MultiMode -> Matrix a -> Matrix b -> Matrix c -> Matrix d
combine3 f Combinatorial a b c = lift3 f a b c
combine3 f Pairwise a b c = combinePairwise ($) (combinePairwise f a b) c

combinePairwise :: forall a b c. (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
combinePairwise f xs ys = fromNonEmptyList $ zipWithEqualLength f (flatten xs) (flatten ys)

toTuples :: forall a. Matrix a -> Matrix (Tuple a a)
toTuples xs = unfoldr1 unconsTuple (flatten xs)

pp :: forall a. Show a => Matrix a -> String
pp (Matrix xss) = "[" <> intercalate "," (map (\xs -> "[" <> intercalate "," (map show xs) <> "]" ) xss) <> "]"

-- **** IMPORTANT TODO: refactor to use tranpose in NonEmptyList.purs and add add guard against 1xn matrices ****
-- turn rows into columns and vice versa (in corrupt case where columns not of same length, just returns the input unchanged)
transpose :: forall a. Matrix a -> Matrix a
transpose (Matrix xss) = 
  case (fromList $ catMaybes $ map (tail >>> fromList) xss) of
    Just yss -> Matrix yss
    Nothing -> Matrix xss

getFromRows :: forall a. a -> Int -> Int -> Matrix a -> Matrix a
getFromRows itemToExtendWith n m x = mapRows (getFromRow itemToExtendWith n m) x

getFromRow :: forall a. a -> Int -> Int -> NonEmptyList a -> NonEmptyList a
getFromRow itemToExtendWith n m xs = 
  case zs of
    Just zs' -> zs'
    Nothing -> L.singleton itemToExtendWith
  where
    n' = max 0 n
    m' = max 1 m
    ys = List.take m' $ drop n' xs -- :: List a
    additionalNeeded = max (List.length ys - m') 0
    additionalItems = replicate additionalNeeded itemToExtendWith
    zs = fromList $ ys <> additionalItems

getRows :: forall a. Matrix a -> NonEmptyList (NonEmptyList a)
getRows (Matrix xss) = xss

rows :: forall a. Matrix a -> Int
rows (Matrix xss) = length xss

columns :: forall a. Matrix a -> Int
columns (Matrix xss) = length (head xss)

instance Channels (Matrix a) where
  channels x = rows x * columns x

