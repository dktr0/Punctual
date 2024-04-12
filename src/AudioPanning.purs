module AudioPanning where

import Prelude ((>),($),pure,otherwise,(*),(<>),(/),(>>=),bind,(-),(+),(<=),map,(==),(<$>))
import Data.Int (toNumber)
import Data.Number (pi,cos)
import Data.Ord (abs)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList,cons,singleton,length,zipWith,head,fromList,mapMaybe,tail)
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (iterateN)
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..))

import W (W,Sample,Frame,assign,product)
import W as W


splay :: Int -> Frame -> W Frame
splay nOutputChnls xs 
  | nOutputChnls <= 1 = singleton <$> W.sum xs
  | length xs == 1 = pan nOutputChnls (Left 0.5) (head xs)
  | otherwise = do
      let nInputChnls = length xs
      let stepSize = 1.0 / toNumber (nInputChnls - 1)
      let inputPositions = map Left $ iterateN nInputChnls (_ + stepSize) 0.0
      xss <- sequence $ zipWith (pan nOutputChnls) inputPositions xs -- :: NonEmptyList Frame -- one Frame per input, each Frame has nOutputChnls Samples
      transposedSums xss

transposedSums :: NonEmptyList Frame -> W Frame
transposedSums xs = do
  h <- W.sum (map head xs) -- :: W Sample
  let a = map tail xs -- :: NonEmptyList (List Sample)
  let b = mapMaybe fromList a -- List (NonEmptyList Sample)
  case fromList b of
    Nothing -> pure $ singleton h
    Just b' -> do
      t <- transposedSums b'
      pure $ h `cons` t
  
pan :: Int -> Sample -> Sample -> W Frame
pan nOutputChnls pos i 
  | nOutputChnls <= 1 = pure $ singleton i
  | otherwise = do
      pos' <- product pos (Left $ toNumber $ nOutputChnls - 1)
      let outputPositions = iterateN nOutputChnls (_ + 1.0) 0.0 
      outputDistances <- traverse (\op -> W.difference (Left op) pos' >>= W.abs >>= W.clip (Tuple (Left 0.0) (Left 1.0)) ) outputPositions
      outputGains <- traverse gainFromDistance outputDistances
      traverse (product i) outputGains

gainFromDistance :: Sample -> W Sample
gainFromDistance (Left x) 
  | abs x > 1.0 = pure $ Left 0.0
  | otherwise = pure $ Left $ cos (abs x * pi / 2.0)
gainFromDistance (Right x) = assign $ "Math.abs(" <> x <> ")>1?0:Math.cos(Math.abs(" <> x <> ")*Math.PI/2)"

