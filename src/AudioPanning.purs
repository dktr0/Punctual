module AudioPanning where

import Prelude ((>),($),pure,otherwise,(*),(<>),(/),(>>=),bind,(-),(+),(<=),map,(==),(<$>))
import Data.Int (toNumber)
import Data.Number (pi,cos)
import Data.Ord (abs)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList, head, length, zipWith)
import Data.Unfoldable1 (iterateN,replicate1)
import Data.Traversable (traverse,sequence)
import Data.Tuple (Tuple(..))

import W (W,Sample,Frame,assign,product,sumChannels,zero)
import W as W
import Matrix (fromNonEmptyList,flatten)

splay :: Int -> Frame -> W (NonEmptyList Sample)
splay nOutputChnls xs
  | nOutputChnls <= 1 = pure <$> W.sum xs
  | length (flatten xs) == 1 = flatten <$> pan nOutputChnls (Left 0.5) (head $ flatten xs)
  | otherwise = do
      let xs' = flatten xs
      let nInputChnls = length xs'
      let stepSize = 1.0 / toNumber (nInputChnls - 1)
      let inputPositions = map Left $ iterateN nInputChnls (_ + stepSize) 0.0
      xss <- sequence $ zipWith (pan nOutputChnls) inputPositions xs' -- :: NonEmptyList Frame -- one Frame per input, each Frame has nOutputChnls Samples
      flatten <$> sumChannels xss

aout :: Int -> Int -> Frame -> W (NonEmptyList Sample)
aout nOutputChnls channelOffset xs = 
  case channelOffset <= 0 of
    true -> splay nOutputChnls xs
    false -> do
      let a = replicate1 channelOffset zero
      b <- splay nOutputChnls xs
      pure $ a <> b

pan :: Int -> Sample -> Sample -> W Frame
pan nOutputChnls pos i 
  | nOutputChnls <= 1 = pure $ pure i
  | otherwise = do
      pos' <- product pos (Left $ toNumber $ nOutputChnls - 1)
      let outputPositions = iterateN nOutputChnls (_ + 1.0) 0.0 
      outputDistances <- traverse (\op -> W.difference (Left op) pos' >>= W.abs >>= W.clip (Tuple (Left 0.0) (Left 1.0)) ) outputPositions
      outputGains <- traverse gainFromDistance outputDistances
      fromNonEmptyList <$> traverse (product i) outputGains

gainFromDistance :: Sample -> W Sample
gainFromDistance (Left x) 
  | abs x > 1.0 = pure $ Left 0.0
  | otherwise = pure $ Left $ cos (abs x * pi / 2.0)
gainFromDistance (Right x) = assign $ "Math.abs(" <> x <> ")>1?0:Math.cos(Math.abs(" <> x <> ")*Math.PI/2)"

