module NewSignal where

import W (W,Sample)
import G (G)
import Matrix (Matrix)
import Expr (Float)
import AST (Expression)

type Signal = { audio :: W (Matrix Sample), video :: G (Matrix Float), expression :: Expression }

-- ??? type Signal m a = { gen :: m (Matrix a), expression :: Expression }

type SignalSignal = { signalSignal :: Signal -> Signal, expression :: Expression }

{-

rmap :: SignalSignal -> Signal -> Signal
rmap f x = { audio, video }
  where
    xs = divideIntoRows x -- :: NonEmptyList Signal
	ys = map f.signalSignal xs -- :: NonEmptyList Signal
	audio = do
	  zs <- sequence $ map (_.audio) ys -- W (NonEmptyList ((Matrix Sample))
	  pure ... put the rows in zs together as a single matrix
	video = do
	  zs <- sequence $ map (_.video) ys -- G (NonEmptyList ((Matrix Float)))
	  pure ... put the rows in zs together as a single matrix

is this possible? divideIntoRows :: Signal -> NonEmptyList Signal
yes, I think so!
divideIntoRows x = 
  ... getRows of x.audio by mapping :: W (NonEmptyList (NonEmptyList Sample))
  ... getRows of x.video by mapping :: G (NonEmptyList (NonEmptyList Float))
  ...zip the two sets of rows pairwise, turning each pair into a Signal

  -- x :: { audio :: W (Matrix Sample), video :: G (Matrix Float) }

-}