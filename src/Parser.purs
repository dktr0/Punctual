module Parser where

import Prelude (($),pure,bind,(<>),(>>=),(<<<))
import Data.Int (toNumber)
import Data.Tuple (Tuple)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Traversable (traverse)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Trans (StateT,runStateT,get)
import Control.Monad.Error.Class (throwError)
import Parsing (ParseError(..),Position)
import Data.Map as Map

import AST (Expression(..))
import Program (ProgramInfo)
import Signal (MultiMode(..), Signal(..))
import Value (Value(..),valuePosition)

type PState = {
  info :: ProgramInfo,
  defs :: Map.Map String Value
  }

type P a = StateT PState (Either ParseError) a

runP :: forall a. ProgramInfo -> Map.Map String Value -> P a -> Either ParseError (Tuple a PState)
runP i d p = runStateT p { info: i, defs: d }  


parseExpression :: Expression -> P Value
parseExpression (Reserved p x) = parseReserved p x
parseExpression (Identifier p x) = do
  s <- get
  case Map.lookup x s.defs of
    Just v -> pure v
    Nothing -> throwError $ ParseError ("unrecognized identifier " <> x) p
parseExpression (LiteralInt p x) = pure $ ValueInt p x
parseExpression (LiteralNumber p x) = pure $ ValueNumber p x
parseExpression (LiteralString p x) = pure $ ValueString p x
parseExpression (ListExpression p xs) = traverse parseExpression xs >>= listValueToValueSignal p -- succeeds if all items can be signals
parseExpression (Application p f x) = do
  f' <- parseExpression f
  case f' of
    ValueFunction _ ff -> do
      x' <- parseExpression x      
      lift $ ff x'
    _ -> throwError $ ParseError "only a function can be on the left-hand side of an application" p
parseExpression (Operation p _ _ _) = throwError $ ParseError "Operation not supported yet" p
{-  x' <- parseExpression x
  y' <- parseExpression y
  parseOperation p op x' y' -}
parseExpression (FromTo p _ _) = throwError $ ParseError "FromTo not supported yet" p
parseExpression (FromThenTo p _ _ _) = throwError $ ParseError "FromThenTo not supported yet" p

parseReserved :: Position -> String -> P Value
parseReserved p "append" = lift $ signalSignalSignal p Append
parseReserved p "zip" = lift $ signalSignalSignal p Zip
parseReserved p "pi" = pure $ ValueSignal p Pi
parseReserved p "px" = pure $ ValueSignal p Px
parseReserved p "py" = pure $ ValueSignal p Py
parseReserved p "aspect" = pure $ ValueSignal p Aspect
parseReserved p "fx" = pure $ ValueSignal p Fx
parseReserved p "fy" = pure $ ValueSignal p Fy
parseReserved p "fxy" = pure $ ValueSignal p Fxy
parseReserved p "frt" = pure $ ValueSignal p FRt
parseReserved p "fr" = pure $ ValueSignal p FR
parseReserved p "ft" = pure $ ValueSignal p FT
parseReserved p "setfx" = lift $ signalSignalSignal p SetFx
parseReserved p "setfy" = lift $ signalSignalSignal p SetFy
parseReserved p "setfxy" = lift $ signalSignalSignal p SetFxy
parseReserved p "zoom" = lift $ signalSignalSignal p Zoom
parseReserved p "move" = lift $ signalSignalSignal p Move
parseReserved p "tile" = lift $ signalSignalSignal p Tile
parseReserved p "spin" = lift $ signalSignalSignal p Spin
parseReserved p "lo" = pure $ ValueSignal p Lo
parseReserved p "mid" = pure $ ValueSignal p Mid
parseReserved p "hi" = pure $ ValueSignal p Hi
parseReserved p "ilo" = pure $ ValueSignal p ILo
parseReserved p "imid" = pure $ ValueSignal p IMid
parseReserved p "ihi" = pure $ ValueSignal p IHi
parseReserved p "cps" = pure $ ValueSignal p Cps
parseReserved p "time" = pure $ ValueSignal p Time
parseReserved p "beat" = pure $ ValueSignal p Beat
parseReserved p "ebeat" = pure $ ValueSignal p EBeat
parseReserved p "etime" = pure $ ValueSignal p ETime
parseReserved p "rnd" = pure $ ValueSignal p Rnd
parseReserved p "audioin" = pure $ ValueSignal p AudioIn
parseReserved p "fft" = lift $ signalSignal p FFT
parseReserved p "ifft" = lift $ signalSignal p IFFT
parseReserved p "mono" = lift $ signalSignal p Mono
parseReserved p "rep" = lift $ intSignalSignal p Rep
parseReserved p "unrep" = lift $ intSignalSignal p UnRep
parseReserved p "bipolar" = lift $ signalSignal p Bipolar
parseReserved p "unipolar" = lift $ signalSignal p Unipolar
parseReserved p "fb" = lift $ signalSignal p Fb
parseReserved p "img" = lift $ stringSignal p Img
parseReserved p "vid" = lift $ stringSignal p Vid
parseReserved p "cam" = pure $ ValueSignal p Cam
parseReserved p "blend" = lift $ signalSignal p Blend
parseReserved p "rgbhsv" = lift $ signalSignal p RgbHsv
parseReserved p "hsvrgb" = lift $ signalSignal p HsvRgb
parseReserved p "hsvh" = lift $ signalSignal p HsvH
parseReserved p "hsvs" = lift $ signalSignal p HsvS
parseReserved p "hsvv" = lift $ signalSignal p HsvV
parseReserved p "hsvr" = lift $ signalSignal p HsvR
parseReserved p "hsvg" = lift $ signalSignal p HsvG
parseReserved p "hsvb" = lift $ signalSignal p HsvB
parseReserved p "rgbh" = lift $ signalSignal p RgbH
parseReserved p "rgbs" = lift $ signalSignal p RgbS
parseReserved p "rgbv" = lift $ signalSignal p RgbV
parseReserved p "rgbr" = lift $ signalSignal p RgbR
parseReserved p "rgbg" = lift $ signalSignal p RgbG
parseReserved p "rgbb" = lift $ signalSignal p RgbB
parseReserved p "osc" = lift $ signalSignal p Osc
parseReserved p "tri" = lift $ signalSignal p Tri
parseReserved p "saw" = lift $ signalSignal p Saw
parseReserved p "sqr" = lift $ signalSignal p Sqr 
parseReserved p "lftri" = lift $ signalSignal p LFTri
parseReserved p "lfsaw" = lift $ signalSignal p LFSaw
parseReserved p "lfsqr" = lift $ signalSignal p LFSqr
parseReserved p "abs" = lift $ signalSignal p Abs
parseReserved p "acos" = lift $ signalSignal p Acos
parseReserved p "acosh" = lift $ signalSignal p Acosh
parseReserved p "asin" = lift $ signalSignal p Asin
parseReserved p "asinh" = lift $ signalSignal p Asinh
parseReserved p "atan" = lift $ signalSignal p Atan
parseReserved p "atanh" = lift $ signalSignal p Atanh
parseReserved p "cbrt" = lift $ signalSignal p Cbrt
parseReserved p "ceil" = lift $ signalSignal p Ceil
parseReserved p "cos" = lift $ signalSignal p Cos
parseReserved p "cosh" = lift $ signalSignal p Cosh
parseReserved p "exp" = lift $ signalSignal p Exp
parseReserved p "floor" = lift $ signalSignal p Floor
parseReserved p "log" = lift $ signalSignal p Log
parseReserved p "log2" = lift $ signalSignal p Log2
parseReserved p "log10" = lift $ signalSignal p Log10
parseReserved p "round" = lift $ signalSignal p Round
parseReserved p "sign" = lift $ signalSignal p Sign
parseReserved p "sin" = lift $ signalSignal p Sin
parseReserved p "sinh" = lift $ signalSignal p Sinh
parseReserved p "sqrt" = lift $ signalSignal p Sqrt
parseReserved p "tan" = lift $ signalSignal p Tan
parseReserved p "tanh" = lift $ signalSignal p Tanh
parseReserved p "trunc" = lift $ signalSignal p Trunc
parseReserved p "rtxy" = lift $ signalSignal p RtXy
parseReserved p "rtx" = lift $ signalSignal p RtX
parseReserved p "rty" = lift $ signalSignal p RtY
parseReserved p "xyrt" = lift $ signalSignal p XyRt
parseReserved p "xyr" = lift $ signalSignal p XyR
parseReserved p "xyt" = lift $ signalSignal p XyT
parseReserved p "point" = lift $ signalSignal p Point
parseReserved p "distance" = lift $ signalSignal p Distance
parseReserved p "prox" = lift $ signalSignal p Prox
parseReserved p "midicps" = lift $ signalSignal p MidiCps
parseReserved p "cpsmidi" = lift $ signalSignal p CpsMidi
parseReserved p "dbamp" = lift $ signalSignal p DbAmp
parseReserved p "ampdb" = lift $ signalSignal p AmpDb
parseReserved p "fract" = lift $ signalSignal p Fract
parseReserved p "max" = lift $ signalSignalSignal p $ Max Combinatorial
parseReserved p "maxp" = lift $ signalSignalSignal p $ Max Pairwise
parseReserved p "min" = lift $ signalSignalSignal p $ Min Combinatorial
parseReserved p "minp" = lift $ signalSignalSignal p $ Min Pairwise
parseReserved p "gate" = lift $ signalSignalSignal p $ Gate Combinatorial
parseReserved p "gatep" = lift $ signalSignalSignal p $ Gate Pairwise
parseReserved p "circle" = lift $ signalSignalSignal p Circle
parseReserved p "rect" = lift $ signalSignalSignal p Rect
parseReserved p "clip" = lift $ signalSignalSignal p Clip
parseReserved p "between" = lift $ signalSignalSignal p Between
parseReserved p "vline" = lift $ signalSignalSignal p VLine
parseReserved p "hline" = lift $ signalSignalSignal p HLine
parseReserved p "step" = lift $ signalSignalSignal p Step
parseReserved p "iline" = lift $ signalSignalSignalSignal p ILine
parseReserved p "line" = lift $ signalSignalSignalSignal p Line
parseReserved p "linlin" = lift $ signalSignalSignalSignal p LinLin
parseReserved p "lpf" = lift $ signalSignalSignalSignal p LPF
parseReserved p "hpf" = lift $ signalSignalSignalSignal p HPF
parseReserved p "bpf" = lift $ signalSignalSignalSignal p BPF
parseReserved p "delay" = lift $ numberSignalSignalSignal p Delay
parseReserved p x = throwError $ ParseError ("internal error in Punctual: parseReserved called with unknown reserved word " <> x) p

signalSignal :: Position -> (Signal -> Signal) -> Either ParseError Value
signalSignal p f =
  pure $ ValueFunction p (\x -> 
    case x of
      ValueSignal _ x' -> pure $ ValueSignal p (f x')
      ValueInt _ x' -> pure $ ValueSignal p (f $ Constant $ toNumber x')
      ValueNumber _ x' -> pure $ ValueSignal p (f $ Constant x')
      y -> throwError $ ParseError "expected Signal" (valuePosition y)
    )

stringSignal :: Position -> (String -> Signal) -> Either ParseError Value
stringSignal p f =
  pure $ ValueFunction p (\x -> 
    case x of
      ValueString _ x' -> pure $ ValueSignal p (f x')
      y -> throwError $ ParseError "expected String" (valuePosition y)
    )

signalSignalSignal :: Position -> (Signal -> Signal -> Signal) -> Either ParseError Value
signalSignalSignal p f = 
  pure $ ValueFunction p (\x ->
    case x of
      ValueSignal _ x' -> signalSignal p (f x')
      ValueInt _ x' -> signalSignal p (f $ Constant $ toNumber x')
      ValueNumber _ x' -> signalSignal p (f $ Constant x')
      y -> throwError $ ParseError "expected Signal" (valuePosition y)
    )

intSignalSignal :: Position -> (Int -> Signal -> Signal) -> Either ParseError Value
intSignalSignal p f = 
  pure $ ValueFunction p (\x ->
    case x of
      ValueInt _ x' -> signalSignal p (f x')
      y -> throwError $ ParseError "expected Int" (valuePosition y)
    )

signalSignalSignalSignal :: Position -> (Signal -> Signal -> Signal -> Signal) -> Either ParseError Value
signalSignalSignalSignal p f = 
  pure $ ValueFunction p (\x ->
    case x of
      ValueSignal _ x' -> signalSignalSignal p (f x')
      ValueInt _ x' -> signalSignalSignal p (f $ Constant $ toNumber x')
      ValueNumber _ x' -> signalSignalSignal p (f $ Constant x')
      y -> throwError $ ParseError "expected Signal" (valuePosition y)
    )
    
numberSignalSignalSignal :: Position -> (Number -> Signal -> Signal -> Signal) -> Either ParseError Value
numberSignalSignalSignal p f = 
  pure $ ValueFunction p (\x ->
    case x of
      ValueInt _ x' -> signalSignalSignal p (f $ toNumber x')
      ValueNumber _ x' -> signalSignalSignal p (f x')
      y -> throwError $ ParseError "expected Signal" (valuePosition y)
    )

-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listValueToValueSignal :: Position -> List Value -> P Value
listValueToValueSignal p xs = traverse valueToValueSignal xs >>= (pure <<< ValueSignal p <<< SignalList)

-- convert a Value to a Signal
-- succeeds only if the provided value can be meaningfully cast to a Signal
valueToValueSignal :: Value -> P Signal
valueToValueSignal (ValueSignal _ x) = pure x
valueToValueSignal (ValueInt _ x) = pure $ Constant $ toNumber x
valueToValueSignal (ValueNumber _ x) = pure $ Constant x
valueToValueSignal (ValueString p _) = throwError $ ParseError "expected Signal (found String)" p
valueToValueSignal (ValueFunction p _) = throwError $ ParseError "expected Signal (found Function)" p

