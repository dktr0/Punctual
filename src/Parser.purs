module Parser where

import Prelude (($),pure,bind,(<>),(>>=),(<<<),(<$>),class Applicative,discard)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (range,List(..),(:),head)
import Data.Traversable (traverse)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Trans (StateT,evalStateT,runStateT,get,modify_)
import Control.Monad.Error.Class (class MonadThrow,throwError)
import Parsing (ParseError(..),Position,runParser)
import Data.Map as Map
import Data.DateTime (DateTime)

import AST (AST,Expression(..),Statement,expression1,statement,parseAST)
import Program (Program)
import MultiMode (MultiMode(..))
import Signal (Signal(..),modulatedRangeLowHigh,modulatedRangePlusMinus,fit,fast,late)
import Value (Value(..),valuePosition,listValueToValueSignal,valueToSignal,class ToValue, class FromValue, toValue, fromValue, valueToAction, valueToFunction)
import Action (Action,signalToAction,setOutput,setCrossFade)
import Output (Output)
import Output as Output

type PState = Map.Map String Value

type P a = StateT PState (Either ParseError) a

runP :: forall a. Map.Map String Value -> P a -> Either ParseError (Tuple a PState)
runP d p = runStateT p d

parsePunctual :: String -> DateTime -> Either ParseError Program
parsePunctual txt eTime = do
  ast <- parseAST txt
  Tuple xs _ <- runP Map.empty $ astToListMaybeAction ast
  pure { actions: xs, evalTime: eTime }

-- just for testing
parseSignal :: String -> Either ParseError Signal
parseSignal txt = do
  ast <- parseAST txt
  Tuple xs _ <- runP Map.empty $ astToListMaybeAction ast 
  case head xs of
    Just mA -> do
      case mA of
        Just x -> pure x.signal
        Nothing -> pure $ Constant 0.0
    Nothing -> pure $ Constant 0.0
  
testAST :: String -> Either ParseError { actions :: List (Maybe Action) }
testAST txt = do
  ast <- parseAST txt
  Tuple xs _ <- runP Map.empty $ astToListMaybeAction ast
  pure { actions: xs }

testStatement :: String -> Either ParseError (Tuple (Maybe Action) PState)
testStatement txt = do
  stmt <- runParser txt statement
  runP Map.empty $ parseMaybeStatement stmt

testExpression :: String -> Either ParseError (Tuple Value PState)
testExpression txt = do
  exp <- runParser txt expression1
  runP Map.empty $ parseExpression exp

astToListMaybeAction :: AST -> P (List (Maybe Action))
astToListMaybeAction = traverse parseMaybeStatement

parseMaybeStatement :: Maybe Statement -> P (Maybe Action)
parseMaybeStatement Nothing = pure Nothing
parseMaybeStatement (Just stmt) = do
  v <- case stmt.identifiers of
    Nil -> parseExpression stmt.expression
    (x:xs) -> do
      v <- embedLambdas stmt.position xs stmt.expression
      let vForDefs = case v of -- if the value is an Action, we only want to store the signal component for later uses/references
                       ValueAction p a -> ValueSignal p a.signal
                       _ -> v
      modify_ $ Map.insert x vForDefs
      pure v
  case v of
      ValueAction _ a -> pure $ Just a
      _ -> pure Nothing

parseExpression :: Expression -> P Value
parseExpression (Reserved p x) = parseReserved p x
parseExpression (Identifier p x) = do
  s <- get
  case Map.lookup x s of
    Just v -> pure v
    Nothing -> throwError $ ParseError ("unrecognized identifier " <> x) p
parseExpression (LiteralInt p x) = pure $ ValueInt p x
parseExpression (LiteralNumber p x) = pure $ ValueNumber p x
parseExpression (LiteralString p x) = pure $ ValueString p x
parseExpression (ListExpression p xs) = traverse parseExpression xs >>= listValueToValueSignal p
parseExpression (Application _ f x) = do
  f' <- parseExpression f
  x' <- parseExpression x
  application f' x'
parseExpression (Operation p op x y) = do
  f <- parseOperator p op
  x' <- parseExpression x
  y' <- parseExpression y
  z <- application f x'
  application z y'
parseExpression (FromTo p x y) = pure $ ValueSignal p $ SignalList $ (Constant <<< toNumber) <$> range x y
parseExpression (FromThenTo p _ _ _) = throwError $ ParseError "FromThenTo not supported yet" p
-- TODO: implement FromThenTo and fix implementation of FromTo to match Haskell behaviour
parseExpression (Lambda p xs e) = embedLambdas p xs e
parseExpression (IfThenElse p i t e) = do
  i' <- parseExpression i >>= valueToSignal
  t' <- parseExpression t >>= valueToSignal
  e' <- parseExpression e >>= valueToSignal
  pure $ ValueSignal p $ Mix Combinatorial e' t' i'


application :: forall m. Applicative m => MonadThrow ParseError m => Value -> Value -> m Value
application f x = do
  f' <- valueToFunction f
  case f' x of
    Right a -> pure a
    Left err -> throwError err

  

parseReserved :: Position -> String -> P Value
parseReserved p "append" = lift $ signalSignalSignal p Append
parseReserved p "zip" = lift $ signalSignalSignal p Zip
parseReserved p "pi" = pure $ ValueSignal p Pi
parseReserved p "px" = pure $ ValueSignal p Px
parseReserved p "py" = pure $ ValueSignal p Py
parseReserved p "pxy" = pure $ ValueSignal p Pxy
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
parseReserved p "early" = lift $ signalSignalSignal p Early
parseReserved p "late" = lift $ signalSignalSignal p late
parseReserved p "slow" = lift $ signalSignalSignal p Slow
parseReserved p "fast" = lift $ signalSignalSignal p fast
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
parseReserved p "fft" = pure $ signalSignal p FFT
parseReserved p "ifft" = pure $ signalSignal p IFFT
parseReserved p "mono" = pure $ signalSignal p Mono
parseReserved p "rep" = lift $ intSignalSignal p Rep
parseReserved p "bipolar" = pure $ signalSignal p Bipolar
parseReserved p "unipolar" = pure $ signalSignal p Unipolar
parseReserved p "fb" = pure $ signalSignal p Fb
parseReserved p "img" = lift $ stringSignal p Img
parseReserved p "vid" = lift $ stringSignal p Vid
parseReserved p "cam" = pure $ ValueSignal p Cam
parseReserved p "rgbhsv" = pure $ signalSignal p RgbHsv
parseReserved p "hsvrgb" = pure $ signalSignal p HsvRgb
parseReserved p "hsvh" = pure $ signalSignal p HsvH
parseReserved p "hsvs" = pure $ signalSignal p HsvS
parseReserved p "hsvv" = pure $ signalSignal p HsvV
parseReserved p "hsvr" = pure $ signalSignal p HsvR
parseReserved p "hsvg" = pure $ signalSignal p HsvG
parseReserved p "hsvb" = pure $ signalSignal p HsvB
parseReserved p "rgbh" = pure $ signalSignal p RgbH
parseReserved p "rgbs" = pure $ signalSignal p RgbS
parseReserved p "rgbv" = pure $ signalSignal p RgbV
parseReserved p "rgbr" = pure $ signalSignal p RgbR
parseReserved p "rgbg" = pure $ signalSignal p RgbG
parseReserved p "rgbb" = pure $ signalSignal p RgbB
parseReserved p "osc" = pure $ signalSignal p Osc
parseReserved p "tri" = pure $ signalSignal p Tri
parseReserved p "saw" = pure $ signalSignal p Saw
parseReserved p "sqr" = pure $ signalSignal p Sqr
parseReserved p "lftri" = pure $ signalSignal p LFTri
parseReserved p "lfsaw" = pure $ signalSignal p LFSaw
parseReserved p "lfsqr" = pure $ signalSignal p LFSqr
parseReserved p "abs" = pure $ signalSignal p Abs
parseReserved p "acos" = pure $ signalSignal p Acos
parseReserved p "acosh" = pure $ signalSignal p Acosh
parseReserved p "asin" = pure $ signalSignal p Asin
parseReserved p "asinh" = pure $ signalSignal p Asinh
parseReserved p "atan" = pure $ signalSignal p Atan
parseReserved p "atanh" = pure $ signalSignal p Atanh
parseReserved p "cbrt" = pure $ signalSignal p Cbrt
parseReserved p "ceil" = pure $ signalSignal p Ceil
parseReserved p "cos" = pure $ signalSignal p Cos
parseReserved p "cosh" = pure $ signalSignal p Cosh
parseReserved p "exp" = pure $ signalSignal p Exp
parseReserved p "floor" = pure $ signalSignal p Floor
parseReserved p "log" = pure $ signalSignal p Log
parseReserved p "log2" = pure $ signalSignal p Log2
parseReserved p "log10" = pure $ signalSignal p Log10
parseReserved p "round" = pure $ signalSignal p Round
parseReserved p "sign" = pure $ signalSignal p Sign
parseReserved p "sin" = pure $ signalSignal p Sin
parseReserved p "sinh" = pure $ signalSignal p Sinh
parseReserved p "sqrt" = pure $ signalSignal p Sqrt
parseReserved p "tan" = pure $ signalSignal p Tan
parseReserved p "tanh" = pure $ signalSignal p Tanh
parseReserved p "trunc" = pure $ signalSignal p Trunc
parseReserved p "rtxy" = pure $ signalSignal p RtXy
parseReserved p "rtx" = pure $ signalSignal p RtX
parseReserved p "rty" = pure $ signalSignal p RtY
parseReserved p "xyrt" = pure $ signalSignal p XyRt
parseReserved p "xyr" = pure $ signalSignal p XyR
parseReserved p "xyt" = pure $ signalSignal p XyT
parseReserved p "point" = pure $ signalSignal p Point
parseReserved p "distance" = pure $ signalSignal p Distance
parseReserved p "prox" = pure $ signalSignal p Prox
parseReserved p "midicps" = pure $ signalSignal p MidiCps
parseReserved p "cpsmidi" = pure $ signalSignal p CpsMidi
parseReserved p "dbamp" = pure $ signalSignal p DbAmp
parseReserved p "ampdb" = pure $ signalSignal p AmpDb
parseReserved p "fract" = pure $ signalSignal p Fract
parseReserved p "max" = lift $ signalSignalSignal p $ Max Combinatorial
parseReserved p "maxp" = lift $ signalSignalSignal p $ Max Pairwise
parseReserved p "min" = lift $ signalSignalSignal p $ Min Combinatorial
parseReserved p "minp" = lift $ signalSignalSignal p $ Min Pairwise
parseReserved p "gate" = lift $ signalSignalSignal p $ Gate Combinatorial
parseReserved p "gatep" = lift $ signalSignalSignal p $ Gate Pairwise
parseReserved p "circle" = lift $ signalSignalSignal p $ Circle Combinatorial
parseReserved p "circlep" = lift $ signalSignalSignal p $ Circle Pairwise
parseReserved p "rect" = lift $ signalSignalSignal p $ Rect Combinatorial
parseReserved p "rectp" = lift $ signalSignalSignal p $ Rect Pairwise
parseReserved p "clip" = lift $ signalSignalSignal p $ Clip Combinatorial
parseReserved p "clipp" = lift $ signalSignalSignal p $ Clip Pairwise
parseReserved p "between" = lift $ signalSignalSignal p $ Between Combinatorial
parseReserved p "betweenp" = lift $ signalSignalSignal p $ Between Pairwise
parseReserved p "smoothstep" = lift $ signalSignalSignal p $ SmoothStep Combinatorial
parseReserved p "smoothstepp" = lift $ signalSignalSignal p $ SmoothStep Pairwise
parseReserved p "vline" = lift $ signalSignalSignal p $ VLine Combinatorial
parseReserved p "vlinep" = lift $ signalSignalSignal p $ VLine Pairwise
parseReserved p "hline" = lift $ signalSignalSignal p $ HLine Combinatorial
parseReserved p "hlinep" = lift $ signalSignalSignal p $ HLine Pairwise
parseReserved p "chain" = lift $ signalSignalSignal p $ Chain Combinatorial
parseReserved p "chainp" = lift $ signalSignalSignal p $ Chain Pairwise
parseReserved p "lines" = lift $ signalSignalSignal p $ Lines Combinatorial
parseReserved p "linesp" = lift $ signalSignalSignal p $ Lines Pairwise
parseReserved p "ilines" = lift $ signalSignalSignal p $ ILines Combinatorial
parseReserved p "ilinesp" = lift $ signalSignalSignal p $ ILines Pairwise
parseReserved p "mesh" = lift $ signalSignalSignal p $ Mesh Combinatorial
parseReserved p "meshp" = lift $ signalSignalSignal p $ Mesh Pairwise
parseReserved p "seq" = pure $ signalSignal p Seq
parseReserved p "fit" = lift $ signalSignalSignal p fit
parseReserved p "iline" = lift $ signalSignalSignalSignal p $ ILine Combinatorial
parseReserved p "ilinep" = lift $ signalSignalSignalSignal p $ ILine Pairwise
parseReserved p "line" = lift $ signalSignalSignalSignal p $ Line Combinatorial
parseReserved p "linep" = lift $ signalSignalSignalSignal p $ Line Pairwise
parseReserved p "linlin" = lift $ signalSignalSignalSignal p $ LinLin Combinatorial
parseReserved p "linlinp" = lift $ signalSignalSignalSignal p $ LinLin Pairwise
parseReserved p "mix" = lift $ signalSignalSignalSignal p $ Mix Combinatorial
parseReserved p "mixp" = lift $ signalSignalSignalSignal p $ Mix Pairwise
parseReserved p "lpf" = lift $ signalSignalSignalSignal p $ LPF Combinatorial
parseReserved p "lpfp" = lift $ signalSignalSignalSignal p $ LPF Pairwise
parseReserved p "hpf" = lift $ signalSignalSignalSignal p $ HPF Combinatorial
parseReserved p "hpfp" = lift $ signalSignalSignalSignal p $ HPF Pairwise
parseReserved p "bpf" = lift $ signalSignalSignalSignal p $ BPF Combinatorial
parseReserved p "bpfp" = lift $ signalSignalSignalSignal p $ BPF Pairwise

parseReserved p "delay" = lift $ numberSignalSignalSignal p Delay
parseReserved p "audio" = pure $ ValueOutput p Output.Audio
parseReserved p "blend" = pure $ ValuePolymorphic p (ValueOutput p Output.Blend : signalSignal p Blend : Nil)
parseReserved p "rgba" = pure $ ValueOutput p Output.RGBA -- TODO: rework FragmentShader.purs with support for RGBA as a Signal that accesses previous output
parseReserved p "add" = pure $ ValuePolymorphic p (ValueOutput p Output.Add : signalSignal p Add : Nil)
parseReserved p "mul" = pure $ ValuePolymorphic p (ValueOutput p Output.Mul : signalSignal p Mul : Nil)
parseReserved p "rgb" = pure $ ValueOutput p Output.RGB -- TODO: rework FragmentShader.purs with support for RGB as a Signal that accesses previous output
parseReserved p x = throwError $ ParseError ("internal error in Punctual: parseReserved called with unknown reserved word " <> x) p

parseOperator :: Position -> String -> P Value
parseOperator p ">>" = lift $ actionOutputAction p setOutput
parseOperator p "<>" = lift $ actionNumberAction p setCrossFade
parseOperator p "$" = pure $ ValueFunction p (\f -> pure $ ValueFunction p (\x -> application f x))
parseOperator p "&" = pure $ ValueFunction p (\x -> pure $ ValueFunction p (\f -> application f x))
parseOperator p "++" = lift $ signalSignalSignal p Append
parseOperator p "~~" = lift $ signalSignalSignalSignal p modulatedRangeLowHigh
parseOperator p "+-" = lift $ signalSignalSignalSignal p modulatedRangePlusMinus
parseOperator p "+" = lift $ signalSignalSignal p $ Addition Combinatorial
parseOperator p "-" = lift $ signalSignalSignal p $ Difference Combinatorial
parseOperator p "*" = lift $ signalSignalSignal p $ Product Combinatorial
parseOperator p "/" = lift $ signalSignalSignal p $ Division Combinatorial
parseOperator p "%" = lift $ signalSignalSignal p $ Mod Combinatorial
parseOperator p "**" = lift $ signalSignalSignal p $ Pow Combinatorial
parseOperator p "==" = lift $ signalSignalSignal p $ Equal Combinatorial
parseOperator p "/=" = lift $ signalSignalSignal p $ NotEqual Combinatorial
parseOperator p ">" = lift $ signalSignalSignal p $ GreaterThan Combinatorial
parseOperator p "<" = lift $ signalSignalSignal p $ LessThan Combinatorial
parseOperator p ">=" = lift $ signalSignalSignal p $ GreaterThanEqual Combinatorial
parseOperator p "<=" = lift $ signalSignalSignal p $ LessThanEqual Combinatorial
parseOperator p "+:" = lift $ signalSignalSignal p $ Addition Pairwise
parseOperator p "-:" = lift $ signalSignalSignal p $ Difference Pairwise
parseOperator p "*:" = lift $ signalSignalSignal p $ Product Pairwise
parseOperator p "/:" = lift $ signalSignalSignal p $ Division Pairwise
parseOperator p "%:" = lift $ signalSignalSignal p $ Mod Pairwise
parseOperator p "**:" = lift $ signalSignalSignal p $ Pow Pairwise
parseOperator p "==:" = lift $ signalSignalSignal p $ Equal Pairwise
parseOperator p "/=:" = lift $ signalSignalSignal p $ NotEqual Pairwise
parseOperator p ">:" = lift $ signalSignalSignal p $ GreaterThan Pairwise
parseOperator p "<:" = lift $ signalSignalSignal p $ LessThan Pairwise
parseOperator p ">=:" = lift $ signalSignalSignal p $ GreaterThanEqual Pairwise
parseOperator p "<=:" = lift $ signalSignalSignal p $ LessThanEqual Pairwise
parseOperator p x = throwError $ ParseError ("internal error in Punctual: parseOperator called with unsupported operator " <> x) p


ab :: forall a b. FromValue a => ToValue b => Position -> (a -> b) -> Value
ab p f = ValueFunction p (\v -> fromValue v >>= (pure <<< toValue p <<< f))

abc :: forall a b c. FromValue a => FromValue b => ToValue c => Position -> (a -> b -> c) -> Value
abc p f = ValueFunction p (\v -> fromValue v >>= (pure <<< ab p <<< f ))

abcd :: forall a b c d. FromValue a => FromValue b => FromValue c => ToValue d => Position -> (a -> b -> c -> d) -> Value
abcd p f = ValueFunction p (\v -> fromValue v >>= (pure <<< abc p <<< f ))


-- todo: replace signalSignal calls with direct calls to ab
signalSignal :: Position -> (Signal -> Signal) -> Value
signalSignal = ab 

-- TODO: replace stringSignal calls with direct calls to ab
stringSignal :: Position -> (String -> Signal) -> Either ParseError Value
stringSignal p f = pure $ ab p f

aAction :: forall a. FromValue a => Position -> (a -> Action) -> Value
aAction p f = ValueFunction p (\v -> fromValue v >>= (pure <<< ValueAction p <<< f))

-- TODO: replace outputAction calls with direct calls to aAction
outputAction :: Position -> (Output -> Action) -> Either ParseError Value
outputAction p f = pure $ aAction p f

-- TODO: replace numberAction calls with direct calls to aAction
numberAction :: Position -> (Number -> Action) -> Either ParseError Value
numberAction p f = pure $ aAction p f


-- TODO: replace signalSignalSignal calls with direct calls to abc
signalSignalSignal :: Position -> (Signal -> Signal -> Signal) -> Either ParseError Value
signalSignalSignal p f = pure $ abc p f

-- TODO: replace intSignalSignal calls with direct calls to abc
intSignalSignal :: Position -> (Int -> Signal -> Signal) -> Either ParseError Value
intSignalSignal p f = pure $ abc p f

actionAAction :: forall a. FromValue a => Position -> (Action -> a -> Action) -> Value
actionAAction p f = ValueFunction p (\v -> valueToAction v >>= (pure <<< aAction p <<< f))

-- TODO: replace actionOutputAction calls with direct calls to actionAAction
actionOutputAction :: Position -> (Action -> Output -> Action) -> Either ParseError Value
actionOutputAction p f = pure $ actionAAction p f 

-- TODO: replace actionNumberAction calls with direct calls to actionAAction
actionNumberAction :: Position -> (Action -> Number -> Action) -> Either ParseError Value
actionNumberAction p f = pure $ actionAAction p f

-- TODO: replace signalSignalSignalSignal calls with direct calls to abcd
signalSignalSignalSignal :: Position -> (Signal -> Signal -> Signal -> Signal) -> Either ParseError Value
signalSignalSignalSignal p f = pure $ abcd p f

-- TODO: replace numberSignalSignalSignal calls with direct calls to abcd
numberSignalSignalSignal :: Position -> (Number -> Signal -> Signal -> Signal) -> Either ParseError Value
numberSignalSignalSignal p f = pure $ abcd p f


embedLambdas :: Position -> List String -> Expression -> P Value
embedLambdas _ Nil e = parseExpression e
embedLambdas p (x:xs) e = do
  s <- get
  pure $ ValueFunction p (\v -> evalStateT (embedLambdas (valuePosition v) xs e) (Map.insert x v s))
