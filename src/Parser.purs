module Parser where

import Prelude (($),pure,bind,(<>),(>>=),(<<<),(<$>),class Applicative,discard)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (range,List(..),(:))
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
import Signal (Signal(..),modulatedRangeLowHigh,modulatedRangePlusMinus)
import Value (Value(..),valuePosition,listValueToValueSignal,valueToSignal)
import Action (Action,signalToAction,setOutput,setCrossFade)
import Output (Output(..))

type PState = Map.Map String Value

type P a = StateT PState (Either ParseError) a

runP :: forall a. Map.Map String Value -> P a -> Either ParseError (Tuple a PState)
runP d p = runStateT p d

parsePunctual :: String -> DateTime -> Either ParseError Program
parsePunctual txt eTime = do
  ast <- parseAST txt
  Tuple xs _ <- runP Map.empty $ astToListMaybeAction ast
  pure { actions: xs, evalTime: eTime }

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
  pure $ ValueSignal p $ Mix Combinatorial i' e' t'
  
  
application :: forall m. Applicative m => MonadThrow ParseError m => Value -> Value -> m Value
application f x = do
  case f of
    ValueFunction _ f' -> do
      case f' x of
        Left err -> throwError err
        Right y -> pure y
    v -> throwError $ ParseError "expected function" (valuePosition v)


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
parseReserved p "step" = lift $ signalSignalSignal p Step
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
parseReserved p "audio" = pure $ ValueOutput p Audio
parseReserved p "video" = pure $ ValueOutput p Video
parseReserved p "rgba" = pure $ ValueOutput p RGBA
parseReserved p "rgb" = pure $ ValueOutput p Video
parseReserved p "alpha" = pure $ ValueOutput p Alpha
parseReserved p x = throwError $ ParseError ("internal error in Punctual: parseReserved called with unknown reserved word " <> x) p

parseOperator :: Position -> String -> P Value
parseOperator p ">>" = lift $ actionOutputAction p setOutput
parseOperator p "<>" = lift $ actionNumberAction p setCrossFade
parseOperator p "$" = pure $ ValueFunction p (\f -> pure $ ValueFunction p (\x -> application f x))
parseOperator p "&" = pure $ ValueFunction p (\x -> pure $ ValueFunction p (\f -> application f x))
parseOperator p "++" = lift $ signalSignalSignal p Zip
parseOperator p "~~" = lift $ signalSignalSignalSignal p modulatedRangeLowHigh
parseOperator p "+-" = lift $ signalSignalSignalSignal p modulatedRangePlusMinus
parseOperator p "+" = lift $ signalSignalSignal p $ Sum Combinatorial
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
parseOperator p "+:" = lift $ signalSignalSignal p $ Sum Pairwise
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

outputAction :: Position -> (Output -> Action) -> Either ParseError Value
outputAction p f =
  pure $ ValueFunction p (\x ->
    case x of
      ValueOutput _ x' -> pure $ ValueAction p (f x')
      y -> throwError $ ParseError "expected Output" (valuePosition y)
    )

numberAction :: Position -> (Number -> Action) -> Either ParseError Value
numberAction p f =
  pure $ ValueFunction p (\x ->
    case x of
      ValueNumber _ x' -> pure $ ValueAction p (f x')
      ValueInt _ x' -> pure $ ValueAction p (f $ toNumber x')
      y -> throwError $ ParseError "expected Number or Int" (valuePosition y)
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

actionOutputAction :: Position -> (Action -> Output -> Action) -> Either ParseError Value
actionOutputAction p f =
  pure $ ValueFunction p (\x ->
    case x of
      ValueAction _ x' -> outputAction p (f x')
      ValueSignal _ x' -> outputAction p (f $ signalToAction x')
      ValueNumber _ x' -> outputAction p (f $ signalToAction $ Constant x')
      ValueInt _ x' -> outputAction p (f $ signalToAction $ Constant $ toNumber x')
      y -> throwError $ ParseError "expected Signal or Action" (valuePosition y)
    )

actionNumberAction :: Position -> (Action -> Number -> Action) -> Either ParseError Value
actionNumberAction p f =
  pure $ ValueFunction p (\x ->
    case x of
      ValueAction _ x' -> numberAction p (f x')
      ValueSignal _ x' -> numberAction p (f $ signalToAction x')
      y -> throwError $ ParseError "expected Signal or Action" (valuePosition y)
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

embedLambdas :: Position -> List String -> Expression -> P Value
embedLambdas _ Nil e = parseExpression e
embedLambdas p (x:xs) e = do
  s <- get
  pure $ ValueFunction p (\v -> evalStateT (embedLambdas (valuePosition v) xs e) (Map.insert x v s))
