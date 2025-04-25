module Parser where

import Prelude (bind, discard, pure, unit, ($), (<$>), (<<<), (<>), (>>=), max, show, Unit)
import Control.Monad (class Monad)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..),(:))
import Data.Traversable (traverse)
import Data.Map (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Trans (get, modify_, put)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Parsing (ParseError(..), Position)
import Data.Map as Map
import Data.DateTime (DateTime)
import Effect (Effect)
import Effect.Ref (new,read,write)
import Effect.Aff (Aff,runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Fetch (fetch)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)

import AST (AST, Expression(..), Statement, parseAST)
import Program (Program)
import MultiMode (MultiMode(..))
import Signal (Signal(..),modulatedRangeLowHigh,modulatedRangePlusMinus,fit,fast,late,zero)
import Value (class FromValue, class ToValue, Library, LibraryCache, P, Value(..), fromValue, listValueToValueSignal, runP, toValue, valueToAction, valueToFunction, valueToSignal, valueToString)
import Action (Action, setCrossFade, setOutput)
import Output (Output)
import Output as Output
import SharedResources (URL)
import Number (fromTo,fromThenTo)

parseProgram :: LibraryCache -> String -> DateTime -> Aff (Either ParseError Program)
parseProgram libCache txt eTime = do
  case parseAST txt of
    Left err -> pure (Left err)
    Right ast -> do
      eErrTup <- runP libCache Map.empty (astToListMaybeAction ast)
      case eErrTup of
        Left err -> pure (Left err)
        Right (Tuple xs _) -> pure $ Right { actions: xs, evalTime: eTime }

parseProgramTest :: String -> Effect Unit
parseProgramTest txt = do
  libCache <- new empty
  eTime <- nowDateTime
  runAff_ (log <<< show) $ parseProgram libCache txt eTime

parseLibrary :: LibraryCache -> String -> Aff (Either ParseError Library)
parseLibrary libCache txt = do
  case parseAST txt of
    Left err -> pure (Left err)
    Right ast -> do
      eErrTup <- runP libCache Map.empty (astToListMaybeAction ast) -- TODO: replace astToListMaybeAction with a parser that throws errors on actions, although this probably will work in the meantime
      case eErrTup of
        Left err -> pure (Left err)
        Right (Tuple _ lib) -> pure $ Right lib


-- just for testing
{-
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

testStatement :: String -> Either ParseError (Tuple (Maybe Action) Library)
testStatement txt = do
  stmt <- runParser txt statement
  runP Map.empty $ parseMaybeStatement stmt

testExpression :: String -> Either ParseError (Tuple Value Library)
testExpression txt = do
  exp <- runParser txt expression1
  runP Map.empty $ parseExpression exp
-}

astToListMaybeAction :: AST -> P (List (Maybe Action))
astToListMaybeAction = do
  _ <- pure unit
  traverse parseMaybeStatement

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
parseExpression (ListExpression p mm xs) = traverse parseExpression xs >>= listValueToValueSignal p mm
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
parseExpression (FromTo p x y) = pure $ ValueSignal p $ SignalList Combinatorial $ Constant <$> fromTo x y
parseExpression (FromThenTo p a b c) = pure $ ValueSignal p $ SignalList Combinatorial $ Constant <$> fromThenTo a b c
parseExpression (Lambda p xs e) = embedLambdas p xs e
parseExpression (IfThenElse p i t e) = do
  i' <- parseExpression i >>= valueToSignal
  t' <- parseExpression t >>= valueToSignal
  e' <- parseExpression e >>= valueToSignal
  pure $ ValueSignal p $ Mix Combinatorial e' t' i'


-- application :: forall m. Applicative m => MonadThrow ParseError m => Value -> Value -> m Value
application :: Value -> Value -> P Value
application f x = do
  f' <- valueToFunction f
  f' x


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
parseReserved p "zoomxy" = lift $ signalSignalSignal p ZoomXy
parseReserved p "zoomx" = lift $ signalSignalSignal p ZoomX
parseReserved p "zoomy" = lift $ signalSignalSignal p ZoomY
parseReserved p "move" = lift $ signalSignalSignal p Move
parseReserved p "tile" = lift $ signalSignalSignal p Tile
parseReserved p "tilexy" = lift $ signalSignalSignal p TileXy
parseReserved p "tilex" = lift $ signalSignalSignal p TileX
parseReserved p "tiley" = lift $ signalSignalSignal p TileY
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
parseReserved p "audioin" = pure $ ValueSignal p $ AIn 1 0 -- deprecated, remove in 0.6
parseReserved p "mic" = pure $ ValueSignal p $ AIn 1 0
parseReserved p "ain" = pure $ abc p $ \n o -> AIn (max 1 n) (max 0 o)
parseReserved p "fft" = pure $ ValueSignal p FFT
parseReserved p "ifft" = pure $ ValueSignal p IFFT
parseReserved p "mono" = pure $ signalSignal p Mono
parseReserved p "rep" = lift $ intSignalSignal p Rep
parseReserved p "bipolar" = pure $ signalSignal p Bipolar
parseReserved p "unipolar" = pure $ signalSignal p Unipolar
parseReserved p "fb" = pure $ ValueSignal p Fb
parseReserved p "img" = lift $ stringSignal p Img
parseReserved p "vid" = lift $ stringSignal p Vid
parseReserved p "imga" = lift $ stringSignal p Imga
parseReserved p "vida" = lift $ stringSignal p Vida
parseReserved p "gdm" = lift $ stringSignal p Gdm
parseReserved p "cam" = pure $ ValueSignal p Cam
parseReserved p "rgbhsv" = pure $ signalSignal p RgbHsv
parseReserved p "hsvrgb" = pure $ signalSignal p HsvRgb
parseReserved p "hsvh" = pure $ signalSignal p RgbR
parseReserved p "hsvs" = pure $ signalSignal p RgbG
parseReserved p "hsvv" = pure $ signalSignal p RgbB
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
parseReserved p "zero" = pure $ signalSignal p zero
parseReserved p "zer0" = pure $ signalSignal p zero
parseReserved p "point" = pure $ signalSignal p Point
parseReserved p "dist" = pure $ signalSignal p Dist
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
parseReserved p "spr" = lift $ signalSignalSignal p $ Spr Combinatorial
parseReserved p "sprp" = lift $ signalSignalSignal p $ Spr Pairwise
parseReserved p "btw" = pure $ abcd p $ Btw Combinatorial
parseReserved p "btwp" = pure $ abcd p $ Btw Pairwise
parseReserved p "pan" = pure $ abcd p $ Pan Combinatorial
parseReserved p "panp" = pure $ abcd p $ Pan Pairwise
parseReserved p "splay" = pure $ abc p $ Splay
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
parseReserved p "stereo" = pure $ ValueOutput p (Output.AOut 0 2)
parseReserved p "aout" = pure $ abc p Output.AOut
parseReserved p "blend" = pure $ ValuePolymorphic p (ValueOutput p Output.Blend : signalSignal p Blend : Nil)
parseReserved p "rgba" = pure $ ValueOutput p Output.RGBA -- TODO: rework FragmentShader.purs with support for RGBA as a Signal that accesses previous output
parseReserved p "add" = pure $ ValuePolymorphic p (ValueOutput p Output.Add : signalSignal p Add : Nil)
parseReserved p "mul" = pure $ ValuePolymorphic p (ValueOutput p Output.Mul : signalSignal p Mul : Nil)
parseReserved p "rgb" = pure $ ValueOutput p Output.RGB -- TODO: rework FragmentShader.purs with support for RGB as a Signal that accesses previous output
parseReserved p "import" = pure $ ValueFunction p (importLibrary p)
parseReserved p x = throwError $ ParseError ("internal error in Punctual: parseReserved called with unknown reserved word " <> x) p

parseOperator :: Position -> String -> P Value
parseOperator p ">>" = lift $ actionOutputAction p setOutput
parseOperator p "<>" = lift $ actionNumberAction p setCrossFade
parseOperator p "$" = pure $ ValueFunction p (\f -> pure $ ValueFunction p (\x -> application f x))
parseOperator p "&" = pure $ ValueFunction p (\x -> pure $ ValueFunction p (\f -> application f x))
parseOperator p "++" = lift $ signalSignalSignal p Append
parseOperator p "~~" = lift $ signalSignalSignalSignal p (modulatedRangeLowHigh Combinatorial)
parseOperator p "~~:" = lift $ signalSignalSignalSignal p (modulatedRangeLowHigh Pairwise)
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
stringSignal :: forall m. Monad m => Position -> (String -> Signal) -> m Value
stringSignal p f = pure $ ab p f

aAction :: forall a. FromValue a => Position -> (a -> Action) -> Value
aAction p f = ValueFunction p (\v -> fromValue v >>= (pure <<< ValueAction p <<< f))

-- TODO: replace outputAction calls with direct calls to aAction
outputAction :: forall m. Monad m => Position -> (Output -> Action) -> m Value
outputAction p f = pure $ aAction p f

-- TODO: replace numberAction calls with direct calls to aAction
numberAction :: forall m. Monad m => Position -> (Number -> Action) -> m Value
numberAction p f = pure $ aAction p f


-- TODO: replace signalSignalSignal calls with direct calls to abc
signalSignalSignal :: forall m. Monad m => Position -> (Signal -> Signal -> Signal) -> m Value
signalSignalSignal p f = pure $ abc p f

-- TODO: replace intSignalSignal calls with direct calls to abc
intSignalSignal :: forall m. Monad m => Position -> (Int -> Signal -> Signal) -> m Value
intSignalSignal p f = pure $ abc p f

actionAAction :: forall a. FromValue a => Position -> (Action -> a -> Action) -> Value
actionAAction p f = ValueFunction p (\v -> valueToAction v >>= (pure <<< aAction p <<< f))

-- TODO: replace actionOutputAction calls with direct calls to actionAAction
actionOutputAction :: forall m. Monad m => Position -> (Action -> Output -> Action) -> m Value
actionOutputAction p f = pure $ actionAAction p f

-- TODO: replace actionNumberAction calls with direct calls to actionAAction
actionNumberAction :: forall m. Monad m => Position -> (Action -> Number -> Action) -> m Value
actionNumberAction p f = pure $ actionAAction p f

-- TODO: replace signalSignalSignalSignal calls with direct calls to abcd
signalSignalSignalSignal :: forall m. Monad m => Position -> (Signal -> Signal -> Signal -> Signal) -> m Value
signalSignalSignalSignal p f = pure $ abcd p f

-- TODO: replace numberSignalSignalSignal calls with direct calls to abcd
numberSignalSignalSignal :: forall m. Monad m => Position -> (Number -> Signal -> Signal -> Signal) -> m Value
numberSignalSignalSignal p f = pure $ abcd p f


embedLambdas :: Position -> List String -> Expression -> P Value
embedLambdas p ks e = do 
  s <- get
  _embedLambdas s p ks e
 
_embedLambdas :: Library -> Position -> List String -> Expression -> P Value
_embedLambdas s _ Nil e = do
  cachedS <- get
  put s
  a <- parseExpression e
  put cachedS
  pure a
_embedLambdas s p (k:ks) e = pure $ ValueFunction p (\v -> _embedLambdas (Map.insert k v s) p ks e)


importLibrary :: Position -> Value -> P Value
importLibrary p v = do
  url <- valueToString v
  libCache <- ask
  eErrLib <- liftAff $ loadLibrary libCache p url
  case eErrLib of
   Left err -> throwError err
   Right lib -> do
     modify_ $ \s -> Map.union lib s
     pure $ ValueInt p 0


loadLibrary :: LibraryCache -> Position -> URL -> Aff (Either ParseError Library)
loadLibrary libCache p url = do
  libraries <- liftEffect $ read libCache
  case Map.lookup url libraries of
    Just r -> do
      log $ "using cached library " <> url
      pure (Right r)
    Nothing -> do
      log $ "loading library " <> url <> "..."
      eErrTxt <- loadTextFile url
      case eErrTxt of
        Left err -> pure $ Left $ ParseError err p
        Right txt -> do
          log $ "parsing library " <> url <> "..."
          eErrLib <- parseLibrary libCache txt
          case eErrLib of
            Left err -> pure $ Left $ err
            Right lib -> do
              log $ "successfully parsed library " <> url
              liftEffect $ write (Map.insert url lib libraries) libCache
              pure $ Right $ lib


loadTextFile :: URL -> Aff (Either String String)
loadTextFile url = do
  { text } <- fetch url {} -- { headers: {} }
  text' <- text
  log $ "loaded text file: " <> text'
  pure $ Right text'
