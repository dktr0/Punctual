module Parser where

import Prelude (bind, discard, pure, unit, ($), (<$>), (<<<), (<>), (>>=), max, show, Unit, flip)
import Control.Applicative (class Applicative)
import Control.Monad (class Monad)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..),(:))
import Data.Traversable (traverse)
import Data.Map (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Trans (get, modify_, put)
import Control.Monad.Error.Class (throwError,class MonadThrow)
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

import AST (AST, Expression(..), Statement, parseAST, expressionPosition)
import Program (Program)
import MultiMode (MultiMode(..))
import Signal (Signal(..),modulatedRangeLowHigh,modulatedRangePlusMinus,fit,fast,late,zero,SignalSignal(..))
import Value (Library, LibraryCache, P, Variant(..), fromVariant, listVariantToVariantSignal, runP, toVariant, class ToVariant, class FromVariant, variantFunction, variantFunction2, variantFunction3, application, VariantFunction(..), variantExpression, variantPosition)
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
    (x:Nil) -> do
      v <- parseExpression stmt.expression
      let vForDefs = case v.variant of
                       Action a -> Signal a.signal
                       anythingElse -> anythingElse
      modify_ $ Map.insert x vForDefs
      pure v
    (x:xs) -> do
      v <- embedLambdas stmt.position xs stmt.expression
      modify_ $ Map.insert x v
      pure v
  case v.variant of
      Action a -> pure $ Just a
      _ -> pure Nothing

parseExpression :: Expression -> P Variant
parseExpression (Reserved p x) = parseReserved p x
parseExpression (Identifier p x) = do
  s <- get
  case Map.lookup x s of
    Just v -> pure v
    Nothing -> throwError $ ParseError ("unrecognized identifier " <> x) p
parseExpression e@(LiteralInt _ x) = pure $ toVariant e x
parseExpression e@(LiteralNumber _ x) = pure $ toVariant e x
parseExpression e@(LiteralString _ x) = pure $ toVariant e x
parseExpression (ListExpression p mm xs) = traverse parseExpression xs >>= listVariantToVariantSignal p mm
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
parseExpression e@(FromTo _ x y) = pure $ toVariant e $ SignalList Combinatorial $ Constant <$> fromTo x y
parseExpression e@(FromThenTo _ a b c) = pure $ toVariant e $ SignalList Combinatorial $ Constant <$> fromThenTo a b c
parseExpression (Lambda p xs e) = embedLambdas p xs e
parseExpression e@(IfThenElse _ i t el) = do
  i' <- parseExpression i >>= fromVariant
  t' <- parseExpression t >>= fromVariant
  el' <- parseExpression el >>= fromVariant
  pure $ toVariant e $ Mix Combinatorial el' t' i'

{-
-- application :: forall m. Applicative m => MonadThrow ParseError m => Value -> Value -> m Value
application :: Value -> Value -> P Value
application f x = do
  f' <- fromValue f
  f' x
-}

parseReserved :: Expression -> String -> P Variant
parseReserved p "append" = signalSignalSignal p Append
parseReserved p "zip" = signalSignalSignal p Zip
parseReserved p "pi" = signal p Pi
parseReserved p "px" = signal p Px
parseReserved p "py" = signal p Py
parseReserved p "pxy" = signal p Pxy
parseReserved p "aspect" = signal p Aspect
parseReserved p "fx" = signal p Fx
parseReserved p "fy" = signal p Fy
parseReserved p "fxy" = signal p Fxy
parseReserved p "frt" = signal p FRt
parseReserved p "fr" = signal p FR
parseReserved p "ft" = signal p FT
parseReserved p "setfx" = signalSignalSignal p SetFx
parseReserved p "setfy" = signalSignalSignal p SetFy
parseReserved p "setfxy" = signalSignalSignal p SetFxy
parseReserved p "zoom" = signalSignalSignal p Zoom
parseReserved p "zoomxy" = signalSignalSignal p ZoomXy
parseReserved p "zoomx" = signalSignalSignal p ZoomX
parseReserved p "zoomy" = signalSignalSignal p ZoomY
parseReserved p "move" = signalSignalSignal p Move
parseReserved p "tile" = signalSignalSignal p Tile
parseReserved p "tilexy" = signalSignalSignal p TileXy
parseReserved p "tilex" = signalSignalSignal p TileX
parseReserved p "tiley" = signalSignalSignal p TileY
parseReserved p "spin" = signalSignalSignal p Spin
parseReserved p "early" = signalSignalSignal p Early
parseReserved p "late" = signalSignalSignal p late
parseReserved p "slow" = signalSignalSignal p Slow
parseReserved p "fast" = signalSignalSignal p fast
parseReserved p "lo" = signal p Lo
parseReserved p "mid" = signal p Mid
parseReserved p "hi" = signal p Hi
parseReserved p "ilo" = signal p ILo
parseReserved p "imid" = signal p IMid
parseReserved p "ihi" = signal p IHi
parseReserved p "cps" = signal p Cps
parseReserved p "time" = signal p Time
parseReserved p "beat" = signal p Beat
parseReserved p "ebeat" = signal p EBeat
parseReserved p "etime" = signal p ETime
parseReserved p "rnd" = signal p Rnd
parseReserved p "audioin" = signal p $ AIn 1 0 -- deprecated, remove in 0.6
parseReserved p "mic" = signal p $ AIn 1 0
parseReserved p "ain" = intIntSignal p $ \n o -> AIn (max 1 n) (max 0 o)
parseReserved p "fft" = signal p FFT
parseReserved p "ifft" = signal p IFFT
parseReserved p "mono" = signalSignal p Mono
parseReserved p "rep" = intSignalSignal p Rep
parseReserved p "rows" = signalSignal p Rows
parseReserved p "cols" = signalSignal p Cols
parseReserved p "chns" = signalSignal p Chns
parseReserved p "flat" = signalSignal p Flat
parseReserved p "tspo" = signalSignal p Tspo
parseReserved p "get" = intIntSignalSignal p Get
parseReserved p "set" = intSignalSignalSignal p (Set Combinatorial)
parseReserved p "setp" = intSignalSignalSignal p (Set Pairwise)
parseReserved p "map" = mapForRmapRfor p Map
parseReserved p "for" = mapForRmapRfor p (flip Map)
parseReserved p "rmap" = mapForRmapRfor p Rmap
parseReserved p "rfor" = mapForRmapRfor p (flip Rmap)
parseReserved p "bipolar" = signalSignal p Bipolar
parseReserved p "unipolar" = signalSignal p Unipolar
parseReserved p "fb" = signal p Fb
parseReserved p "img" = stringSignal p Img
parseReserved p "imga" = stringSignal p Imga
parseReserved p "vid" = stringSignal p Vid
parseReserved p "vida" = stringSignal p Vida
parseReserved p "gdm" = stringSignal p Gdm
parseReserved p "gdma" = stringSignal p Gdma
parseReserved p "cam" = signal p Cam
parseReserved p "cama" = signal p Cama
parseReserved p "rgbhsv" = signalSignal p RgbHsv
parseReserved p "hsvrgb" = signalSignal p HsvRgb
parseReserved p "hsvh" = signalSignal p RgbR
parseReserved p "hsvs" = signalSignal p RgbG
parseReserved p "hsvv" = signalSignal p RgbB
parseReserved p "hsvr" = signalSignal p HsvR
parseReserved p "hsvg" = signalSignal p HsvG
parseReserved p "hsvb" = signalSignal p HsvB
parseReserved p "rgbh" = signalSignal p RgbH
parseReserved p "rgbs" = signalSignal p RgbS
parseReserved p "rgbv" = signalSignal p RgbV
parseReserved p "rgbr" = signalSignal p RgbR
parseReserved p "rgbg" = signalSignal p RgbG
parseReserved p "rgbb" = signalSignal p RgbB
parseReserved p "osc" = signalSignal p Osc
parseReserved p "tri" = signalSignal p Tri
parseReserved p "saw" = signalSignal p Saw
parseReserved p "sqr" = signalSignal p Sqr
parseReserved p "lftri" = signalSignal p LFTri
parseReserved p "lfsaw" = signalSignal p LFSaw
parseReserved p "lfsqr" = signalSignal p LFSqr
parseReserved p "abs" = signalSignal p Abs
parseReserved p "acos" = signalSignal p Acos
parseReserved p "acosh" = signalSignal p Acosh
parseReserved p "asin" = signalSignal p Asin
parseReserved p "asinh" = signalSignal p Asinh
parseReserved p "atan" = signalSignal p Atan
parseReserved p "atanh" = signalSignal p Atanh
parseReserved p "cbrt" = signalSignal p Cbrt
parseReserved p "ceil" = signalSignal p Ceil
parseReserved p "cos" = signalSignal p Cos
parseReserved p "cosh" = signalSignal p Cosh
parseReserved p "exp" = signalSignal p Exp
parseReserved p "floor" = signalSignal p Floor
parseReserved p "log" = signalSignal p Log
parseReserved p "log2" = signalSignal p Log2
parseReserved p "log10" = signalSignal p Log10
parseReserved p "round" = signalSignal p Round
parseReserved p "sign" = signalSignal p Sign
parseReserved p "sin" = signalSignal p Sin
parseReserved p "sinh" = signalSignal p Sinh
parseReserved p "sqrt" = signalSignal p Sqrt
parseReserved p "tan" = signalSignal p Tan
parseReserved p "tanh" = signalSignal p Tanh
parseReserved p "trunc" = signalSignal p Trunc
parseReserved p "rtxy" = signalSignal p RtXy
parseReserved p "rtx" = signalSignal p RtX
parseReserved p "rty" = signalSignal p RtY
parseReserved p "xyrt" = signalSignal p XyRt
parseReserved p "xyr" = signalSignal p XyR
parseReserved p "xyt" = signalSignal p XyT
parseReserved p "zero" = signalSignal p zero
parseReserved p "zer0" = signalSignal p zero
parseReserved p "point" = signalSignal p Point
parseReserved p "dist" = signalSignal p Dist
parseReserved p "prox" = signalSignal p Prox
parseReserved p "midicps" = signalSignal p MidiCps
parseReserved p "cpsmidi" = signalSignal p CpsMidi
parseReserved p "dbamp" = signalSignal p DbAmp
parseReserved p "ampdb" = signalSignal p AmpDb
parseReserved p "fract" = signalSignal p Fract
parseReserved p "max" = signalSignalSignal p $ Max Combinatorial
parseReserved p "maxp" = signalSignalSignal p $ Max Pairwise
parseReserved p "min" = signalSignalSignal p $ Min Combinatorial
parseReserved p "minp" = signalSignalSignal p $ Min Pairwise
parseReserved p "gate" = signalSignalSignal p $ Gate Combinatorial
parseReserved p "gatep" = signalSignalSignal p $ Gate Pairwise
parseReserved p "circle" = signalSignalSignal p $ Circle Combinatorial
parseReserved p "circlep" = signalSignalSignal p $ Circle Pairwise
parseReserved p "rect" = signalSignalSignal p $ Rect Combinatorial
parseReserved p "rectp" = signalSignalSignal p $ Rect Pairwise
parseReserved p "clip" = signalSignalSignal p $ Clip Combinatorial
parseReserved p "clipp" = signalSignalSignal p $ Clip Pairwise
parseReserved p "between" = signalSignalSignal p $ Between Combinatorial
parseReserved p "betweenp" = signalSignalSignal p $ Between Pairwise
parseReserved p "smoothstep" = signalSignalSignal p $ SmoothStep Combinatorial
parseReserved p "smoothstepp" = signalSignalSignal p $ SmoothStep Pairwise
parseReserved p "vline" = signalSignalSignal p $ VLine Combinatorial
parseReserved p "vlinep" = signalSignalSignal p $ VLine Pairwise
parseReserved p "hline" = signalSignalSignal p $ HLine Combinatorial
parseReserved p "hlinep" = signalSignalSignal p $ HLine Pairwise
parseReserved p "chain" = signalSignalSignal p $ Chain Combinatorial
parseReserved p "chainp" = signalSignalSignal p $ Chain Pairwise
parseReserved p "lines" = signalSignalSignal p $ Lines Combinatorial
parseReserved p "linesp" = signalSignalSignal p $ Lines Pairwise
parseReserved p "ilines" = signalSignalSignal p $ ILines Combinatorial
parseReserved p "ilinesp" = signalSignalSignal p $ ILines Pairwise
parseReserved p "mesh" = signalSignalSignal p $ Mesh Combinatorial
parseReserved p "meshp" = signalSignalSignal p $ Mesh Pairwise
parseReserved p "spr" = signalSignalSignal p $ Spr Combinatorial
parseReserved p "sprp" = signalSignalSignal p $ Spr Pairwise
parseReserved p "btw" = signalSignalSignalSignal p $ Btw Combinatorial
parseReserved p "btwp" = signalSignalSignalSignal p $ Btw Pairwise
parseReserved p "pan" = signalSignalSignalSignal p $ Pan Combinatorial
parseReserved p "panp" = signalSignalSignalSignal p $ Pan Pairwise
parseReserved p "splay" = intSignalSignal p $ Splay
parseReserved p "seq" = signalSignal p Seq
parseReserved p "fit" = signalSignalSignal p fit
parseReserved p "iline" = signalSignalSignalSignal p $ ILine Combinatorial
parseReserved p "ilinep" = signalSignalSignalSignal p $ ILine Pairwise
parseReserved p "line" = signalSignalSignalSignal p $ Line Combinatorial
parseReserved p "linep" = signalSignalSignalSignal p $ Line Pairwise
parseReserved p "linlin" = signalSignalSignalSignal p $ LinLin Combinatorial
parseReserved p "linlinp" = signalSignalSignalSignal p $ LinLin Pairwise
parseReserved p "mix" = signalSignalSignalSignal p $ Mix Combinatorial
parseReserved p "mixp" = signalSignalSignalSignal p $ Mix Pairwise
parseReserved p "lpf" = signalSignalSignalSignal p $ LPF Combinatorial
parseReserved p "lpfp" = signalSignalSignalSignal p $ LPF Pairwise
parseReserved p "hpf" = signalSignalSignalSignal p $ HPF Combinatorial
parseReserved p "hpfp" = signalSignalSignalSignal p $ HPF Pairwise
parseReserved p "bpf" = signalSignalSignalSignal p $ BPF Combinatorial
parseReserved p "bpfp" = signalSignalSignalSignal p $ BPF Pairwise
parseReserved p "delay" = numberSignalSignalSignal p Delay
parseReserved p "audio" = pure $ toVariant p Output.Audio
parseReserved p "stereo" = pure $ toVariant p (Output.AOut 0 2)
parseReserved p "aout" = pure $ abc p Output.AOut
parseReserved p "blend" = pure $ { expression: p, variant: OutputOrSignalSignal Output.Blend Blend }
parseReserved p "rgba" = pure $ toVariant p Output.RGBA -- TODO: rework FragmentShader.purs with support for RGBA as a Signal that accesses previous output
parseReserved p "add" = pure $ { expression: p, variant: OutputOrSignalSignal Output.Add Add }
parseReserved p "mul" = pure $ { expression: p, variant: OutputOrSignalSignal Output.Mul Mul }
parseReserved p "rgb" = pure $ toVariant p Output.RGB -- TODO: rework FragmentShader.purs with support for RGB as a Signal that accesses previous output
parseReserved p "import" = ab p (importLibrary p)
parseReserved p x = throwError $ ParseError ("internal error in Punctual: parseReserved called with unknown reserved word " <> x) p

parseOperator :: Position -> String -> P Variant
parseOperator p ">>" = actionOutputAction p setOutput
parseOperator p "<>" = actionNumberAction p setCrossFade
parseOperator p "$" = applicationOperator p
parseOperator p "&" = reverseApplicationOperator p
parseOperator p "++" = signalSignalSignal p Append
parseOperator p "~~" = signalSignalSignalSignal p (modulatedRangeLowHigh Combinatorial)
parseOperator p "~~:" = signalSignalSignalSignal p (modulatedRangeLowHigh Pairwise)
parseOperator p "+-" = signalSignalSignalSignal p modulatedRangePlusMinus
parseOperator p "+" = signalSignalSignal p $ Addition Combinatorial
parseOperator p "-" = signalSignalSignal p $ Difference Combinatorial
parseOperator p "*" = signalSignalSignal p $ Product Combinatorial
parseOperator p "/" = signalSignalSignal p $ Division Combinatorial
parseOperator p "%" = signalSignalSignal p $ Mod Combinatorial
parseOperator p "**" = signalSignalSignal p $ Pow Combinatorial
parseOperator p "==" = signalSignalSignal p $ Equal Combinatorial
parseOperator p "/=" = signalSignalSignal p $ NotEqual Combinatorial
parseOperator p ">" = signalSignalSignal p $ GreaterThan Combinatorial
parseOperator p "<" = signalSignalSignal p $ LessThan Combinatorial
parseOperator p ">=" = signalSignalSignal p $ GreaterThanEqual Combinatorial
parseOperator p "<=" = signalSignalSignal p $ LessThanEqual Combinatorial
parseOperator p "+:" = signalSignalSignal p $ Addition Pairwise
parseOperator p "-:" = signalSignalSignal p $ Difference Pairwise
parseOperator p "*:" = signalSignalSignal p $ Product Pairwise
parseOperator p "/:" = signalSignalSignal p $ Division Pairwise
parseOperator p "%:" = signalSignalSignal p $ Mod Pairwise
parseOperator p "**:" = signalSignalSignal p $ Pow Pairwise
parseOperator p "==:" = signalSignalSignal p $ Equal Pairwise
parseOperator p "/=:" = signalSignalSignal p $ NotEqual Pairwise
parseOperator p ">:" = signalSignalSignal p $ GreaterThan Pairwise
parseOperator p "<:" = signalSignalSignal p $ LessThan Pairwise
parseOperator p ">=:" = signalSignalSignal p $ GreaterThanEqual Pairwise
parseOperator p "<=:" = signalSignalSignal p $ LessThanEqual Pairwise
parseOperator p x = throwError $ ParseError ("internal error in Punctual: parseOperator called with unsupported operator " <> x) p

applicationOperator :: Expression -> m Variant
applicationOperator e = pure $ toVariant e $ VariantFunction $ \v -> application v

reverseApplicationOperator :: Expression -> m Variant
reverseApplicationOperator e = pure $ toVariant e $ VariantFunction $ \v -> (flip application) v

-- * 

signal :: forall m. Applicative m => Expression -> Signal -> m Variant
signal e x = pure $ toVariant e x


-- * -> *

ab :: forall a b m. Applicative m => FromVariant a => ToVariant b => Expression -> (a -> b) -> m Variant
ab e f = pure $ variantFunction e f

signalSignal :: forall m. Applicative m => Expression -> (Signal -> Signal) -> m Variant
signalSignal e f = pure { expression: e, variant: SignalSignal_v f }

stringSignal :: forall m. Applicative m => Expression -> (String -> Signal) -> m Variant
stringSignal = ab

aAction :: forall a m. Applicative m => FromVariant a => Expression -> (a -> Action) -> m Variant
aAction = ab

outputAction :: forall m. Applicative m => Expression -> (Output -> Action) -> m Variant
outputAction = ab

numberAction :: forall m. Applicative m => Expression -> (Number -> Action) -> m Variant
numberAction = ab


-- * -> * -> *

abc :: forall a b c m. Applicative m => FromVariant a => FromVariant b => ToVariant c => Expression -> (a -> b -> c) -> m Variant
abc e f = pure $ variantFunction2 e f

signalSignalSignal :: forall m. Applicative m => Expression -> (Signal -> Signal -> Signal) -> m Variant
signalSignalSignal = aSignalSignal

intSignalSignal :: forall m. Applicative m => Position -> (Int -> Signal -> Signal) -> m Variant
intSignalSignal = aSignalSignal

intIntSignal :: forall m. Applicative m => Expression -> (Int -> Int -> Signal) -> m Variant
intIntSignal = abc

aSignalSignal :: forall a m. Applicative m => FromVariant a => Expression -> (a -> Signal -> Signal) -> m Variant
aSignalSignal e f = pure $ VariantFunction_v e $ VariantFunction $ \v ->
  case fromVariant v of
    Left err -> Left err
    Right a -> do
      let e' = Application (expressionPosition e) e (variantExpression v)
      Right $ toVariant e' $ SignalSignal { signalSignal: f a, expression: e' }

mapForRmapRfor :: forall m. Applicative m => Expression -> ((Signal -> Signal) -> Signal -> Signal) -> m Variant
mapForRmapRfor e f = pure $ VariantFunction_v e $ VariantFunction $ \v ->
  case fromVariant v of
    Left err -> Left err
    Right (SignalSignal a) -> do
      let e' = Application (expressionPosition e) e (variantExpression v)
      Right $ toVariant e' $ SignalSignal { signalSignal: f a.signalSignal, expression: e' }

actionAAction :: forall a m. Applicative m => FromVariant a => Expression -> (Action -> a -> Action) -> m Variant
actionAAction = abc 

actionOutputAction :: forall m. Applicative m => Expression -> (Action -> Output -> Action) -> m Variant
actionOutputAction = abc

actionNumberAction :: forall m. Applicative m => Expression -> (Action -> Number -> Action) -> m Variant
actionNumberAction = abc


-- * -> * -> * -> *

abcd :: forall a b c d m. Applicative m => FromVariant a => FromVariant b => FromVariant c => ToVariant d => Expression -> (a -> b -> c -> d) -> m Variant
abcd e f = pure $ variantFunction3 e f

signalSignalSignalSignal :: forall m. Applicative m => Expression -> (Signal -> Signal -> Signal -> Signal) -> m Variant
signalSignalSignalSignal = abSignalSignal

intIntSignalSignal :: forall m. Applicative m => Expression -> (Int -> Int -> Signal -> Signal) -> m Variant
intIntSignalSignal = abSignalSignal

intSignalSignalSignal :: forall m. Applicative m => Expression -> (Int -> Signal -> Signal -> Signal) -> m Variant
intSignalSignalSignal = abSignalSignal

signalSignalSignalSignal :: forall m. Applicative m => Expression -> (Signal -> Signal -> Signal -> Signal) -> m Variant
signalSignalSignalSignal = abSignalSignal

numberSignalSignalSignal :: forall m. Applicative m => Position -> (Number -> Signal -> Signal -> Signal) -> m Variant
numberSignalSignalSignal = abSignalSignal

abSignalSignal :: forall a b m. Applicative m => FromVariant a => FromVariant b => Expression -> (a -> b -> Signal -> Signal) -> m Variant
abSignalSignal e f = pure $ VariantFunction_v e $ VariantFunction $ \v ->
  case fromVariant v of
    Left err -> Left err
    Right a -> do
      let e' = Application (expressionPosition e) e (variantExpression v)
      aSignalSignal e' $ f a
      

embedLambdas :: Position -> List String -> Expression -> P Variant
embedLambdas p ks e = do 
  s <- get
  _embedLambdas s p ks e
 
_embedLambdas :: Library -> Position -> List String -> Expression -> P Variant
_embedLambdas s _ Nil e = do
  cachedS <- get
  put s
  a <- parseExpression e
  put cachedS
  pure a
_embedLambdas s p (k:ks) e = pure $ ValueFunction p (\v -> _embedLambdas (Map.insert k v s) p ks e)


importLibrary :: Variant -> P Variant
importLibrary v = do
  url <- fromVariant v
  libCache <- ask
  eErrLib <- liftAff $ loadLibrary libCache (variantPosition v) url
  case eErrLib of
   Left err -> throwError err
   Right lib -> do
     modify_ $ \s -> Map.union lib s
     pure $ toVariant v.expression 0


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
