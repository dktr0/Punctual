{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (Sound.Punctual.Parser.parse) where

import Data.Text (Text)
import Data.Function ((&))
import qualified Data.Text as T
import Data.List
import Data.List.Split
import Data.Foldable (asum)
import Language.Haskell.Exts
import Language.Haskellish
import Data.IntMap.Strict as IntMap
import Data.Map as Map
import Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Control.Applicative
import Data.Time
import Data.Bifunctor
import Data.Char (isSpace)

import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Duration
import Sound.Punctual.DefTime
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((>>),(<>),graph,defTime)
import qualified Sound.Punctual.Action as P
import Sound.Punctual.Program


parse :: UTCTime -> Text -> Either String Program
parse eTime x = do
  let (x',pragmas) = extractPragmas x
  if (elem "glsl" pragmas) then do
    return $ (emptyProgram eTime) { directGLSL = Just x' }
  else first errorMessage $ parseProgram eTime $ T.unpack x'

extractPragmas :: Text -> (Text,[Text])
extractPragmas t = (newText,pragmas)
  where
    f "#glsl" = (T.empty,["glsl"])
    f x = (x,[])
    xs = fmap (f . T.stripEnd) $ T.lines t
    newText = T.unlines $ fmap fst xs
    pragmas = concat $ fmap snd xs


errorMessage :: (Span,Text) -> String
errorMessage (s,m) = show s ++ " " ++ T.unpack m

parseProgram :: UTCTime -> String -> Either (Span,Text) Program
parseProgram eTime x = do
  (p,st) <- parseWithModeAndRun haskellSrcExtsParseMode (program eTime) emptyParserState $ reformatProgramAsList $ removeComments x
  return $ p {
    textureSet = textureRefs st,
    programNeedsAudioInputAnalysis = audioInputAnalysis st,
    programNeedsAudioOutputAnalysis = audioOutputAnalysis st,
    programNeedsWebcam = needsWebcam st
  }

reformatProgramAsList :: String -> String
reformatProgramAsList x =
  let x' = splitOn ";" x
      x'' = Data.List.filter (\y -> length (dropWhile isSpace y) > 0) x'
      x''' = intercalate "," x''
  in "[" ++ x''' ++ "\n]"


type Identifier = String

data ParserState = ParserState {
  actionCount :: Int,
  textureRefs :: Set TextureRef,
  localBindings :: Map Identifier Int, -- eg. f x y = fromList [("x",0),("y",1)]
  definitions1 :: Map Identifier Graph,
--  definitions2 :: Map Identifier (Graph -> Graph),
--  definitions3 :: Map Identifier (Graph -> Graph -> Graph),
  audioInputAnalysis :: Bool,
  audioOutputAnalysis :: Bool,
  needsWebcam :: Bool
} deriving (Show)

emptyParserState :: ParserState
emptyParserState = ParserState {
  actionCount = 0,
  textureRefs = Set.empty,
  localBindings = Map.empty,
  definitions1 = Map.empty,
--  definitions2 = Map.empty,
--  definitions3 = Map.empty,
  audioInputAnalysis = False,
  audioOutputAnalysis = False,
  needsWebcam = False
  }

type H = Haskellish ParserState

haskellSrcExtsParseMode :: ParseMode
haskellSrcExtsParseMode = defaultParseMode {
      fixities = Just [
        Fixity (AssocRight ()) 9 (UnQual () (Symbol () ".")),
        Fixity (AssocRight ()) 8 (UnQual () (Symbol () "**")),
        Fixity (AssocRight ()) 8 (UnQual () (Symbol () "**:")),   
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "*")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "*:")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "/")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "/:")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "%")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "%:")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "+")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "+:")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "-")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "-:")),
        Fixity (AssocRight ()) 5 (UnQual () (Symbol () "++")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "==")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "==:")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "/=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "/=:")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<:")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<=:")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">=:")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">:")),
        Fixity (AssocRight ()) 3 (UnQual () (Symbol () "&&")),
        Fixity (AssocRight ()) 2 (UnQual () (Symbol () "||")),
        Fixity (AssocLeft ()) 0 (UnQual () (Symbol () ">>")), -- modified from Haskell default (1) to have equal priority to ops below...
        Fixity (AssocLeft ()) 1 (UnQual () (Symbol () ">>=")),
        Fixity (AssocRight ()) 1 (UnQual () (Symbol () "=<<")),
        Fixity (AssocRight ()) 1 (UnQual () (Symbol () "$")), -- is 0 in Haskell, changed to 1 to have less priority than ops below...
        Fixity (AssocRight ()) 0 (UnQual () (Symbol () "$!")),
        Fixity (AssocRight ()) 0 (UnQual () (Ident () "seq")), -- this line and above are fixities from defaultParseMode
        Fixity (AssocLeft ()) 0 (UnQual () (Symbol () "<>")), -- this line and below are fixities defined for Punctual's purposes...
        Fixity (AssocLeft ()) 0 (UnQual () (Symbol () "@@")),
        Fixity (AssocRight ()) 1 (UnQual () (Symbol () "<<"))
        ]
    }


program :: UTCTime -> H Program
program eTime = do
  xs <- list action
  let xs' = Data.List.filter (not . Data.List.null . Sound.Punctual.Action.outputs) xs
  return $ (emptyProgram eTime) { actions = IntMap.fromList $ zip [0..] xs' }

action :: H Action
action = asum [
  duration_action <*> duration,
  defTime_action <*> defTime,
  outputs_action <*> Sound.Punctual.Parser.outputs,
  actionFromGraph <$> graph
  ] <?> "expected Action"

duration_action :: H (Duration -> Action)
duration_action = action_duration_action <*> action

defTime_action :: H (DefTime -> Action)
defTime_action = action_defTime_action <*> action

outputs_action :: H ([Output] -> Action)
outputs_action = action_outputs_action <*> action

action_duration_action :: H (Action-> Duration -> Action)
action_duration_action = reserved "<>" >> return (P.<>)

action_defTime_action :: H (Action -> DefTime -> Action)
action_defTime_action = reserved "@@" >> return (@@)

action_outputs_action :: H (Action -> [Output] -> Action)
action_outputs_action = reserved ">>" >> return (P.>>)

number :: H Rational
number = asum [
  realToFrac <$> rationalOrInteger,
  reverseApplication number (reserved "m" >> return (realToFrac.midicps.realToFrac)),
  reverseApplication number (reserved "db" >> return (realToFrac.dbamp.realToFrac))
  ]

duration :: H Duration
duration = asum [
  Seconds <$> rationalOrInteger,
  reverseApplication number (reserved "s" >> return Seconds),
  reverseApplication number (reserved "ms" >> return (\x -> Seconds $ x/1000.0)),
  reverseApplication number (reserved "c" >> return Cycles)
  ]

defTime :: H DefTime
defTime = asum [
  (\(x,y) -> Quant x y) <$> Language.Haskellish.tuple number duration,
  After <$> duration
  ]

outputs :: H [Output]
outputs = asum [
  concat <$> list Sound.Punctual.Parser.outputs,
  ((:[]) . Panned . realToFrac) <$> rationalOrInteger,
  reserved "audio" >> return [Splay],
  reserved "video" >> return [RGB],
  reserved "rgba" >> return [RGBA],
  reserved "left" >> return [Panned 0],
  reserved "right" >> return [Panned 1],
  reserved "centre" >> return [Panned 0.5],
  reserved "splay" >> return [Splay],
  reserved "red" >> return [Red],
  reserved "green" >> return [Green],
  reserved "blue" >> return [Blue],
  reserved "hue" >> return [Hue],
  reserved "saturation" >> return [Saturation],
  reserved "value" >> return [Value],
  reserved "rgb" >> return [RGB],
  reserved "hsv" >> return [HSV],
  reserved "alpha" >> return [Alpha],
  reserved "fdbk" >> return [Fdbk]
  ]

definitions1H :: H Graph
definitions1H = do
  x <- Sound.Punctual.Parser.identifier
  m <- gets definitions1 -- Map Text Graph
  let xm = Map.lookup x m
  if isJust xm then return (fromJust xm) else nonFatal ""

debugExp :: Haskellish st a -> Haskellish st a
debugExp h = do
  e <- Language.Haskellish.exp
  throwError $ T.pack $ show e

graph :: H Graph
graph = asum [
  identifiedGraph,
  definitions1H,
  reverseApplication graph (reserved "m" >> return MidiCps),
  reverseApplication graph (reserved "db" >> return DbAmp),
  (Constant . realToFrac) <$> rational,
  (Constant . fromIntegral) <$> integer,
  Multi <$> list graph,
  multiSeries,
  img,
  vid,
  cam,
  reserved "pi" >> return Pi,
  reserved "audioin" >> return AudioIn,
  reserved "cps" >> return Cps,
  reserved "time" >> return Time,
  reserved "beat" >> return Beat,
  reserved "etime" >> return ETime,
  reserved "ebeat" >> return EBeat,
  reserved "rnd" >> return Rnd,
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy,
  reserved "fxy" >> return Fxy,
  reserved "px" >> return Px,
  reserved "py" >> return Py,
  reserved "frt" >> return FRt,
  reserved "fr" >> return FR,
  reserved "ft" >> return FT,
  reserved "aspect" >> return Aspect,
  reserved "lo" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Lo,
  reserved "mid" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Mid,
  reserved "hi" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Hi,
  reserved "ilo" >> modify (\s -> s { audioInputAnalysis = True } ) >> return ILo,
  reserved "imid" >> modify (\s -> s { audioInputAnalysis = True } ) >> return IMid,
  reserved "ihi" >> modify (\s -> s { audioInputAnalysis = True } ) >> return IHi,
  -- (reserved "sin" >> return Sin) <**!> graph,
  graph2 <*> graph,
  ifThenElseParser,
  graph2_graph <*> graph2
  ] <?> "expected Graph"

{- -- *** we've tested this and are pretty sure it's correct
(<**!>) :: Haskellish st (a -> b) -> Haskellish st a -> Haskellish st b
f <**!> x =
  (f <*> required x) <|> -- 1. succeeds when f and x succeed, and are applied to each other
  (f >> undermined x >> fatal "") -- 2. if f succeeds against the expression as a whole, force x to fail fatally
-}

ifThenElseParser :: H Graph
ifThenElseParser = do
  (a,b,c) <- ifThenElse graph graph graph
  return $ IfThenElse a b c

graph2 :: H (Graph -> Graph)
graph2 = asum [
  -- unary functions from JavaScript Math
  reserved "abs" >> return Abs,
  reserved "acos" >> return Acos,
  reserved "acosh" >> return Acosh,
  reserved "asin" >> return Asin,
  reserved "asinh" >> return Asinh,
  reserved "atan" >> return Atan,
  reserved "atanh" >> return Atanh,
  reserved "cbrt" >> return Cbrt,
  reserved "ceil" >> return Ceil,
  reserved "cos" >> return Cos,
  reserved "cosh" >> return Cosh,
  reserved "exp" >> return Exp,
  reserved "floor" >> return Floor,
  reserved "log" >> return Log,
  reserved "log2" >> return Log2,
  reserved "log10" >> return Log10,
  reserved "round" >> return Round,
  reserved "sign" >> return Sign,
  reserved "sin'" >> return Sin,
  reserved "sinh" >> return Sinh,
  reserved "sqrt" >> return Sqrt,
  reserved "tan" >> return Tan,
  reserved "tanh" >> return Tanh,
  reserved "trunc" >> return Trunc,
  -- other unary functions
  reserved "rtxy" >> return RtXy,
  reserved "rtx" >> return RtX,
  reserved "rty" >> return RtY,
  reserved "xyrt" >> return XyRt,
  reserved "xyr" >> return XyR,
  reserved "xyt" >> return XyT,
  reserved "zero" >> return ((*) 0),
  reserved "zer0" >> return ((*) 0),
  reserved "bipolar" >> return Bipolar,
  reserved "unipolar" >> return Unipolar,
  reserved "sin" >> return Osc, -- deprecated, so that sinf can become sin
  reserved "osc" >> return Osc,
  reserved "tri" >> return Tri,
  reserved "saw" >> return Saw,
  reserved "sqr" >> return Sqr,
  reserved "lftri" >> return LFTri,
  reserved "lfsaw" >> return LFSaw,
  reserved "lfsqr" >> return LFSqr,
  reserved "mono" >> return Mono,
  reserved "cpsmidi" >> return CpsMidi,
  reserved "midicps" >> return MidiCps,
  reserved "dbamp" >> return DbAmp,
  reserved "ampdb" >> return AmpDb,
  reserved "fract" >> return Fract,
  reserved "blend" >> return Blend,
  reserved "hsvrgb" >> return HsvRgb,
  reserved "rgbhsv" >> return RgbHsv,
  reserved "hsvh" >> return HsvH,
  reserved "hsvs" >> return HsvS,
  reserved "hsvv" >> return HsvV,
  reserved "hsvr" >> return HsvR,
  reserved "hsvg" >> return HsvG,
  reserved "hsvb" >> return HsvB,
  reserved "rgbh" >> return RgbH,
  reserved "rgbs" >> return RgbS,
  reserved "rgbv" >> return RgbV,
  reserved "rgbr" >> return RgbR,
  reserved "rgbg" >> return RgbG,
  reserved "rgbb" >> return RgbB,
  reserved "distance" >> return Distance, -- deprecated
  reserved "dist" >> return Distance,
  reserved "prox" >> return Prox,
  reserved "point" >> return Point,
  reserved "fb" >> return Fb,
  reserved "fft" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return FFT,
  reserved "ifft" >> modify (\s -> s { audioInputAnalysis = True } ) >> return IFFT,
  textureRef_graph_graph <*> textureRef,
  int_graph_graph <*> int,
  lGraph_graph_graph <*> (list graph <?> "expected list of Graphs"),
  graph3 <*> graph
  ] <?> "expected Graph -> Graph"


graph3 :: H (Graph -> Graph -> Graph)
graph3 = asum [
  reserved "++" >> return Append,
  reserved "zip" >> return Zip,

  -- combinatorial arithmetic operators
  reserved "+" >> return (+),
  reserved "-" >> return (-),
  reserved "*" >> return (Product Combinatorial),
  reserved "/" >> return (Division Combinatorial),
  reserved "%" >> return (Mod Combinatorial),
  reserved "**" >> return (Pow Combinatorial),
  reserved "==" >> return (Equal Combinatorial),
  reserved "/=" >> return (NotEqual Combinatorial),
  reserved ">" >> return (GreaterThan Combinatorial),
  reserved "<" >> return (LessThan Combinatorial),
  reserved ">=" >> return (GreaterThanOrEqual Combinatorial),
  reserved "<=" >> return (LessThanOrEqual Combinatorial),

  -- pairwise arithmetic operators
  reserved "+:" >> return (Sum PairWise),
  reserved "-:" >> return (\a b -> Sum PairWise a $ negate b),
  reserved "*:" >> return (Product PairWise),
  reserved "/:" >> return (Division PairWise),
  reserved "%:" >> return (Mod PairWise),
  reserved "**:" >> return (Pow PairWise),
  reserved "==:" >> return (Equal PairWise),
  reserved "/=:" >> return (NotEqual PairWise),
  reserved ">:" >> return (GreaterThan PairWise),
  reserved "<:" >> return (LessThan PairWise),
  reserved ">=:" >> return (GreaterThanOrEqual PairWise),
  reserved "<=:" >> return (LessThanOrEqual PairWise),

  -- other binary functions (with combinatorial semantics, generally speaking)
  reserved "fit" >> return fit,
  reserved "min" >> return (Min Combinatorial),
  reserved "max" >> return (Max Combinatorial),
  reserved "minp" >> return (Min PairWise),
  reserved "maxp" >> return (Max PairWise),
  reserved "hline" >> return HLine,
  reserved "vline" >> return VLine,
  reserved "circle" >> return Circle,
  reserved "rect" >> return Rect,
  reserved "clip" >> return Clip,
  reserved "between" >> return Between,
  reserved "when" >> return Sound.Punctual.Graph.when,
  reserved "gate" >> return (Gate Combinatorial),
  reserved "gatep" >> return (Gate PairWise),
  reserved "setfx" >> return SetFx,
  reserved "setfy" >> return SetFy,
  reserved "setfxy" >> return SetFxy,
  reserved "zoom" >> return Zoom,
  reserved "move" >> return Move,
  reserved "tile" >> return Tile,
  reserved "spin" >> return Spin,
  double_graph_graph_graph <*> double,
  graph4 <*> graph
  ] <?> "expected Graph -> Graph -> Graph"

graph4 :: H (Graph -> Graph -> Graph -> Graph)
graph4 = asum [
  reserved "lpf" >> return LPF,
  reserved "hpf" >> return HPF,
  reserved "bpf" >> return BPF,
  reserved "~~" >> return modulatedRangeGraph,
  reserved "+-" >> return (+-),
  reserved "linlin" >> return LinLin,
  reserved "iline" >> return ILine,
  reserved "line" >> return Line
  ] <?> "expected Graph -> Graph -> Graph -> Graph"

lGraph_graph_graph :: H ([Graph] -> Graph -> Graph)
lGraph_graph_graph = reserved "step" >> return Step

int_graph_graph :: H (Int -> Graph -> Graph)
int_graph_graph = asum [
  reserved "rep" >> return Rep,
  reserved "unrep" >> return UnRep
  ]

double_graph_graph_graph :: H (Double -> Graph -> Graph -> Graph)
double_graph_graph_graph = reserved "delay" >> return Delay

graph2_graph :: H ((Graph -> Graph) -> Graph)
graph2_graph = graph_graph2_graph <*> graph

graph_graph2_graph :: H (Graph -> (Graph -> Graph) -> Graph)
graph_graph2_graph = reserved "&" >> return (&)

identifiedGraph :: H Graph
identifiedGraph = do
  (_,i,g) <- binaryApplication (reserved "<<") Sound.Punctual.Parser.identifier  graph
  modify $ \s -> s { definitions1 = Map.insert i g $ definitions1 s }
  return g

identifier :: H Identifier
identifier = Language.Haskellish.identifier
-- TODO: reject identifiers that are reserved words

int :: H Int
int = (fromIntegral <$> integer) <?> "expected Int"

double :: H Double
double = (realToFrac <$> rationalOrInteger) <?> "expected Double"

img :: H Graph
img = do
  (_,url) <- functionApplication (reserved "img" >> return Img) (T.pack <$> string)
  let ref = ImgRef url
  modify' $ \s -> s { textureRefs = Set.insert ref $ textureRefs s }
  return (Img ref)

vid :: H Graph
vid = do
  (_,url) <- functionApplication (reserved "vid" >> return Vid) (T.pack <$> string)
  let ref = VidRef url
  modify' $ \s -> s { textureRefs = Set.insert ref $ textureRefs s }
  return (Img ref)

cam :: H Graph
cam = reserved "cam" >> modify (\s -> s { needsWebcam = True }) >> return Cam

-- deprecated
textureRef_graph_graph :: H (TextureRef -> Graph -> Graph)
textureRef_graph_graph = asum [
  reserved "tex" >> return Tex
  ]

-- deprecated
textureRef :: H TextureRef
textureRef = (do
  t <- T.pack <$> string
  let ref = ImgRef t
  modify' $ \s -> s { textureRefs = Set.insert ref $ textureRefs s }
  return ref
  ) <?> "expected texture URL"


multiSeries :: H Graph
multiSeries = multiSeries0 <|> multiSeries1 <|> multiSeriesDeprecated

multiSeries0 :: H Graph
multiSeries0 = do
  let i = fromIntegral <$> integer
  (a,b) <- Language.Haskellish.enumFromTo i i
  return $ Multi $ fmap Constant $ Data.List.take 64 [a .. b]

multiSeries1 :: H Graph
multiSeries1 = do
  (a,b,c) <- Language.Haskellish.enumFromThenTo double double double
  return $ Multi $ fmap Constant $ Data.List.take 64 $ multiSeries1' a b c
  
multiSeries1' :: Double -> Double -> Double -> [Double]
multiSeries1' a b c
  | (b == a) = [a]
  | (c == a) = [a]
  | otherwise = [a, b .. c]

-- will be removed in punctual 0.5
multiSeriesDeprecated :: H Graph
multiSeriesDeprecated = (reserved "..." >> return f) <*> i <*> i
  where
    f x y = Multi $ fmap Constant $ Data.List.take 64 [x .. y]
    i = fromIntegral <$> integer
