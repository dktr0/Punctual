{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (Sound.Punctual.Parser.parse) where

import Data.Text (Text)
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

import Sound.Punctual.AudioTime
import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Duration
import Sound.Punctual.DefTime
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((>>),(<>),graph,defTime)
import qualified Sound.Punctual.Action as P
import Sound.Punctual.Program


parse :: AudioTime -> Text -> Either String Program
parse eTime x = do
  let (x',pragmas) = extractPragmas x
  if (elem "glsl" pragmas) then do
    return $ emptyProgram { directGLSL = Just x' }
  else parseProgram eTime $ T.unpack x'

extractPragmas :: Text -> (Text,[Text])
extractPragmas t = (newText,pragmas)
  where
    f "#glsl" = (T.empty,["glsl"])
    f x = (x,[])
    xs = fmap (f . T.stripEnd) $ T.lines t
    newText = T.unlines $ fmap fst xs
    pragmas = concat $ fmap snd xs

parseProgram :: AudioTime -> String -> Either String Program
parseProgram eTime x = do
  let x' = intercalate "," $ fmap (++ " _0") $ splitOn ";" x
  let x'' = "[" ++ x' ++ "\n]"
  (p,st) <- parseHaskellish program emptyParserState x''
  return $ p {
    textureSet = textureRefs st,
    programNeedsAudioInputAnalysis = audioInputAnalysis st,
    programNeedsAudioOutputAnalysis = audioOutputAnalysis st,
    evalTime = eTime
  }


type Identifier = String

data ParserState = ParserState {
  actionCount :: Int,
  textureRefs :: Set Text,
  localBindings :: Map Identifier Int, -- eg. f x y = fromList [("x",0),("y",1)]
  definitions1 :: Map Identifier Graph,
--  definitions2 :: Map Identifier (Graph -> Graph),
--  definitions3 :: Map Identifier (Graph -> Graph -> Graph),
  audioInputAnalysis :: Bool,
  audioOutputAnalysis :: Bool
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
  audioOutputAnalysis = False
  }

type H = Haskellish ParserState

parseHaskellish :: H a -> ParserState -> String -> Either String (a,ParserState)
parseHaskellish p st x = (parseResultToEither $ parseWithMode haskellSrcExtsParseMode x) >>= runHaskellish p st

parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk x) = Right x
parseResultToEither (ParseFailed _ s) = Left s

haskellSrcExtsParseMode :: ParseMode
haskellSrcExtsParseMode = defaultParseMode {
      fixities = Just [
        Fixity (AssocRight ()) 9 (UnQual () (Symbol () ".")),
        Fixity (AssocLeft ()) 9 (UnQual () (Symbol () "!!")),
        Fixity (AssocRight ()) 8 (UnQual () (Symbol () "^")),
        Fixity (AssocRight ()) 8 (UnQual () (Symbol () "^^")),
        Fixity (AssocRight ()) 8 (UnQual () (Symbol () "**")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "*")),
        Fixity (AssocLeft ()) 7 (UnQual () (Symbol () "/")),
        Fixity (AssocLeft ()) 7 (UnQual () (Ident () "quot")),
        Fixity (AssocLeft ()) 7 (UnQual () (Ident () "rem")),
        Fixity (AssocLeft ()) 7 (UnQual () (Ident () "div")),
        Fixity (AssocLeft ()) 7 (UnQual () (Ident () "mod")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "+")),
        Fixity (AssocLeft ()) 6 (UnQual () (Symbol () "-")),
        Fixity (AssocRight ()) 5 (UnQual () (Symbol () ":")),
        Fixity (AssocRight ()) 5 (UnQual () (Symbol () "++")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "==")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "/=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () "<=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">=")),
        Fixity (AssocNone ()) 4 (UnQual () (Symbol () ">")),
        Fixity (AssocNone ()) 4 (UnQual () (Ident () "elem")),
        Fixity (AssocNone ()) 4 (UnQual () (Ident () "notElem")),
        Fixity (AssocLeft ()) 4 (UnQual () (Symbol () "<$>")),
        Fixity (AssocLeft ()) 4 (UnQual () (Symbol () "<$")),
        Fixity (AssocLeft ()) 4 (UnQual () (Symbol () "<*>")),
        Fixity (AssocLeft ()) 4 (UnQual () (Symbol () "<*")),
        Fixity (AssocLeft ()) 4 (UnQual () (Symbol () "*>")),
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

_0Arg :: H a -> H a
_0Arg p = p <|> fmap fst (functionApplication p $ reserved "_0")

program :: H Program
program = do
  xs <- concat <$> list actionOr_0
  let xs' = Data.List.filter (not . Data.List.null . Sound.Punctual.Action.outputs) xs
  return $ emptyProgram { actions = IntMap.fromList $ zip [0..] xs' }

actionOr_0 :: H [Action]
actionOr_0 = _0Arg $ asum [
  reserved "_0" >> return [],
  fmap (:[]) action
  ]

action :: H Action
action = asum [
  duration_action <*> duration,
  defTime_action <*> defTime,
  outputs_action <*> Sound.Punctual.Parser.outputs,
  actionFromGraph <$> graph
  ]

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

double :: H Double
double = asum [
  realToFrac <$> rationalOrInteger,
  reverseApplication double (reserved "m" >> return midicps),
  reverseApplication double (reserved "db" >> return dbamp)
  ]

duration :: H Duration
duration = _0Arg $ asum [
  Seconds <$> double,
  reverseApplication double (reserved "s" >> return Seconds),
  reverseApplication double (reserved "ms" >> return (\x -> Seconds $ x/1000.0)),
  reverseApplication double (reserved "c" >> return Cycles)
  ]

defTime :: H DefTime
defTime = _0Arg $ asum [
  (\(x,y) -> Quant x y) <$> Language.Haskellish.tuple double duration,
  After <$> duration
  ]

outputs :: H [Output]
outputs = _0Arg $ asum [
  concat <$> list Sound.Punctual.Parser.outputs,
  ((:[]) . Panned . realToFrac) <$> rationalOrInteger,
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
  if isJust xm then return (fromJust xm) else throwError ""

graph :: H Graph
graph = _0Arg $ asum [
  identifiedGraph,
  definitions1H,
  reverseApplication graph (reserved "m" >> return MidiCps),
  reverseApplication graph (reserved "db" >> return DbAmp),
  (Constant . realToFrac) <$> rational,
  (Constant . fromIntegral) <$> integer,
  Multi <$> list graph,
  multiSeries,
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy,
  reserved "fxy" >> return Fxy,
  reserved "px" >> return Px,
  reserved "py" >> return Py,
  reserved "lo" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Lo,
  reserved "mid" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Mid,
  reserved "hi" >> modify (\s -> s { audioOutputAnalysis = True } ) >> return Hi,
  reserved "ilo" >> modify (\s -> s { audioInputAnalysis = True } ) >> return ILo,
  reserved "imid" >> modify (\s -> s { audioInputAnalysis = True } ) >> return IMid,
  reserved "ihi" >> modify (\s -> s { audioInputAnalysis = True } ) >> return IHi,
  graph2 <*> graph,
  ifThenElseParser
  ]

ifThenElseParser :: H Graph
ifThenElseParser = do
  (a,b,c) <- ifThenElse graph graph graph
  return $ IfThenElse a b c

graph2 :: H (Graph -> Graph)
graph2 = asum [
  reserved "bipolar" >> return Bipolar,
  reserved "unipolar" >> return Unipolar,
  reserved "sin" >> return Sin,
  reserved "tri" >> return Tri,
  reserved "saw" >> return Saw,
  reserved "sqr" >> return Sqr,
  reserved "mono" >> return Mono,
  reserved "abs" >> return Abs,
  reserved "cpsmidi" >> return CpsMidi,
  reserved "midicps" >> return MidiCps,
  reserved "dbamp" >> return DbAmp,
  reserved "ampdb" >> return AmpDb,
  reserved "sqrt" >> return Sqrt,
  reserved "floor" >> return Floor,
  reserved "fract" >> return Fract,
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
  reserved "distance" >> return Distance,
  reserved "point" >> return Point,
  reserved "fb" >> return Fb,
  textureRef_graph_graph <*> textureRef,
  int_graph_graph <*> int,
  lDouble_graph_graph <*> list double,
  graph3 <*> graph
  ]

graph3 :: H (Graph -> Graph -> Graph)
graph3 = asum [
  reserved "+" >> return (+),
  reserved "-" >> return (-),
  reserved ">" >> return GreaterThan,
  reserved "<" >> return LessThan,
  reserved ">=" >> return GreaterThanOrEqual,
  reserved "<=" >> return LessThanOrEqual,
  reserved "==" >> return Equal,
  reserved "!=" >> return NotEqual,
  reserved "**" >> return Pow,
  reserved "*" >> return Product,
  reserved "/" >> return Division,
  reserved "min" >> return Min,
  reserved "max" >> return Max,
  reserved "hline" >> return HLine,
  reserved "vline" >> return VLine,
  reserved "circle" >> return Circle,
  reserved "rect" >> return Rect,
  reserved "clip" >> return Clip,
  reserved "between" >> return Between,
  reserved "when" >> return Sound.Punctual.Graph.when,
  reserved "gate" >> return Gate,
  graph4 <*> graph
  ]

graph4 :: H (Graph -> Graph -> Graph -> Graph)
graph4 = asum [
  reserved "lpf" >> return LPF,
  reserved "hpf" >> return HPF,
  reserved "~~" >> return modulatedRangeGraph,
  reserved "+-" >> return (+-),
  reserved "linlin" >> return LinLin,
  reserved "iline" >> return ILine,
  reserved "line" >> return Line
  ]

lDouble_graph_graph :: H ([Double] -> Graph -> Graph)
lDouble_graph_graph = reserved "step" >> return Step

int_graph_graph :: H (Int -> Graph -> Graph)
int_graph_graph = asum [
  reserved "rep" >> return Rep,
  reserved "unrep" >> return UnRep
  ]

identifiedGraph :: H Graph
identifiedGraph = do
  (_,i,g) <- binaryApplication (reserved "<<") Sound.Punctual.Parser.identifier  graph
  modify $ \s -> s { definitions1 = Map.insert i g $ definitions1 s }
  return g

identifier :: H Identifier
identifier = Language.Haskellish.identifier
-- TODO: reject identifiers that are reserved words

int :: H Int
int = fromIntegral <$> integer

textureRef_graph_graph :: H (Text -> Graph -> Graph)
textureRef_graph_graph = asum [
  reserved "tex" >> return Tex,
  reserved "texhsv" >> return texhsv
  ]

textureRef :: H Text
textureRef = do
  t <- T.pack <$> string
  modify' $ \s -> s { textureRefs = Set.insert t $ textureRefs s }
  return t

multiSeries :: H Graph
multiSeries = (reserved "..." >> return f) <*> i <*> i
  where
    f x y = Multi $ fmap Constant [x .. y]
    i = fromIntegral <$> integer
