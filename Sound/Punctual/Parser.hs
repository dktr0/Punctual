{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (runPunctualParser) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (asum)
import Language.Haskell.Exts
import Language.Haskellish
import Data.Time
import TextShow
import Data.IntMap.Strict as IntMap
import Data.Map as Map
import Data.List.Split (linesBy)
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Sound.Punctual.AudioTime
import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Duration
import Sound.Punctual.DefTime
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((>>),(<>),graph,output,defTime,outputs)
import qualified Sound.Punctual.Action as P
import Sound.Punctual.Program

data ParserState = ParserState {
  textureRefs :: Map String Int,
  localBindings :: Map String Int, -- eg. f x y = fromList [("x",0),("y",1)]
  definitions1 :: Map String Graph,
  definitions2 :: Map String (Graph -> Graph),
  definitions3 :: Map String (Graph -> Graph -> Graph)
}

emptyParserState :: ParserState
emptyParserState = ParserState {
  textureRefs = Map.empty,
  localBindings = Map.empty,
  definitions1 = Map.empty,
  definitions2 = Map.empty,
  definitions3 = Map.empty
  }

type H = Haskellish ParserState

parseHaskellish :: H a -> ParserState -> String -> Either String (a,ParserState)
parseHaskellish p st x = (parseResultToEither $ parseWithMode haskellSrcExtsParseMode x) >>= runHaskellish p st

parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk x) = Right x
parseResultToEither (ParseFailed l s) = Left s

parseProgram :: AudioTime -> [String] -> Either String Program
parseProgram eTime xs = do
  let initialProgram = emptyProgram { evalTime = eTime }
  (p,st) <- foldM parseStatement (initialProgram,emptyParserState) $ zip [0..] xs
  return $ p { textureMap = Map.mapKeys T.pack $ textureRefs st }

parseStatement :: (Program,ParserState) -> (Int,String) -> Either String (Program,ParserState)
parseStatement x y = parseStatementAsDefinition x y <|> parseStatementAsAction x y

parseStatementAsDefinition :: (Program,ParserState) -> (Int,String) -> Either String (Program,ParserState)
parseStatementAsDefinition (p,st) (_,x) = do
  st' <- parseDefinition st x
  return (p,st')

parseStatementAsAction :: (Program,ParserState) -> (Int,String) -> Either String (Program,ParserState)
parseStatementAsAction (p,st) (lineNumber,x) = do
  (a,st') <- parseAction st x
  return (p { actions = IntMap.insert lineNumber a (actions p) },st')

parseAction :: ParserState -> String -> Either String (Action,ParserState)
parseAction = parseHaskellish action

parseDefinition :: ParserState -> String -> Either String ParserState
parseDefinition st x = (parseResultToEither $ parseWithMode haskellSrcExtsParseMode x) >>= simpleDefinition st
  
simpleDefinition :: ParserState -> Decl SrcSpanInfo -> Either String ParserState
simpleDefinition st (PatBind _ (PVar _ (Ident _ x)) (UnGuardedRhs _ e) _) = do
  (e',st') <- runHaskellish graph st e
  return $ st' { definitions1 = Map.insert x e' (definitions1 st') }
simpleDefinition st _ = Left ""

runPunctualParser :: AudioTime -> Text -> IO (Either String Program)
runPunctualParser eTime x = do
  (x',pragmas) <- return $! extractPragmas x
  r <- if (elem "glsl" pragmas) then do
    return $ Right $ emptyProgram { directGLSL = Just x' }
  else do
    a <- return $! Prelude.filter notEmptyLine $ linesBy (==';') $ T.unpack x' -- cast to String and separate on ;
    p <- return $! parseProgram eTime a
    return p
  return r

extractPragmas :: Text -> (Text,[Text])
extractPragmas t = (newText,pragmas)
  where
    f "#glsl" = (T.empty,["glsl"])
    f x = (x,[])
    xs = fmap (f . T.stripEnd) $ T.lines t
    newText = T.unlines $ fmap fst xs
    pragmas = concat $ fmap snd xs

notEmptyLine :: String -> Bool
notEmptyLine = (/="") . Prelude.filter (\y -> y /= '\n' && y /=' ' && y /= '\t')

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
        Fixity (AssocLeft ()) 0 (UnQual () (Symbol () "@@"))
        ]
    }

action :: H Action
action = asum [
  duration_action <*> duration,
  defTime_action <*> defTime,
  outputs_action <*> outputs,
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

int :: H Int
int = fromIntegral <$> integer

double :: H Double
double = asum [
  realToFrac <$> rationalOrInteger,
  reverseApplication double (reserved "m" >> return midicps),
  reverseApplication double (reserved "db" >> return dbamp)
  ]

duration :: H Duration
duration = asum [
  Seconds <$> double,
  reverseApplication double (reserved "s" >> return Seconds),
  reverseApplication double (reserved "ms" >> return (\x -> Seconds $ x/1000.0)),
  reverseApplication double (reserved "c" >> return Cycles)
  ]

defTime :: H DefTime
defTime = asum [
  (\(x,y) -> Quant x y) <$> Language.Haskellish.tuple double duration,
  After <$> duration
  ]

outputs :: H [Output]
outputs = asum [
  concat <$> list outputs,
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
  x <- identifier
  m <- gets definitions1 -- Map Text Graph
  let xm = Map.lookup x m
  if isJust xm then return (fromJust xm) else throwError ""


graph :: H Graph
graph = asum [
  definitions1H,
  reverseApplication graph (reserved "m" >> return MidiCps),
  reverseApplication graph (reserved "db" >> return DbAmp),
  (Constant . realToFrac) <$> rational,
  (Constant . fromIntegral) <$> integer,
  Multi <$> list graph,
  multiSeries,
  reserved "noise" >> return Noise,
  reserved "pink" >> return Pink,
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy,
  reserved "px" >> return Px,
  reserved "py" >> return Py,
  reserved "lo" >> return Lo,
  reserved "mid" >> return Mid,
  reserved "hi" >> return Hi,
  reserved "ilo" >> return ILo,
  reserved "imid" >> return IMid,
  reserved "ihi" >> return IHi,
  graph2 <*> graph
  ]

graph2 :: H (Graph -> Graph)
graph2 = asum [
  reserved "bipolar" >> return Bipolar,
  reserved "unipolar" >> return Unipolar,
  reserved "sin" >> return Sine,
  reserved "tri" >> return Tri,
  reserved "saw" >> return Saw,
  reserved "sqr" >> return Square,
  reserved "mono" >> return Mono,
  reserved "abs" >> return Abs,
  reserved "cpsmidi" >> return CpsMidi,
  reserved "midicps" >> return MidiCps,
  reserved "dbamp" >> return DbAmp,
  reserved "ampdb" >> return AmpDb,
  reserved "sqrt" >> return Sqrt,
  reserved "floor" >> return Floor,
  reserved "fract" >> return Fract,
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
  reserved "mean" >> return Mean,
  reserved "min" >> return Min,
  reserved "max" >> return Max,
  reserved "distance" >> return Distance,
  reserved "point" >> return Point,
  reserved "hline" >> return HLine,
  reserved "vline" >> return VLine,
  reserved "fb" >> return fb,
  graph4 <*> graph,
  textureRef_graph_graph_graph <*> textureRef
  ]

graph4 :: H (Graph -> Graph -> Graph -> Graph)
graph4 = asum [
  reserved "lpf" >> return LPF,
  reserved "hpf" >> return HPF,
  reserved "circle" >> return Circle,
  reserved "clip" >> return Clip,
  reserved "between" >> return Between,
  reserved "~~" >> return modulatedRangeGraph,
  reserved "+-" >> return (+-),
  graph5 <*> graph
  ]

graph5 :: H (Graph -> Graph -> Graph -> Graph -> Graph)
graph5 = asum [
  reserved "rect" >> return Rect,
  graph6 <*> graph
  ]

graph6 :: H (Graph -> Graph -> Graph -> Graph -> Graph -> Graph)
graph6 = asum [
  reserved "linlin" >> return LinLin,
  reserved "iline" >> return ILine,
  reserved "line" >> return Line
  ]

textureRef_graph_graph_graph :: H (Int -> Graph -> Graph -> Graph)
textureRef_graph_graph_graph = asum [
  reserved "tex" >> return tex,
  reserved "texr" >> return TexR,
  reserved "texg" >> return TexG,
  reserved "texb" >> return TexB
  ]

textureRef :: H Int
textureRef = do
  x <- string
  tRefs <- gets textureRefs
  let x' = Map.lookup x tRefs
  case x' of
    Just i -> return i
    Nothing -> do
      let i = Map.size tRefs + 1
      modify $ \s -> s { textureRefs = Map.insert x i tRefs }
      return i

multiSeries :: H Graph
multiSeries = (reserved "..." >> return f) <*> i <*> i
  where
    f x y = Multi $ fmap Constant [x .. y]
    i = fromIntegral <$> integer
