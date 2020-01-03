{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (runPunctualParser,runPunctualParserTimed) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (asum)
import Language.Haskell.Exts
import Language.Haskellish
import Data.Time
import TextShow
import Data.IntMap.Strict

import Sound.Punctual.AudioTime
import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Duration
import Sound.Punctual.DefTime
import Sound.Punctual.Output
import Sound.Punctual.Action hiding ((>>),(<>),graph,output,defTime,outputs)
import qualified Sound.Punctual.Action as P
import Sound.Punctual.Program

runPunctualParserTimed :: AudioTime -> Text -> IO (Either String Program)
runPunctualParserTimed eTime x = do
  t0 <- getCurrentTime
  (x',pragmas) <- return $! extractPragmas x
  t1 <- getCurrentTime
  r <- if (elem "glsl" pragmas) then do
    T.putStrLn $ "parse pragmas: " <> " " <> showt (round (diffUTCTime t1 t0 * 1000) :: Int) <> " ms"
    return $ Right $ emptyProgram { directGLSL = Just x' }
  else do
    a <- return $! preprocess x'
    t2 <- getCurrentTime
    b <- return $! parseWithMode haskellSrcExtsParseMode a
    t3 <- getCurrentTime
    c <- return $! f b
    t4 <- getCurrentTime
    T.putStrLn $ "parse pragmas: " <> " " <> showt (round (diffUTCTime t1 t0 * 1000) :: Int) <> " ms"
    T.putStrLn $ "parse preprocess: " <> " " <> showt (round (diffUTCTime t2 t1 * 1000) :: Int) <> " ms"
    T.putStrLn $ "parse parseWithMode: " <> " " <> showt (round (diffUTCTime t3 t2 * 1000) :: Int) <> " ms"
    T.putStrLn $ "parse runHaskellish: " <> " " <> showt (round (diffUTCTime t4 t3 * 1000) :: Int) <> " ms"
    return c
  tEnd <- getCurrentTime
  T.putStrLn $ "parse (total): " <> " " <> showt (round (diffUTCTime tEnd t0 * 1000) :: Int) <> " ms"
  return r
  where
    f (ParseOk x) = runHaskellish (program eTime) x
    f (ParseFailed l s) = Left s

extractPragmas :: Text -> (Text,[Text])
extractPragmas t = (newText,pragmas)
  where
    f "#glsl" = (T.empty,["glsl"])
    f x = (x,[])
    xs = fmap (f . T.stripEnd) $ T.lines t
    newText = T.unlines $ fmap fst xs
    pragmas = concat $ fmap snd xs

preprocess :: Text -> String
preprocess = ('[':) . (++ "]") . T.unpack . (T.replace ";" ",")

-- TODO: rework this to include pragmas as well as per runPunctualParserTimed above
runPunctualParser :: AudioTime -> Text -> Either String Program
runPunctualParser eTime = f . parseWithMode haskellSrcExtsParseMode . preprocess
  where
    f (ParseOk x) = runHaskellish (program eTime) x
    f (ParseFailed l s) = Left s

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

program :: AudioTime -> Haskellish Program
program t = do
  xs <- list action
  return $ Program {
    directGLSL = Nothing,
    actions = fromList $ zip [0..] xs,
    evalTime = t
  }

action :: Haskellish Action
action = asum [
  duration_action <*> duration,
  defTime_action <*> defTime,
  outputs_action <*> outputs,
  actionFromGraph <$> graph
  ]

duration_action :: Haskellish (Duration -> Action)
duration_action = action_duration_action <*> action

defTime_action :: Haskellish (DefTime -> Action)
defTime_action = action_defTime_action <*> action

outputs_action :: Haskellish ([Output] -> Action)
outputs_action = action_outputs_action <*> action

action_duration_action :: Haskellish (Action-> Duration -> Action)
action_duration_action = reserved "<>" >> return (P.<>)

action_defTime_action :: Haskellish (Action -> DefTime -> Action)
action_defTime_action = reserved "@@" >> return (@@)

action_outputs_action :: Haskellish (Action -> [Output] -> Action)
action_outputs_action = reserved ">>" >> return (P.>>)

double :: Haskellish Double
double = asum [
  realToFrac <$> rationalOrInteger,
  reverseApplication double (reserved "m" >> return midicps),
  reverseApplication double (reserved "db" >> return dbamp)
  ]

duration :: Haskellish Duration
duration = asum [
  Seconds <$> double,
  reverseApplication double (reserved "s" >> return Seconds),
  reverseApplication double (reserved "ms" >> return (\x -> Seconds $ x/1000.0)),
  reverseApplication double (reserved "c" >> return Cycles)
  ]

defTime :: Haskellish DefTime
defTime = asum [
  (\(x,y) -> Quant x y) <$> Language.Haskellish.tuple double duration,
  After <$> duration
  ]

outputs :: Haskellish [Output]
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
  reserved "alpha" >> return [Alpha],
  reserved "rgb" >> return [RGB],
  reserved "hsv" >> return [HSV]
  ]

graph :: Haskellish Graph
graph = asum [
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
  graph2 <*> graph
  ]

graph2 :: Haskellish (Graph -> Graph)
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

graph3 :: Haskellish (Graph -> Graph -> Graph)
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
  graph4 <*> graph
  ]

graph4 :: Haskellish (Graph -> Graph -> Graph -> Graph)
graph4 = asum [
  reserved "lpf" >> return LPF,
  reserved "hpf" >> return HPF,
  reserved "circle" >> return Circle,
  reserved "texr" >> return TexR,
  reserved "texg" >> return TexG,
  reserved "texb" >> return TexB,
  reserved "tex" >> return tex,
  reserved "clip" >> return Clip,
  reserved "between" >> return Between,
  reserved "~~" >> return modulatedRangeGraph,
  reserved "+-" >> return (+-),
  graph5 <*> graph
  ]

graph5 :: Haskellish (Graph -> Graph -> Graph -> Graph -> Graph)
graph5 = asum [
  reserved "rect" >> return Rect,
  graph6 <*> graph
  ]

graph6 :: Haskellish (Graph -> Graph -> Graph -> Graph -> Graph -> Graph)
graph6 = asum [
  reserved "linlin" >> return LinLin,
  reserved "iline" >> return ILine,
  reserved "line" >> return Line
  ]

multiSeries :: Haskellish Graph
multiSeries = (reserved "..." >> return f) <*> i <*> i
  where
    f x y = Multi $ fmap Constant [x .. y]
    i = fromIntegral <$> integer
