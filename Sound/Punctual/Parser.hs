{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (runPunctualParser) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (asum)
import Language.Haskell.Exts
import Language.Haskellish
import Sound.MusicW as W (midicps,dbamp)

import Sound.Punctual.Graph as P
import qualified Sound.Punctual.Types as P

runPunctualParser :: Text -> Either String [P.Expression]
runPunctualParser = f . parseWithMode m . ('[':) . (++ "]") . T.unpack . (T.replace ";" ",")
  where
    f (ParseOk x) = runHaskellish punctualParser x
    f (ParseFailed l s) = Left s
    m = defaultParseMode {
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

punctualParser :: Haskellish [P.Expression]
punctualParser = list expression

expression :: Haskellish P.Expression
expression = asum [
  duration_expression <*> duration,
  defTime_expression <*> defTime,
  output_expression <*> output,
  P.expressionFromGraph <$> graph
  ]

duration_expression :: Haskellish (P.Duration -> P.Expression)
duration_expression = expression_duration_expression <*> expression

defTime_expression :: Haskellish (P.DefTime -> P.Expression)
defTime_expression = expression_defTime_expression <*> expression

output_expression :: Haskellish (P.Output -> P.Expression)
output_expression = expression_output_expression <*> expression

expression_duration_expression :: Haskellish (P.Expression -> P.Duration -> P.Expression)
expression_duration_expression = reserved "<>" >> return (P.<>)

expression_defTime_expression :: Haskellish (P.Expression -> P.DefTime -> P.Expression)
expression_defTime_expression = reserved "@@" >> return (P.@@)

expression_output_expression :: Haskellish (P.Expression -> P.Output -> P.Expression)
expression_output_expression = reserved ">>" >> return (P.>>)

double :: Haskellish Double
double = asum [
  realToFrac <$> rationalOrInteger,
  reverseApplication double (reserved "m" >> return W.midicps),
  reverseApplication double (reserved "db" >> return W.dbamp)
  ]

duration :: Haskellish P.Duration
duration = asum [
  P.Seconds <$> double,
  reverseApplication double (reserved "s" >> return P.Seconds),
  reverseApplication double (reserved "ms" >> return (\x -> P.Seconds $ x/1000.0)),
  reverseApplication double (reserved "c" >> return P.Cycles)
  ]

defTime :: Haskellish P.DefTime
defTime = asum [
  (\(x,y) -> P.Quant x y) <$> Language.Haskellish.tuple double duration,
  P.After <$> duration
  ]

output :: Haskellish P.Output
output = asum [
  (P.PannedOutput . realToFrac) <$> rationalOrInteger,
  reserved "left" >> return (P.PannedOutput 0),
  reserved "right" >> return (P.PannedOutput 1),
  reserved "centre" >> return (P.PannedOutput 0.5),
  reserved "splay" >> return (P.NamedOutput "splay"),
  reserved "red" >> return (P.NamedOutput "red"),
  reserved "green" >> return (P.NamedOutput "green"),
  reserved "blue" >> return (P.NamedOutput "blue"),
  reserved "alpha" >> return (P.NamedOutput "alpha"),
  reserved "rgb" >> return (P.NamedOutput "rgb"),
  reserved "hsv" >> return (P.NamedOutput "hsv")
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
  reserved "fb" >> return fb,
  graph2 <*> graph
  ]

graph2 :: Haskellish (Graph -> Graph)
graph2 = asum [
  reserved "bipolar" >> return bipolar,
  reserved "unipolar" >> return unipolar,
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
  reserved "squared" >> return squared,
  reserved "sqrt" >> return Sqrt,
  reserved "hline" >> return hline,
  reserved "vline" >> return vline,
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
  reserved "mean" >> return mean,
  reserved "distance" >> return distance,
  reserved "point" >> return point,
  graph4 <*> graph
  ]

graph4 :: Haskellish (Graph -> Graph -> Graph -> Graph)
graph4 = asum [
  reserved "lpf" >> return LPF,
  reserved "hpf" >> return HPF,
  reserved "circle" >> return circle,
  reserved "texr" >> return TexR,
  reserved "texg" >> return TexG,
  reserved "texb" >> return TexB,
  reserved "tex" >> return tex,
  reserved "clip" >> return Clip,
  reserved "~~" >> return modulatedRangeGraph,
  reserved "+-" >> return (P.+-),
  graph5 <*> graph
  ]

graph5 :: Haskellish (Graph -> Graph -> Graph -> Graph -> Graph)
graph5 = asum [
  reserved "rect" >> return rect,
  graph6 <*> graph
  ]

graph6 :: Haskellish (Graph -> Graph -> Graph -> Graph -> Graph -> Graph)
graph6 = asum [
  reserved "linlin" >> return linlin
  ]

multiSeries :: Haskellish Graph
multiSeries = (reserved "..." >> return f) <*> i <*> i
  where
    f x y = Multi $ fmap Constant [x .. y]
    i = fromIntegral <$> integer
