module TokenParser where

import Prelude (Unit, discard, negate, ($), ($>), (*), (<$>), (<>))
import Data.Identity (Identity)
import Parsing (ParserT)
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),unGenLanguageDef,GenTokenParser,makeTokenParser)
import Parsing.Combinators (choice,try)
import Data.Either
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Int (toNumber)

type P a = ParserT String Identity a

number :: P Number
number = choice [
  try negativeFloat,
  try float,
  toNumber <$> integer
  ]

negativeFloat :: P Number
negativeFloat = do
  reservedOp "-"
  ((*) (-1.0)) <$> float

boolean :: P Boolean
boolean = choice [
  reserved "true" $> true,
  reserved "false" $> false
  ]

functionsWithNoArgumentsDef :: Array String
functionsWithNoArgumentsDef = [
  "pi","audioin","cps","time","beat","etime","ebeat","rnd",
  "fx","fy","fxy","px","py","pxy","frt","fr","ft","aspect",
  "lo","mid","hi","ilo","imid","ihi","fft","ifft","fb","cam"
  ]
  
functionsWithArgumentsDef :: Array String
functionsWithArgumentsDef = [
  "blend","add","mul",
  "abs","acos","acosh","asin","asinh","atan","atanh","cbrt","ceil","cos",
  "cosh","exp","fract","floor","log","log2","log10","round","sign","sin","sinh",
  "sqrt","tan","tanh","trunc",
  "rtxy","rtx","rty","xyrt","xyr","xyt","zero","zer0","bipolar","unipolar",
  "osc","tri","saw","sqr","lftri","lfsaw","lfsqr",
  "mono","cpsmidi","midicps","dbamp","ampdb",
  "hsvrgb","hsvh","hsvs","hsvv","hsvr","hsvg","hsvb",
  "rgbhsv","rgbh","rgbs","rgbv","rgbr","rgbg","rgbb",
  "dist","prox",
  "point","hline","hlinep","vline","vlinep","circle","circlep","rect","rectp","iline","ilinep","line","linep",
  "chain","chainp","lines","linesp","ilines","ilinesp","mesh","meshp",
  "zip",
  "fit","min","max","minp","maxp",
  "clip","clipp","between","betweenp","smoothstep","smoothstepp","gate","gatep","when","seq",
  "setfx","setfy","setfxy","zoom","zoomxy","zoomx","zoomy","move","tile","tilexy","tilex","tiley","spin",
  "early","late","slow","fast",
  "lpf","lpfp","hpf","hpfp","bpf","bpfp","delay",
  "linlin","linlinp",
  "rep",
  "img","vid",
  "mix","mixp",
  "import"
  ]

otherReservedNamesDef :: Array String
otherReservedNamesDef = [
  "if","then","else",
  "audio","blend","rgba","add","mul","rgb"
  ]

reservedNamesDef :: Array String
reservedNamesDef = functionsWithNoArgumentsDef <> functionsWithArgumentsDef <> otherReservedNamesDef
  
-- operators0 are used at the top-level of parsing (statements) but not in recursive expressions
operators0 :: Array String
operators0 = ["<<","="]

operators1 :: Array String
operators1 = [">>","<>","->","\\"]

operators2 :: Array String
operators2 = ["$","&"]

operators3 :: Array String
operators3 = ["++","~~","+-"]

operators4 :: Array String
operators4 = ["==","/=",">","<",">=","<=","==:","/=:",">:","<:",">=:","<=:"]
  
operators5 :: Array String
operators5 = ["+","-","+:","-:"]
  
operators6 :: Array String
operators6 = ["*","/","%","*:","/:","%:"]

operators7 :: Array String
operators7 = ["**","**:"]


tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = reservedNamesDef,
  reservedOpNames = operators0 <> operators1 <> operators2 <> operators3 <> operators4 <> operators5 <> operators6 <> operators7,
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = true
  }

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace
