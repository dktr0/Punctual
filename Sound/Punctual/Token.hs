{-# LANGUAGE OverloadedStrings #-}
module Sound.Punctual.Token where

import Data.Text
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as P
import Control.Monad.Identity (Identity)

tokenParser :: P.GenTokenParser Text () Identity
tokenParser = P.makeTokenParser $ P.LanguageDef {
  P.commentStart = "{-",
  P.commentEnd = "-}",
  P.commentLine = "--",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> char '_',
  P.opStart = oneOf "+*:@<>~=%",
  P.opLetter = oneOf "+*:@<>~=%",
  P.reservedNames = ["c","s","ms","db","sin","tri","saw","sqr","noise","pink","fx","fy",
  "lpf","hpf","mix","x","y","red","green","blue","alpha","clear","width","height",
  "left","right","centre","bipolar","unipolar","linlin","rect","px","py","m","abs","splay",
  "point","hline","vline","rgb","mean","squared","sqrt","circle","distance"],
  P.reservedOpNames = ["+","*","/",":","@","<>","~","=","%",";","+-","..","=>","==","!=","<",">","<=",">="],
  P.caseSensitive = False
  }

identifier = P.identifier tokenParser
reserved = P.reserved tokenParser
operator = P.operator tokenParser
reservedOp = P.reservedOp tokenParser
charLiteral = P.charLiteral tokenParser
stringLiteral = P.stringLiteral tokenParser
natural = P.natural tokenParser
integer = P.integer tokenParser
float = P.float tokenParser
naturalOrFloat = P.naturalOrFloat tokenParser
decimal = P.decimal tokenParser
hexadecimal = P.hexadecimal tokenParser
octal = P.octal tokenParser
symbol = P.symbol tokenParser
lexeme = P.lexeme tokenParser
whiteSpace = P.whiteSpace tokenParser
parens = P.parens tokenParser
braces = P.braces tokenParser
angles = P.angles tokenParser
brackets = P.brackets tokenParser
semi = P.semi tokenParser
comma = P.comma tokenParser
colon = P.colon tokenParser
dot = P.dot tokenParser
semiSep = P.semiSep tokenParser
semiSep1 = P.semiSep1 tokenParser
commaSep = P.commaSep tokenParser
commaSep1 = P.commaSep1 tokenParser

double :: Parser Double
double = choice [
  try $ parens double,
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]
