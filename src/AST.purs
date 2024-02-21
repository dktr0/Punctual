module AST where

import Prelude (class Eq,class Show,bind,pure,unit,discard,(<$>),($),map,($>),(<$),(<<<),(*),negate)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Parsing (Position,ParseError,runParser,position)
import Parsing.Combinators (chainl1,chainr1,(<|>),try,choice,lookAhead,many,many1,option)
import Parsing.String (eof)
import Data.Either (Either(..))
import Data.Foldable (foldl)

import TokenParser (P, commaSep, identifier, number, parens, reserved, reservedOp, semiSep, stringLiteral, whiteSpace, brackets, reservedNamesDef, operators1, operators2, operators3, comma, integer,naturalOrFloat)


type AST = List (Maybe Statement)

type Statement = {
  position :: Position,
  identifiers :: List String,
  expression :: Expression
  }

data Expression =
  Reserved Position String |
  Identifier Position String |
  LiteralInt Position Int |
  LiteralNumber Position Number |
  LiteralString Position String |
  ListExpression Position (List Expression) |
  Application Position Expression Expression |
  Operation Position String Expression Expression |
  FromTo Position Int Int |
  FromThenTo Position Number Number Number |
  Lambda Position (List String) Expression |
  IfThenElse Position Expression Expression Expression

derive instance Eq Expression
derive instance Generic Expression _
instance Show Expression where
  show x = genericShow x

parseAST :: String -> Either ParseError AST
parseAST x = runParser x ast

ast :: P AST
ast = do
  whiteSpace
  xs <- semiSep statement
  eof
  pure $ xs

statement :: P (Maybe Statement)
statement =
  (Just <$> try statementWithAssignment) <|>
  (Just <$> try statementNoAssignment) <|>
  emptyStatement

statementWithAssignment :: P Statement
statementWithAssignment = do
  p <- position
  xs <- many1 identifier
  reservedOp "=" <|> reservedOp "<<"
  e <- expression1
  pure { position: p, identifiers: (toList xs), expression: e }
  
statementNoAssignment :: P Statement
statementNoAssignment = do
  p <- position
  e <- expression1
  pure { position: p, identifiers: Nil, expression: e }
  
emptyStatement :: P (Maybe Statement)
emptyStatement = do
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure Nothing

operator :: Array String -> P (Expression -> Expression -> Expression)
operator xs = do
  p <- position
  choice $ map (\x -> reservedOp x $> Operation p x) xs

expression1 :: P Expression
expression1 = do
  _ <- pure unit
  chainl1 expression2 (operator operators1)

expression2 :: P Expression
expression2 = do
  _ <- pure unit
  chainr1 expression3 (operator operators2) -- note: right associativity for $ and (I think?) &

expression3 :: P Expression
expression3 = do
  _ <- pure unit
  chainl1 expression4 (operator operators3)

expression4 :: P Expression
expression4 = do
  _ <- pure unit
  choice [
    try application,
    argument
    ]

application :: P Expression
application = do
  _ <- pure unit
  p <- position
  f <- argument
  firstArg <- argument
  otherArgs <- many argument
  pure $ foldl (Application p) (Application p f firstArg) otherArgs

argument :: P Expression
argument = do
  _ <- pure unit
  p <- position
  choice [
    parens expression1,
    try reservedNames,
    try intOrNumber,
    try $ LiteralString p <$> stringLiteral,
    try fromTo,
    try fromThenTo,
    try list,
    try lambda,
    try ifThenElse,
    Identifier p <$> identifier
    ]

reservedNames :: P Expression
reservedNames = choice $ map (try <<< reservedName) reservedNamesDef

reservedName :: String -> P Expression
reservedName x = do
  p <- position
  Reserved p x <$ reserved x

fromTo :: P Expression
fromTo = do
  _ <- pure unit
  brackets $ do
    p <- position
    x <- integer
    reservedOp ".."
    y <- integer
    pure $ FromTo p x y

fromThenTo :: P Expression
fromThenTo = do
  _ <- pure unit
  brackets $ do
    p <- position
    x <- number
    _ <- comma
    y <- number
    reservedOp ".."
    z <- number
    pure $ FromThenTo p x y z

list :: P Expression
list = do
  _ <- pure unit
  brackets $ do
    p <- position
    xs <- commaSep expression1
    pure $ ListExpression p xs

intOrNumber :: P Expression
intOrNumber = do
  p <- position
  isPositive <- option true (false <$ reservedOp "-")
  x <- naturalOrFloat
  case x of
    Left i -> if isPositive then (pure $ LiteralInt p i) else (pure $ LiteralInt p (i*(-1)))
    Right f -> if isPositive then (pure $ LiteralNumber p f) else (pure $ LiteralNumber p (f*(-1.0)))

lambda :: P Expression
lambda = do
  p <- position
  reservedOp "\\"
  xs <- many1 identifier
  reservedOp "->"
  e <- expression1
  pure $ Lambda p (toList xs) e
  
ifThenElse :: P Expression
ifThenElse = do
  p <- position
  reserved "if"
  i <- argument
  reserved "then"
  t <- argument
  reserved "else"
  e <- argument
  pure $ IfThenElse p i t e

