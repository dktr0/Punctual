module AST where

import Prelude (class Eq,class Show,(==),(<>),(&&),show,bind,pure,unit,discard,(<$>),($),map,($>),(<$),(<<<))
import Data.List (List(..),(:))
import Parsing (Position(..),ParseError,runParser,position)
import Parsing.Combinators (chainl1,(<|>),try,choice,lookAhead,many)
import Parsing.String (eof)
import Data.Either (Either)
import Data.Foldable (foldl)

import TokenParser (P, commaSep, identifier, number, parens, reserved, reservedOp, semiSep, stringLiteral, whiteSpace, brackets, reservedNamesDef, operators1, operators2, operators3, comma, integer)

type AST = List Statement

data Statement = Expression Expression | EmptyStatement Position

instance Eq Statement where
  eq (Expression e1) (Expression e2) = e1 == e2
  eq (EmptyStatement p1) (EmptyStatement p2) = p1 == p2
  eq _ _ = false

instance Show Statement where
  show (Expression e) = "Expression (" <> show e <> ")"
  show (EmptyStatement p) = "EmptyStatement (" <> show p <> ")"
  
data Expression = 
  Reserved Position String |
  Identifier Position String |
  LiteralNumber Position Number |
  LiteralString Position String |
  ListExpression Position (List Expression) |
  Application Position Expression Expression |
  Operation Position String Expression Expression |
  FromTo Position Int Int |
  FromThenTo Position Number Number Number
  
instance Eq Expression where
  eq (Reserved p1 x1) (Reserved p2 x2) = p1 == p2 && x1 == x2
  eq (Identifier p1 x1) (Identifier p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralNumber p1 x1) (LiteralNumber p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralString p1 x1) (LiteralString p2 x2) = p1 == p2 && x1 == x2
  eq (ListExpression p1 x1) (ListExpression p2 x2) = p1 == p2 && x1 == x2
  eq (Application p1 x1 y1) (Application p2 x2 y2) = p1 == p2 && x1 == x2 && y1 == y2
  eq (Operation p1 o1 x1 y1) (Operation p2 o2 x2 y2) = p1 == p2 && o1 == o2 && x1 == x2 && y1 == y2
  eq (FromTo p1 x1 y1) (FromTo p2 x2 y2) = p1 == p2 && x1 == x2 && y1 == y2
  eq (FromThenTo p1 x1 y1 z1) (FromThenTo p2 x2 y2 z2) = p1 == p2 && x1 == x2 && y1 == y2 && z1 == z2
  eq _ _ = false

instance Show Expression where
  show (Reserved p1 x1) = "Reserved (" <> show p1 <> ") (" <> show x1 <> ")"
  show (Identifier p1 x1) = "Identifier (" <> show p1 <> ") (" <> show x1 <> ")"
  show (LiteralNumber p1 x1) = "LiteralNumber (" <> show p1 <> ") (" <> show x1 <> ")"
  show (LiteralString p1 x1) = "LiteralString (" <> show p1 <> ") (" <> show x1 <> ")"
  show (ListExpression p1 x1) = "ListExpression (" <> show p1 <> ") (" <> show x1 <> ")"
  show (Application p1 x1 y1) = "Application (" <> show p1 <> ") (" <> show x1 <> ") (" <> show y1 <> ")"
  show (Operation p1 o1 x1 y1) = "Operation (" <> show p1 <> ") (" <> show o1 <> ") (" <> show x1 <> ") (" <> show y1 <> ")"
  show (FromTo p1 x1 y1) = "FromTo (" <> show p1 <> ") (" <> show x1 <> ") (" <> show y1 <> ")"
  show (FromThenTo p1 o1 x1 y1) = "FromThenTo (" <> show p1 <> ") (" <> show o1 <> ") (" <> show x1 <> ") (" <> show y1 <> ")"

emptyAST :: AST
emptyAST = EmptyStatement (Position { column: 1, index: 0, line: 1 }) : Nil

parseAST :: String -> Either ParseError AST
parseAST x = runParser x ast

ast :: P AST
ast = do
  whiteSpace
  xs <- semiSep statement
  eof
  pure $ xs

statement :: P Statement
statement = try (Expression <$> expression1) <|> emptyStatement

emptyStatement :: P Statement
emptyStatement = do
  p <- position
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure $ EmptyStatement p

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
  chainl1 expression3 (operator operators2)
  
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
    try $ LiteralNumber p <$> number,
    try $ LiteralString p <$> stringLiteral,
    try fromTo,
    try fromThenTo,
    try list,
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

