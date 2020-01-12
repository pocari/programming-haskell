{-# OPTIONS -Wall -Werror #-}
module Sample2 where

import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P
  (\inp -> case inp of
    []       -> []
    (x : xs) -> [(x, xs)]
  )

instance Functor Parser where
  -- fmapt :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P
    (\inp -> case p inp of
      []        -> []
      [(x, xs)] -> [(f x, xs)]
      _         -> [] -- ここは実質はないが、-Werror してるので、入れておく
    )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (P pf) <*> pa = P
    (\inp -> case pf inp of
      []        -> []
      [(f, xs)] -> parse (fmap f pa) xs
      _         -> [] -- ここは実質はないが、-Werror してるので、入れておく
    )

instance Monad Parser where
  -- (>>=) :: Parse a -> (a -> Parse b) -> Parse b
  (P p) >>= f = P
    (\inp -> case p inp of
      []        -> []
      [(x, xs)] -> parse (f x) xs
      _         -> [] -- ここは実質はないが、-Werror してるので、入れておく
    )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P
    (\inp -> case parse p inp of
      []        -> parse q inp
      [(x, xs)] -> [(x, xs)]
      _         -> [] -- ここは実質はないが、-Werror してるので、入れておく
    )

sat :: (Char -> Bool) -> Parser Char
sat f = do
  x <- item
  if f x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

int :: Parser Int
int =
  do
      _ <- char '-'
      n <- nat
      return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
  _ <- space
  x <- p
  _ <- space
  return x

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

nats :: Parser [Int]
nats = do
  _  <- symbol "["
  n  <- natural
  ns <- many
    (do
      _ <- symbol ","
      natural
    )
  _ <- symbol "]"
  return (n : ns)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Nat Int
          deriving Show

eval :: Expr -> Int
eval (Nat n  ) = n
eval (Add l r) = eval l + eval r
eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r
eval (Div l r) = eval l `div` eval r

run :: String -> Int
run s = case parse expr s of
  [(e, [])] -> eval e
  [(_, xs)] -> error ("Unused input " ++ xs)
  _         -> error "Invalid Input"

-- BNF
-- expr   ::= term   (('-' | '+) term   | e)
-- term   ::= factor (('/' | '*) factor | e)
-- factor ::= '(' expr ')' | number
-- number ::= 1 | 2 | 3 | ...

expr :: Parser Expr
expr = do
  l <- term
  exprSubAdd l <|> return l

exprSubAdd :: Expr -> Parser Expr
exprSubAdd lhs = do
  elems <- many
    (do
      op <- string "+" <|> string "-"
      f  <- term
      return (op, f)
    )
  return $ foldl applyMulDiv lhs elems
 where
  applyMulDiv :: Expr -> (String, Expr) -> Expr
  applyMulDiv acc (op, e1) = if op == "+" then Add acc e1 else Sub acc e1

term :: Parser Expr
term = do
  lhs <- factor
  termMulDiv lhs <|> return lhs

termMulDiv :: Expr -> Parser Expr
termMulDiv lhs = do
  elems <- many
    (do
      op <- string "*" <|> string "/"
      f  <- factor
      return (op, f)
    )
  return $ foldl applyMulDiv lhs elems
 where
  applyMulDiv :: Expr -> (String, Expr) -> Expr
  applyMulDiv acc (op, e1) = if op == "*" then Mul acc e1 else Div acc e1

factor :: Parser Expr
factor =
  do
      _ <- string "("
      e <- expr
      _ <- string ")"
      return e
    <|> do
          n <- natural
          return (Nat n)

