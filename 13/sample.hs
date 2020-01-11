{-# OPTIONS -Wall -Werror #-}
module Sample where

import           System.IO
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

-- 二文字目を読み飛ばすパーサ
sample :: Parser (Char, Char)
sample = do
  x <- item
  _ <- item
  z <- item
  return (x, z)

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

-- BNF
-- expr   ::= term   ('+' expr | e)
-- term   ::= factor ('*' term | e)
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 |...

expr :: Parser Int
expr = do
  lhs <- term
  do
      _   <- symbol "+"
      rhs <- expr
      return (lhs + rhs)
    <|> return lhs

term :: Parser Int
term = do
  lhs <- factor
  do
      _   <- symbol "*"
      rhs <- term
      return (lhs * rhs)
    <|> return lhs

factor :: Parser Int
factor =
  do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e
    <|> natural

-- 計算機で違うevalを定義するのでここの名前はeval'にする
eval' :: String -> Int
eval' s = case parse expr s of
  [(n, [])] -> n
  [(_, xs)] -> error ("Unused input " ++ xs)
  _         -> error "Invalid Input"

-- 計算機

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

box :: [String]
box =
  [ "+---------------+"
  , "|               |"
  , "+---+---+---+---+"
  , "| q | c | d | = |"
  , "+---+---+---+---+"
  , "| 1 | 2 | d | + |"
  , "+---+---+---+---+"
  , "| 4 | 5 | 6 | - |"
  , "+---+---+---+---+"
  , "| 7 | 8 | 9 | * |"
  , "+---+---+---+---+"
  , "| 0 | ( | ) | / |"
  , "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
 where
  standard = "qcd=123+456-789*0()/"
  extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [ writeat (1, y) b | (y, b) <- zip [1 ..] box ]

display :: String -> IO ()
display xs = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if elem c buttons
    then process c xs
    else do
      _ <- beep
      calc xs

quit :: IO ()
quit = goto (1, 14)

beep :: IO ()
beep = putStr "\BEL"

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

clear :: IO ()
clear = calc []

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, [])] -> calc (show n)
  _         -> do
    _ <- beep
    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

run :: IO ()
run = do
  cls
  showbox
  clear

---------------------------------------------------
-- 13.11.1
comment :: Parser String
comment = do
  _ <- string "--"
  c <- many (sat (/= '\n'))
  _ <- char '\n'
  return c

-- 13.11.2
-- 2番目の構文規則
-- expr   ::= expr + expr | term
-- term   ::= term * term | factor
-- factor ::= ( expr ) | nat
-- nat    ::= 0 | 1 | 2 |...
--
-- 2 + 3 + 4 の構文木
--          expr
--          /  \
--         /    \
--        /      \
--       expr + expr
--       /  \      \
--      /    \     nat
--     /      \     |
--   expr  + expr   4
--     |       |
--    nat     nat
--     |       |
--     2       3
--
--
--          expr
--          /  \
--         /    \
--        /      \
--       expr + expr
--       |      /  \
--      nat    /    \
--       |    /      \
--       2   expr +  expr
--            |       |
--           nat     nat
--            |       |
--            3       4

-- 13.11.3
-- 3番目の構文規則
-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= ( expr ) | nat
-- nat    ::= 0 | 1 | 2 |...

-- 2 + 3
--          expr
--          /  \
--         /    \
--        /      \
--      term  + expr
--        |       |
--       nat     nat
--        |       |
--        2       3

-- 2 * 3 * 4
--          expr
--          /  \
--         /    \
--        /      \
--     factor *  term
--       /  \      \
--      /    \     nat
--     /      \     |
-- factor  * term   4
--     |       |
--    nat     nat
--     |       |
--     2       3

-- (2 + 3) + 4
--             expr
--             /  \
--            /    \
--           /      \
--          term + expr
--         /           \
--        /           nat
--    ( expr )         |
--      /  \           4
--     /    \
--    /      \
--  term  +  expr
--   |       |
--  nat     nat
--   |       |
--   2       3
