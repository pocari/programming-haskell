module Sanmoku where

import           Data.Char
import           Data.List
-- import           System.IO

size :: Int
size = 3

depth :: Int
depth = 9

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

type Grid = [[Player]]

data Player = O | B | X
            deriving (Eq, Ord, Show)

sampleOWin1 :: Grid
sampleOWin1 = [[O, O, O], [X, X, B], [X, X, B]]

sampleOWin2 :: Grid
sampleOWin2 = [[O, X, B], [O, X, B], [O, X, B]]

sampleOWin3 :: Grid
sampleOWin3 = [[O, X, O], [X, O, B], [O, X, B]]

sampleOWin4 :: Grid
sampleOWin4 = [[O, X, O], [X, O, B], [B, X, O]]


next :: Player -> Player
next O = X
next X = O
next B = B -- dummy

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
 where
  ps = concat g
  os = length (filter (== O) ps)
  xs = length (filter (== X) ps)

wins :: Player -> Grid -> Bool
wins p g = any line (row ++ col ++ dias)
 where
  line = all (== p)
  row  = g
  col  = transpose g
  dias = [diag g, diag $ map reverse g]

diag :: Grid -> [Player]
diag g = map (\(pos, r) -> r !! (pos - 1)) (zip [1 .. size] g)

won :: Grid -> Bool
won g = wins O g || wins X g

playerToStr :: Player -> String
playerToStr B = " "
playerToStr O = "O"
playerToStr X = "X"

join :: String -> [String] -> String
join _   []       = ""
join _   [xs    ] = xs
join sep (s : ss) = s ++ sep ++ join sep ss

interleave :: a -> [a] -> [a]
interleave x []       = []
interleave x [y     ] = [y]
interleave x (y : ys) = y : x : interleave x ys

-- [B, O, O]
-- [O, X, O]
-- [X, X, X]
--
--    |   |
--    | O | O
--    |   |
-- ---+---+----
--    |   |
--  O | X | O
--    |   |
-- ---+---+----
--    |   |
--  X | X | X
--    |   |
--

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [join "+" (replicate size "---")]

showPlayer :: Player -> String
showPlayer O = " O "
showPlayer X = " X "
showPlayer B = "   "

showRow :: [Player] -> [String]
showRow ps =
  [ join "|" (replicate size "   ")
  , join "|" (map showPlayer ps)
  , join "|" (replicate size "   ")
  ]

valid :: Grid -> Int -> Bool
valid g n = 0 <= n && n < (size * size) && concat g !! n == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = [ chop size (xs ++ [p] ++ ys) | valid g i ]
  where (xs, B : ys) = splitAt i (concat g)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
            deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [ gametree g' (next p) | g' <- moves g p ]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [ move g i p | i <- [0 .. ((size * size) - 1)] ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _ ) = Node a []
prune n (Node x ts) = Node x [ prune (n - 1) t | t <- ts ]

