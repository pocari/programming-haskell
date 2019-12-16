module Sample where

import           System.IO
import           Data.Char

wrapper :: IO ()
wrapper = do
  (x, y) <- act
  z      <- getLine
  print x
  print y
  print z
  return ()

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return (x, y)


getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

putStr' :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = do
  putChar '\n'
  return ()
putStrLn' (x : xs) = do
  putChar x
  putStrLn' xs

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  x <- getLine'
  putStr' "The string has "
  putStr' (show (length x))
  putStrLn' " characters."

----------------------------------------------------------
-- handman
sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

match :: String -> String -> String
match xs ys = [ if x `elem` ys then x else '-' | x <- xs ]

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!!"
    else do
      putStrLn (match word guess)
      play word

hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

----------------------------------------------------------
-- nim

next :: Int -> Int
next 1 = 2
next 2 = 1
next _ = error "invalid argument"

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b row n = b !! (row - 1) >= n

move :: Board -> Int -> Int -> Board
move b row num = [ update r n | (r, n) <- zip [1 ..] b ]
  where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow r n = do
  putStr (show r)
  putStr ":"
  putStrLn (concat (replicate n " *"))

putBoard :: Board -> IO ()
putBoard = putBoardHelper 1
 where
  putBoardHelper _ []       = putStrLn ""
  putBoardHelper n (x : xs) = do
    putRow n x
    putBoardHelper (n + 1) xs

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

playNim :: Board -> Int -> IO ()
playNim board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr "Player "
      putStr (show (next player))
      putStrLn " Wins !!"
    else do
      newline
      putStr "Player "
      putStrLn (show player)
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove: "
      if valid board row num
        then playNim (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: invalid move"
          playNim board player


