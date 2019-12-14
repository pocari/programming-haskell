module Sample where

import           System.IO

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
