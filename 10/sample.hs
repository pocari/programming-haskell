module Sample where

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

