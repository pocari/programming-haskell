module LifeGame where

import           Control.Concurrent

----------------------------------------------------------
-- life game

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 20

height :: Int
height = 20

type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells b = sequence_ [ writeat p "O" | p <- b ]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map
  wrap
  [ (x - 1, y - 1)
  , (x    , y - 1)
  , (x + 1, y - 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y + 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  ]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [ p | p <- b, liveneighbs b p `elem` [2, 3] ]

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

births :: Board -> [Pos]
births b =
  [ p | p <- rmdups $ concatMap neighbs b, isEmpty b p, liveneighbs b p == 3 ]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [ return () | _ <- [1 .. n] ]

life :: Board -> IO ()
life b = do
  cls
  showcells b
  -- 0.1秒待機
  threadDelay (1 * 100 * 1000)
  life (nextgen b)

