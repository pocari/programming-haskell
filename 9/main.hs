module Main where

import           Sample

-- -- ./main  4.57s user 0.04s system 94% cpu 4.893 total
-- main :: IO ()
-- main = print (solutions [1, 3, 7, 10, 25, 50] 765)

-- ./main  0.28s user 0.01s system 45% cpu 0.633 total (slow valid)
-- ./main  0.05s user 0.00s system 14% cpu 0.393 total (fast valid)
main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)
