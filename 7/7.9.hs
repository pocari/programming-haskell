module Sample where
import           Data.Char

-- 7.9.1
sample1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
sample1 f p xs = [ f x | x <- xs, p x ]

sample2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
sample2 f p = map f . filter p

-- 7.9.2-a
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ []       = True
myAll f (x : xs) = f x && myAll f xs

-- 7.9.2-b
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []       = True
myAny f (x : xs) = f x || myAny f xs

-- 7.9.2-c
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x : xs) | f x       = x : myTakeWhile f xs
                       | otherwise = []

-- 7.9.2-d
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f xx@(x : xs) | f x       = myDropWhile f xs
                          | otherwise = xx

-- 7.9.3-map
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

-- 7.9.3-filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr pick []
 where
  pick x y | f x       = x : y
           | otherwise = y

-- 7.9.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

-- 7.9.5-a
myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x y -> f (x, y)

-- 7.9.5-b
myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x, y) -> f x y

-- 7.9.6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
-- p : 終了条件
-- h : 具体的なオペレーション
-- t : 次の再帰パラメータ生成
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

-- originals
-- bit encode decode
type Bit = Int
type Bits = [Bit]

int2bin :: Int -> Bits
int2bin 0 = []
int2bin n = m : int2bin d where (d, m) = n `divMod` 2

make8 :: Bits -> Bits
make8 bits = take 8 $ bits ++ repeat 0

chop8 :: Bits -> [Bits]
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

encode :: String -> Bits
encode = concatMap (make8 . int2bin . ord)

int2binUnfold :: Int -> Bits
int2binUnfold = unfold (== 0) (`mod` 2) (`div` 2)

-- 7.9.6
chop8Unfold :: Bits -> [Bits]
chop8Unfold = unfold null (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (\x -> f $ head x) tail

--これしかおもいつかんけど、iterateは最初の要素はfが適用されずに、引数のままなので、無理そう
iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold f = unfold (const False) f f

