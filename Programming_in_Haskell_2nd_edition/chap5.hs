{-
  Chap. 5 List comprehensions
-}

module Chap5 where

import Data.Char (ord, chr, isLower, isUpper, toLower, toUpper)

-- 1

solve1 :: Int
solve1 = sum [x*x | x <- [1..100]]

-- 2

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

-- 3

square :: Int -> [(Int, Int)]
square n = [p | p <- grid n n, uncurry (/=) p]

-- 4

replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- 5

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

-- 6

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == 2*x]

-- 7

solve7 :: [(Int,Int)]
solve7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8

find :: Eq a => a -> [(a, b)] -> [b]
find k' t = [v | (k, v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x' xs = [i | (x, i) <- zip xs [0..], x == x']

-- 9

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [uncurry (*) z | z <- zip xs ys]

-- 10

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = toUpper . shift n . toLower $ c
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
