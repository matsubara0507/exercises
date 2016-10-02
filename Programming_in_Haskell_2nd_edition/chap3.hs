{-
  Chap. 3 Types and Classes
-}

module Chap3 where

-- 1.

x1 :: [Char]
x1 = ['a', 'b', 'c']

x2 :: (Char, Char, Char)
x2 = ('a', 'b', 'c')

x3 :: [(Bool, Char)]
x3 = [(False, '0'), (True, '1')]

x4 :: ([Bool], [Char])
x4 = ([False, True], ['0', '1'])

x5 :: [[a] -> [a]]
x5 = [tail, init, reverse]

-- 2.

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = repeat [0..]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f = f

-- 3.

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
