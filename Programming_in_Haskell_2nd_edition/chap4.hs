{-
  Chap. 4 Defining functions
-}

module Chap4 where

-- 1.

halves :: [a] -> ([a],[a])
halves xs = splitAt (length xs `div` 2) xs

-- 2.

thirdA :: [a] -> a
thirdA = head . tail . tail

thirdB :: [a] -> a
thirdB = flip (!!) 2

thirdC :: [a] -> a
thirdC (_ : _ : x : _) = x
thirdC _ = error "a few elemnts of list"

-- 3.

safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs
  | null xs = []
  | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs

-- 4.

orA :: Bool -> Bool -> Bool
orA True True   = True
orA True False  = True
orA False True  = True
orA False False = False

orB :: Bool -> Bool -> Bool
orB False False = False
orB _ _ = True

orC :: Bool -> Bool -> Bool
orC True _  = True
orC False b = b

orD :: Bool -> Bool -> Bool
orD b c
  | b == c = b
  | otherwise = True

-- 5.

andA :: Bool -> Bool -> Bool
andA b c = if b then if c then True else False else False

-- 6,

andB :: Bool -> Bool -> Bool
andB b c = if b then c else False

-- 7.

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

luhnDouble :: Int -> Int
luhnDouble x = ((x + 5) `mod` 10 * 2 + 1) `mod` 11

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0 
