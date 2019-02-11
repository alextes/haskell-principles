module Recursion where

sum' :: Integer -> Integer -> Integer
sum' agg 1    = agg + 1
sum' agg next = sum' (agg + next) (next - 1)

sum'' :: (Eq a, Num a) => a -> a
sum'' 1 = 1
sum'' x = x + sum'' (x - 1)

mul' :: (Integral a) => a -> a -> a
mul' a 1 = a
mul' a b = a + mul' a (b - 1)
