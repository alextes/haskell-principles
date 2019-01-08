module Lib where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd $ divMod xLast 10
  where
    xLast = x `div` 10

foldBool :: a -> a -> Bool -> a
foldBool a b c =
  case c of
    True  -> a
    False -> b

foldBool' :: a -> a -> Bool -> a
foldBool' a b c
  | _ _ True = a
  | _ _ False = b
