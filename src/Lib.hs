module Lib where

import Data.List (nub)

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
  if c
    then a
    else b

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

tree = Node Leaf 1 Leaf

mapBTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBTree f Leaf = Leaf
mapBTree f (Node aTree val bTree) =
  Node (mapBTree f aTree) (f val) (mapBTree f bTree)

foldBTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldBTree _ acc Leaf = acc
foldBTree f b (Node left a right) = f a bOfLeftRight
  where
    bOfLeftRight = foldBTree f bOfLeft right
    bOfLeft = foldBTree f b left
