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
  if c
    then a
    else b

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

tree :: Num a => BinaryTree a
tree = Node Leaf 1 Leaf

mapBTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBTree _ Leaf = Leaf
mapBTree f (Node aTree val bTree) =
  Node (mapBTree f aTree) (f val) (mapBTree f bTree)

foldBTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldBTree _ acc Leaf = acc
foldBTree f b (Node left a right) = f a bOfLeftRight
  where
    bOfLeftRight = foldBTree f bOfLeft right
    bOfLeft = foldBTree f b left

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
  case x of
    Nothing -> catMaybes xs
    Just x' -> x' : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe [x] =
  case x of
    Nothing -> Nothing
    Just x' -> Just [x']
flipMaybe (x:xs) =
  case flipMaybe xs of
    Nothing -> Nothing
    Just xs' ->
      case x of
        Nothing -> Nothing
        Just x' -> Just (x' : xs')
