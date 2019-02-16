module Lists where

myReverse :: [a] -> [a]
myReverse []     = []
myReverse xs = foldl (flip (:)) [] xs

squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (: []) x ++ squishAgain xs
