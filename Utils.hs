module Utils (
 (|>),
 allCombinations
) where

(|>) f g = g . f

allCombinations :: [Int] -> [Int] -> [(Int, Int)]
allCombinations (x:xs) (y:ys) = concat [[(x, y)], (allCombinations xs ys), (allCombinations [x] ys), (allCombinations xs [y])]
allCombinations [] _ = []
allCombinations _ [] = []