module Utils (
 (|>),
 allCombinations, 
 uniquesWithCounts
) where
	
import Data.List

(|>) f g = g . f

allCombinations :: [Int] -> [Int] -> [(Int, Int)]
allCombinations (x:xs) (y:ys) = concat [[(x, y)], (allCombinations xs ys), (allCombinations [x] ys), (allCombinations xs [y])]
allCombinations [] _ = []
allCombinations _ [] = []

uniquesWithCounts :: Ord a => [a] -> [(a, Int)]
uniquesWithCounts = sort |> group |> map (\xs@(x:_) -> (x, length xs))