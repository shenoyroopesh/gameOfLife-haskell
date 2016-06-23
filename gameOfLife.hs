{-# OPTIONS_GHC -XTupleSections -XDeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

import Test.QuickCheck
import Data.List
import Control.Lens (makeLenses, set, view)
import qualified Data.Map as Map

data Cell = Cell { _x :: Int, _y :: Int, _alive :: Bool, _aliveInNextTick :: Bool, _neighbouringCells :: [Cell] }
makeLenses ''Cell

instance Eq (Cell) where
	(==) (Cell x1 y1 _ _ _) (Cell x2 y2 _ _ _) = x1 == x2 && y1 == y2
	
instance Ord (Cell) where
	compare (Cell x1 y1 _ _ _) (Cell x2 y2 _ _ _) = if (x1 == x2) then 
														(if y1 == y2 then EQ 
															else (if y1 > y2 then GT else LT)) 
													else (if x1 > x2 then GT else LT)

tick :: [Cell] -> [Cell]
tick =  map (set aliveInNextTick True) |> (\xs -> map (fillNeighbours xs) xs) |>
				map (killIfNeighbours (<2)) |> map (killIfNeighbours (>3)) |> birthNewCells |> (map finalize)

fillNeighbours :: [Cell] -> Cell -> Cell
fillNeighbours xs cell@(Cell x y _ _ _) = 
	set neighbouringCells (map (cellFromPosition xs) (neighbouringPosns x y)) cell

neighbouringPosns :: Int -> Int -> [(Int, Int)]	
neighbouringPosns x y = allCombinations [(x-1) .. (x+1)] [(y-1) .. (y+1)]

-- if find cell in list then give it back, else create new cell
cellFromPosition :: [Cell] -> (Int, Int) -> Cell
cellFromPosition xs (x,y) = maybe (Cell x y False False []) id (find (\(Cell x1 y1 _ _ _) -> x1 == x && y1 == y) xs)

killIfNeighbours :: (Int -> Bool) -> Cell -> Cell
killIfNeighbours condition cell@(Cell _ _ _ _ neighbouringCells) = 
	if (condition $ length $ filter (view alive) neighbouringCells) then (set aliveInNextTick False cell) else cell

-- first collect all neighbouring cells in one place - then make individual counts of each of them - then birth the ones that were dead
birthNewCells :: [Cell] -> [Cell]
birthNewCells a = nub $ a ++ (getNewBirthCells a)

getNewBirthCells :: [Cell] -> [Cell]
getNewBirthCells = (concatMap (\(Cell _ _ _ _ nc) -> nc)) -- |> uniquesWithCounts |> (filter (\x -> (snd x >= 3))) -- |> (map fst) -- (snd |> (>=3))) -- |> map (set aliveInNextTick True)

finalize :: Cell -> Cell
finalize x = set alive (view aliveInNextTick x) x

uniquesWithCounts :: [Cell] -> [(Cell, Int)]
uniquesWithCounts = map (\xs@(x:_) -> (x, length xs)) . group . sort

-- utils
-- pipe operator for convenience
(|>) f g = g . f

allCombinations :: [Int] -> [Int] -> [(Int, Int)]
allCombinations (x:xs) (y:ys) = concat [[(x, y)], (allCombinations xs ys), (allCombinations [x] ys), (allCombinations [y] xs)]
allCombinations [] _ = []
allCombinations _ [] = []