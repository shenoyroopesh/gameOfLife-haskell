{-# OPTIONS_GHC -XTupleSections -XDeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module GameOfLife (tickExtern) where

import Data.List
import Control.Lens (makeLenses, set, view)
import qualified Data.Map as Map
import Utils

-- data type
data Cell = Cell { _x :: Int, _y :: Int, _alive :: Bool, _aliveInNextTick :: Bool, _neighbouringCells :: [Cell] }
makeLenses ''Cell

instance Eq (Cell) where
	(==) a b = posnFromCell a == posnFromCell b
	
instance Ord (Cell) where
	compare a@(Cell x1 y1 _ _ _) b@(Cell x2 y2 _ _ _) = 
		if a == b then EQ else if x1 > x2 || (x1 == x2 && y1 > y2) then GT else LT

posnFromCell :: Cell -> (Int, Int)
posnFromCell (Cell x y _ _ _) = (x, y)

-- if find cell in list then give it back, else create new cell
cellFromPosition :: [Cell] -> (Int, Int) -> Cell
cellFromPosition xs c@(x,y) = maybe (Cell x y False False []) id (find (posnFromCell |> (c ==)) xs)

fillNeighbours :: [Cell] -> Cell -> Cell
fillNeighbours xs (Cell x y a an _) = (Cell x y a an (map (cellFromPosition xs) $ neighbouringPosns x y))

-- key external world interface												
tickExtern::[(Int, Int)] -> [(Int, Int)]
tickExtern = (map $ cellFromPosition []) |> tick |> (map $ posnFromCell)

-- all the functions we need
tick :: [Cell] -> [Cell]
tick =  (map $ set alive True |> set aliveInNextTick True) 
			|> (\xs -> map (fillNeighbours xs) xs) 
			|> (map $ killIfNeighbours (<2) |> killIfNeighbours (>3)) 
			|> birthNewCells |> (map finalize) |> (filter $ view alive) |> sort

neighbouringPosns :: Int -> Int -> [(Int, Int)]	
neighbouringPosns x y = delete (x,y) $ allCombinations [(x-1) .. (x+1)] [(y-1) .. (y+1)]


killIfNeighbours :: (Int -> Bool) -> Cell -> Cell
killIfNeighbours condition cell@(Cell _ _ _ _ nc) = 
	if (condition $ length $ filter (view alive) nc) then (set aliveInNextTick False cell) else cell

-- first collect all neighbouring cells in one place - then make individual counts of each of them - then birth the ones that were dead
birthNewCells :: [Cell] -> [Cell]
birthNewCells a = nub $ a ++ (getNewBirthCells a)

getNewBirthCells :: [Cell] -> [Cell]
getNewBirthCells = (concatMap $ view neighbouringCells) |> uniquesWithCounts |> (filter (snd |> (== 3))) 
			|> (map $ fst |> set aliveInNextTick True)

finalize :: Cell -> Cell
finalize x = set alive (view aliveInNextTick x) x