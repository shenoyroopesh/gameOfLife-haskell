{-# OPTIONS_GHC -XTupleSections -XDeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module GameOfLife (tick) where

import Data.List
import Control.Lens (makeLenses, set, (^.))
import qualified Data.Map as Map
import Utils

-- data type
data Cell = Cell { _x :: Int, _y :: Int, _alive :: Bool, _willBeAlive :: Bool, _neighbors :: [Cell] }
makeLenses ''Cell

instance Eq (Cell) where
	(==) a b = posnFromCell a == posnFromCell b
	
instance Ord (Cell) where
	compare c1 c2 = if result == EQ then compare (c1^.y) (c2^.y) else result where result = compare (c1^.x) (c2^.x)

posnFromCell :: Cell -> (Int, Int)
posnFromCell (Cell x y _ _ _) = (x, y)

-- if find cell in list then give it back, else create new cell
cellFromPosition :: [Cell] -> (Int, Int) -> Cell
cellFromPosition xs c@(x,y) = maybe (Cell x y False False []) id $ find (posnFromCell |> (c ==)) xs

fillNeighbours :: [Cell] -> Cell -> Cell
fillNeighbours xs (Cell x y a an _) = (Cell x y a an $ map (cellFromPosition xs) $ neighborPosns)
									where neighborPosns = delete (x,y) $ allCombinations [(x-1) .. (x+1)] [(y-1) .. (y+1)]

-- key external world interface	
tick::[(Int, Int)] -> [(Int, Int)]
tick = (map $ cellFromPosition []) |> tickInternal |> (map posnFromCell)

-- all the functions we need
tickInternal :: [Cell] -> [Cell]
tickInternal =  (map $ set alive True |> set willBeAlive True)
			|> fillNeighboursForAll |> killRequiredCells |> birthNewCells |> (map updateStatus) |> (filter (^.alive)) |> sort
			where
				fillNeighboursForAll xs = map (fillNeighbours xs) xs
				killRequiredCells = (map $ killIfNeighbors (<2) |> killIfNeighbors (>3))
				killIfNeighbors condn cell = 
					if (condn $ length $ filter (^. alive) (cell ^. neighbors)) then (set willBeAlive False cell) else cell
				birthNewCells a = nub $ a ++ (getNewCells a)
				updateStatus x = set alive (x^.willBeAlive) x

getNewCells :: [Cell] -> [Cell]
getNewCells = (concatMap (^. neighbors)) |> uniquesWithCounts |> (filter (snd |> (== 3))) 
			|> (map $ fst |> set willBeAlive True)