import Data.Map

data Cell = Cell { x :: Int, y :: Int, alive :: Bool, aliveInNextTick :: Bool, neighbouringCells :: [Cell] }

tick = (\x = map (fillNeighbours x) x) |> map (killIfNeighbours (<2)) |> map (killIfNeighbours (>3)) |> birthNewCells |> (map finalize)

fillNeighbours currentList cell@(Cell x y _ _ _) = 
	setL neighbouringCells (map cellFromPosition (allCombinations [x-1 .. x+1] [y-1 .. y+1])) cell

cellFromPosition currentList (x,y) = case (find (\cell -> getL x cell == x && getL y cell == y) currentList) of
										Just cell -> cell
										Nothing   -> (Cell x y False False [])

killIfNeighbours condition cell@(Cell _ _ _ _ neighbouringCells) = 
	if (condition (length (filter alive neighbouringCells)) then (setL aliveInNextTick False cell) else cell

-- first collect all neighbouring cells in one place - then make individual counts of each of them - then birth the ones that were dead
addCellsToBeBirthed a = nub  a:(getNewBirthCells a)

getNewBirthCells = (map neighbouringCells) |> concat |> uniquesWithCounts |> (filter (count |> >= 3)) |> map (setL aliveInNextTick true)

finalize (Cell x y alive aliveInNextTick neighbouringCells) = (Cell x y aliveInNextTick aliveInNextTick neighbouringCells)


-- utils
-- pipe operator for convenience
|> f g = g . f

uniquesWithCounts = Map.fromListWith (+) . map (, 1)

allCombinations (x:xs) (y:ys) = concat (x, y) (allCombinations xs ys) (allCombinations x ys) (allCombinations y xs)
allCombinations [] _ = []
allCombinations _ [] = []