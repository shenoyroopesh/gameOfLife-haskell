import Data.Map

data Cell = Cell { x :: Int, y :: Int, alive :: Bool, aliveInNextTick :: Bool, neighbouringCells :: [Cell] }

tick = (\xs = map (fillNeighbours xs) xs) |> map (setL aliveInNextTick True) |>
				map (killIfNeighbours (<2)) |> map (killIfNeighbours (>3)) |> birthNewCells |> (map finalize)

fillNeighbours xs cell@(Cell x y _ _ _) = 
	setL neighbouringCells (map cellFromPosition xs (allCombinations [x-1 .. x+1] [y-1 .. y+1])) cell

-- if find cell in list then give it back, else create new cell
cellFromPosition xs (x,y) = maybe (Cell x y False False []) id (find (\cell -> getL x cell == x && getL y cell == y) xs)

killIfNeighbours condition cell@(Cell _ _ _ _ neighbouringCells) = 
	if (condition (length (filter alive neighbouringCells)) then (setL aliveInNextTick False cell) else cell

-- first collect all neighbouring cells in one place - then make individual counts of each of them - then birth the ones that were dead
birthNewCells a = nub a:(getNewBirthCells a)

getNewBirthCells = (map neighbouringCells) |> concat |> uniquesWithCounts |> (filter (count |> >= 3)) |> map (setL aliveInNextTick true)

finalize x = setL alive (getL aliveInNextTick x) x


-- utils
-- pipe operator for convenience
|> f g = g . f

uniquesWithCounts = Map.fromListWith (+) . map (, 1)

allCombinations (x:xs) (y:ys) = concat (x, y) (allCombinations xs ys) (allCombinations x ys) (allCombinations y xs)
allCombinations [] _ = []
allCombinations _ [] = []