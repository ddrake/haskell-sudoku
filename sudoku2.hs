import Data.List
import Control.Monad

data Cell = Cell { row :: Int, col :: Int, possibles :: [Int] } deriving (Show, Read)
instance Eq Cell where x == y = row x == row y && col x == col y

type Game = [Cell]

index :: Int -> Int -> Int
index row col = row  * 9 + col

-- Get the box for a cell
box :: Cell -> Int
box (Cell r c _) = 3 * (r `div` 3) + (c `div` 3)

-- Get an empty game (no values set)
initGame :: [Int] -> Game 
initGame vals = 
    let val row col = vals !! (index row col)
        possibles row col = if val row col == 0 then [1..9] else [val row col]
    in [Cell row col (possibles row col) | row <- [0..8], col <- [0..8] ]

cellsForRow :: Game -> Int -> [Cell]
cellsForRow g r = filter (\cell -> (row cell) == r) g

cellsForCol :: Game -> Int -> [Cell]
cellsForCol g c = filter (\cell -> (col cell) == c) g

cellsForBox :: Game -> Int -> [Cell]
cellsForBox g b = filter (\cell -> (box cell) == b) g

knownValues :: [Cell] -> [Int]
knownValues cells = concat . filter (\p -> length p == 1) . map possibles $ cells

newPossiblesForCell :: Game -> Cell -> [Int]
newPossiblesForCell game cell@(Cell r c p)
    | length p == 1 = p
    | otherwise =
        let rowValues = knownValues . cellsForRow game . row $ cell
            colValues = knownValues . cellsForCol game . col $ cell
            boxValues = knownValues . cellsForBox game . box $ cell
        in (\\) (possibles cell) . union rowValues . union colValues $ boxValues

newPossiblesForAllCells :: Game -> [[Int]]
newPossiblesForAllCells game = map (newPossiblesForCell game) game

step1 :: Game -> Game
step1 game = 
    let possibles = newPossiblesForAllCells game
    in zipWith (\c p -> Cell (row c) (col c) p) game possibles

-- If a cell has a possible value that is not possible for any other cell in its row, that must be
-- the value for the cell.

unionPossiblesForCells :: [Cell] -> [Int]
unionPossiblesForCells cells = 
    let allPossible = map possibles cells
    in foldr (\x a -> union a x) [] allPossible

uniquesForRow :: Game -> Cell -> [Int]
uniquesForRow game cell = 
    let restOfRow = delete cell (cellsForRow game (row cell))
        otherPossibles = unionPossiblesForCells restOfRow
    in (possibles cell) \\ otherPossibles

uniquesForCol :: Game -> Cell -> [Int]
uniquesForCol game cell = 
    let restOfCol = delete cell (cellsForCol game (col cell))
        otherPossibles = unionPossiblesForCells restOfCol
    in (possibles cell) \\ otherPossibles

uniquesForBox :: Game -> Cell -> [Int]
uniquesForBox game cell = 
    let restOfBox = delete cell (cellsForBox game (box cell))
        otherPossibles = unionPossiblesForCells restOfBox
    in (possibles cell) \\ otherPossibles

uniquesForCell :: Game -> Cell -> [Int]
uniquesForCell game cell = 
    let rowUniques = uniquesForRow game cell
        colUniques = uniquesForCol game cell
        boxUniques = uniquesForBox game cell
    in union rowUniques (union colUniques boxUniques) 

uniquesForAllCells :: Game -> [[Int]]
uniquesForAllCells game = map (uniquesForCell game) game

step2 :: Game -> Game
step2 game = 
    let newCell c p = if length p == 1 then Cell (row c) (col c) p else c
        uniques = uniquesForAllCells game
    in zipWith (\c p -> newCell c p) game uniques

-- let inits = [5,0,0, 0,0,6, 0,7,0,  0,4,0, 8,0,0, 0,0,0,  0,8,1, 4,0,5, 0,0,2,  0,0,0, 0,0,0, 7,0,0,  7,0,2, 0,0,0, 4,0,9,  0,0,4, 0,0,0, 0,0,0,  4,0,0, 6,0,1, 3,5,0,   0,0,0, 0,0,4, 0,6,0,  0,3,0, 5,0,0, 0,0,8] :: [Int]
-- let g = initGame inits
-- let g' = step1 g
-- let g = step2 g'
-- length $ knownValues g


-- Todo: Check each row (col, box) to see if it has two cells with the same pair of possibles.  
-- If so, remove these from the lists of possibles for the other cells in the row (col, box)

-- Check a row (col, box) for double pairs.  There may be multiple double pairs.  If so, we'll need to handle them separately
dpsForBlock ::[Cell] -> [Cell]
dpsForBlock block = filter (\c -> length (possibles c) == 2) block

-- Update a cell from a list of values
removePossibles :: [Int] -> Cell -> Cell
removePossibles vals cell = Cell (row cell) (col cell) (possibles cell \\ vals)

-- Given a value and a list, return the value if it's not in the list, otherwise the corresponding value from the list
listItemOrOriginal :: (Eq a) => [a] -> a -> a
listItemOrOriginal list item =  case find (item ==) list of
                                    Nothing -> item
                                    Just listItem -> listItem

gct :: Game -> Int
gct game = length . knownValues $ game


dpsForRow :: Game -> Int -> Game
dpsForRow game row = dpsForBlock (cellsForRow game row)

dpsForCol :: Game -> Int -> Game
dpsForCol game col = dpsForBlock (cellsForCol game col)

dpsForBox :: Game -> Int -> Game
dpsForBox game box = dpsForBlock (cellsForBox game box)

refineBlock :: Game -> [Cell] -> [Cell] -> Game
refineBlock game cells block =
    let vals = possibles (head cells)
        restOfBlock = foldr (\x a -> delete x a) block cells
        updatedRest = map (\c -> removePossibles vals c) restOfBlock
    in map (\c -> listItemOrOriginal updatedRest c) game

cycleWhileImproving :: Game -> Game
cycleWhileImproving game =
    let ct = gct game
        game1 = step1 game
        game2 = step1 game1
        game3 = step2 game2
        game4 = step2 game3
    in if gct game4 < ct then cycleWhileImproving game4 else game4


-- let blk = cellsForBox g 8
-- let dps = dpsForBlock blk
-- let col6 = cellsForCol g 6
-- let g' = refineBlock g dps col6
