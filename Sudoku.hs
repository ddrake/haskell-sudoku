module Sudoku
( initGame,
  cellsForRow,
  cellsForCol,
  cellsForBox,
  knownValues,
  gct,
  refineBlock,
  cycleWhileImproving,
  Cell(..),
  Game
) where

import Data.List
import Control.Monad

data Cell = Cell { row :: Int, col :: Int, possibles :: [Int] } deriving (Show, Read)
instance Eq Cell where x == y = row x == row y && col x == col y
type Game = [Cell]

-- The index of a cell in the context of the overall 81 cell game
gameIndex :: Int -> Int -> Int
gameIndex row col = row  * 9 + col

-- Get the box for a cell
box :: Cell -> Int
box (Cell r c _) = 3 * (r `div` 3) + (c `div` 3)

-- Initialize a game from a list of Ints where the unknown values are represented by zeros
initGame :: [Int] -> Game 
initGame vals = 
    let val row col = vals !! (gameIndex row col)
        possibles row col = if val row col == 0 then [1..9] else [val row col]
    in [Cell row col $ possibles row col | row <- [0..8], col <- [0..8] ]

cellsForRow :: Int -> Game -> [Cell]
cellsForRow r = filter (\cell -> row cell == r)

cellsForCol :: Int -> Game -> [Cell]
cellsForCol c = filter (\cell -> col cell == c)

cellsForBox :: Int -> Game -> [Cell]
cellsForBox b = filter (\cell -> box cell == b)

knownValues :: [Cell] -> [Int]
knownValues = concat . filter (\p -> length p == 1) . map possibles

-- Re-compute the possible values for a cell by applying the "no duplicates" rule
newPossiblesForCell :: Game -> Cell -> [Int]
newPossiblesForCell game cell@(Cell r c p)
    | length p == 1 = p
    | otherwise =
        let rowValues = knownValues . cellsForRow (row cell) $ game 
            colValues = knownValues . cellsForCol (col cell) $ game 
            boxValues = knownValues . cellsForBox (box cell) $ game 
        in (\\) (possibles cell) . union rowValues . union colValues $ boxValues

step1 :: Game -> Game
step1 game = 
    let possibles = map (newPossiblesForCell game) game
    in zipWith (\c p -> Cell (row c) (col c) p) game possibles

-- If a cell has a possible value that is not possible for any other cell in its row, 
-- that value must be the value for the cell.
unionPossiblesForCells :: [Cell] -> [Int]
unionPossiblesForCells cells = 
    let allPossible = map possibles cells
    in foldr (\x a -> union a x) [] allPossible

uniquesForDisjointBlockAndCell :: [Cell] -> Cell -> [Int]
uniquesForDisjointBlockAndCell block cell = (possibles cell) \\ (unionPossiblesForCells block)

uniquesForRow :: Game -> Cell -> [Int]
uniquesForRow game cell = 
    let block = delete cell . cellsForRow (row cell) $ game
    in uniquesForDisjointBlockAndCell block cell

uniquesForCol :: Game -> Cell -> [Int]
uniquesForCol game cell = 
    let block = delete cell . cellsForCol (col cell) $ game 
    in uniquesForDisjointBlockAndCell block cell

uniquesForBox :: Game -> Cell -> [Int]
uniquesForBox game cell = 
    let block = delete cell . cellsForBox (box cell) $ game
    in uniquesForDisjointBlockAndCell block cell

uniquesForCell :: Game -> Cell -> [Int]
uniquesForCell game cell = 
    let rowUniques = uniquesForRow game cell
        colUniques = uniquesForCol game cell
        boxUniques = uniquesForBox game cell
    in union rowUniques . union colUniques $ boxUniques

uniquesForAllCells :: Game -> [[Int]]
uniquesForAllCells game = map (uniquesForCell game) game

-- For any cells whose possibles can be replaced by a singleton 
-- based on the "all nine numbers must be present" rule, perform the replacemnt. 
step2 :: Game -> Game
step2 game = 
    let newCell c p = if length p == 1 then Cell (row c) (col c) p else c
        uniques = uniquesForAllCells game
    in zipWith (\c p -> newCell c p) game uniques

-- Check a row (col, box) for double pairs.  There may be multiple double pairs.  
-- If so, we'll need to handle them separately
dpsForBlock ::[Cell] -> [Cell]
dpsForBlock = filter (\c -> length (possibles c) == 2)

-- Update a cell from a list of values
removePossibles :: [Int] -> Cell -> Cell
removePossibles vals cell = Cell (row cell) (col cell) (possibles cell \\ vals)

-- General Purpose Helper: Given a value and a list, return the corresponding value from the list
-- if it's there, otherwise the original value
listItemOrOriginal :: (Eq a) => [a] -> a -> a
listItemOrOriginal list item =  
    case find (item ==) list of
        Nothing -> item
        Just listItem -> listItem

gct :: Game -> Int
gct = length . knownValues

dpsForRow :: Int -> Game -> [Cell]
dpsForRow row = dpsForBlock . cellsForRow row

dpsForCol :: Int -> Game -> [Cell]
dpsForCol col = dpsForBlock . cellsForCol col

dpsForBox :: Int -> Game -> [Cell]
dpsForBox box = dpsForBlock . cellsForBox box

refineBlock :: [Cell] -> [Cell] -> Game -> Game
refineBlock cells block=
    let vals = possibles (head cells)
        restOfBlock = foldr (\x a -> delete x a) block cells
        updatedRest = map (\c -> removePossibles vals c) restOfBlock
    in map (\c -> listItemOrOriginal updatedRest c)

cycleWhileImproving :: Game -> Game
cycleWhileImproving game =
    let ct = gct game
        game' = step2 . step1 $ game
    in if gct game' > ct then cycleWhileImproving game' else game'


-- Sample usage
--let inits = [5,0,0, 0,0,6, 0,7,0,  0,4,0, 8,0,0, 0,0,0,  0,8,1, 4,0,5, 0,0,2,  0,0,0, 0,0,0, 7,0,0,  7,0,2, 0,0,0, 4,0,9,  0,0,4, 0,0,0, 0,0,0,  4,0,0, 6,0,1, 3,5,0,   0,0,0, 0,0,4, 0,6,0,  0,3,0, 5,0,0, 0,0,8] :: [Int]
--let g = initGame inits
--let g' = cycleWhileImproving g
--let blk = cellsForBox 8 g'
--let dps = dpsForBlock blk
--let col6 = cellsForCol 6 g'
--let g = refineBlock dps col6 g'
--let g' = cycleWhileImproving g
--gct g' ---> 81  :-)
--knownValues ---> the solution

-- TODO: In general, we could have up to four pairs of matching values for a row (col, box)
-- I think we should separate the pairs into a list of lists, then process each pair via a mapping operation.
-- To avoid having to keep track of which pairs have been processed in each of the various contexts,
-- I think it would be simplest to process all pairs for the game in a single step