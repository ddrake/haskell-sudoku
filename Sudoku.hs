module Sudoku
( initGame,
  initg,
  cellsForRow,
  cellsForCol,
  cellsForBox,
  kv,
  gct,
  refine,
  cycl,
  pretty,
  Cell(..),
  Game
) where

import Data.List
import Data.Function

data Cell = Cell { row :: Int, col :: Int, possibles :: [Int] } deriving (Show, Read)
instance Eq Cell where x == y = row x == row y && col x == col y
type Game = [Cell]

rows :: Game -> [[Cell]]
rows = groupsOf 9

sPoss :: [Int] -> String
sPoss = centerTo 10 . map (\x -> head (show x)) 

allPoss :: [Cell] -> String
allPoss row = concat . map (\c -> sPoss (possibles c)) $ row

pretty :: Game -> IO ()
pretty game = putStr . unlines . map allPoss $ rows game

centerTo :: Int -> String -> String
centerTo n s = 
    let half = (n - length s) `div` 2
        rest = n - length s - half
    in replicate half ' ' ++ s ++ replicate rest ' '

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

initg :: String -> Game
initg s = 
    let inits = map (\x -> if x == '.' then 0 :: Int else read [x] :: Int) s
    in initGame inits
    
cellsForRow :: Int -> Game -> [Cell]
cellsForRow r = filter (\cell -> row cell == r)

cellsForCol :: Int -> Game -> [Cell]
cellsForCol c = filter (\cell -> col cell == c)

cellsForBox :: Int -> Game -> [Cell]
cellsForBox b = filter (\cell -> box cell == b)

kv :: [Cell] -> [Int]
kv = concat . filter (\p -> length p == 1) . map possibles

-- Re-compute the possible values for a cell by applying the "no duplicates" rule
newPossiblesForCell :: Game -> Cell -> [Int]
newPossiblesForCell game cell@(Cell r c p)
    | length p == 1 = p
    | otherwise =
        let rowValues = kv . cellsForRow (row cell) $ game 
            colValues = kv . cellsForCol (col cell) $ game 
            boxValues = kv . cellsForBox (box cell) $ game 
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

-- General Purpose Helper: Break a list into n-sized chunks
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf 0 list = [list]
groupsOf n list = take n list : groupsOf n (drop n list)
-- General Purpose Helper: Given a value and a list, return the corresponding value from the list
-- if it's there, otherwise the original value
listItemOrOriginal :: (Eq a) => [a] -> a -> a
listItemOrOriginal list item =  
    case find (item ==) list of
        Nothing -> item
        Just listItem -> listItem

gct :: Game -> Int
gct = length . kv

partitionDps :: [Cell] -> [[Cell]]
partitionDps = filter (\x -> length x == 2) . groupBy ((==) `on` possibles) . sortBy (compare `on` possibles) . dpsForBlock

refineBlock :: [Cell] -> [Cell] -> Game -> Game
refineBlock cells block=
    let vals = possibles (head cells)
        restOfBlock = foldr (\x a -> delete x a) block cells
        updatedRest = map (\c -> removePossibles vals c) restOfBlock
    in map (\c -> listItemOrOriginal updatedRest c)

refineRow :: Int -> Game -> Game
refineRow row game = 
    let rowCells = cellsForRow row game
        partitioned = partitionDps rowCells
    in foldr (\x a -> refineBlock x rowCells a) game partitioned

refineCol :: Int -> Game -> Game
refineCol col game = 
    let colCells = cellsForCol col game
        partitioned = partitionDps colCells
    in foldr (\x a -> refineBlock x colCells a) game partitioned

refineBox :: Int -> Game -> Game
refineBox box game = 
    let boxCells = cellsForBox box game
        partitioned = partitionDps boxCells
    in foldr (\x a -> refineBlock x boxCells a) game partitioned

cycl :: Game -> Game
cycl game =
    let ct = gct game
        game' = refine . step2 . step1 $ game
    in if gct game' > ct then cycl game' else game'

refine :: Game -> Game
refine game = 
    let gameR = foldr (\x a -> refineRow x a) game [0..8]
        gameC = foldr (\x a -> refineCol x a) gameR [0..8]
    in foldr (\x a -> refineBox x a) gameC [0..8]

setCellValue :: Int -> Int -> Int -> Game -> Game
setCellValue r c v = map (\cell -> if Cell r c [v] == cell then Cell (row cell) (col cell) [v] else cell)

guess :: Int -> Int -> Int -> Game -> Game
guess r c v = setCellValue (r-1) (c-1) v

-- Sample usage
--let g = initg ".94...13..............76..2.8..1.....32.........2...6.....5.4.......8..7..63.4..8"
--let g' = cycl g
--let g = refine g'
--gct g' ---> 40 (not there yet)
--let g = refine g'
--gct g  ---> 41 (found one using double-pairs)
--let g' = cycl g
--gct g ---> 81  :-)
--kv ---> 'known values' == the solution
