module Sudoku
( initg,
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

-------------------------------------
-- Primary Tools for Solving a Puzzle
-------------------------------------
cycl :: Game -> Game
cycl game =
    let ct = gct game
        game' = refine . step2 . step1 $ game
    in if gct game' > ct then cycl game' else game'

guess :: Int -> Int -> Int -> Game -> Game
guess r c v = setCellValue (r-1) (c-1) v

-----------------
-- Initialization
-----------------
initg :: String -> Game
initg s = 
    let inits = map (\x -> if x == '.' then 0 :: Int else read [x] :: Int) s
    in initGame inits
    
-- Initialize a game from a list of Ints where the unknown values are represented by zeros
initGame :: [Int] -> Game 
initGame vals = 
    let val row col = vals !! (gameIndex row col)
        possibles row col = if val row col == 0 then [1..9] else [val row col]
    in [Cell row col $ possibles row col | row <- [0..8], col <- [0..8] ]

-- The index of a cell in the context of the overall 81 cell game
gameIndex :: Int -> Int -> Int
gameIndex row col = row  * 9 + col

----------------
-- Display Logic
----------------
pretty :: Game -> IO ()
pretty game = putStr . unlines . map allPoss $ rows game

sPoss :: [Int] -> String
sPoss = centerTo 10 . map (\x -> head (show x)) 

allPoss :: [Cell] -> String
allPoss row = concat . map (\c -> sPoss (possibles c)) $ row

centerTo :: Int -> String -> String
centerTo n s = 
    let half = (n - length s) `div` 2
        rest = n - length s - half
    in replicate half ' ' ++ s ++ replicate rest ' '

-----------------------
-- 'No Duplicates' Rule
-----------------------
step1 :: Game -> Game
step1 game = 
    let possibles = map (newPossiblesForCell game) game
    in zipWith (\c p -> Cell (row c) (col c) p) game possibles

newPossiblesForCell :: Game -> Cell -> [Int]
newPossiblesForCell game cell@(Cell r c p)
    | length p == 1 = p
    | otherwise =
        let rowValues = kv . cellsForRow (row cell) $ game 
            colValues = kv . cellsForCol (col cell) $ game 
            boxValues = kv . cellsForBox (box cell) $ game 
        in (\\) (possibles cell) . union rowValues . union colValues $ boxValues

-----------------------------------------
-- 'Every Value (1-9) Must Be There' Rule
--   If a cell has a possible value that is 
--   not possible for any other cell in its row, 
--   that value must be the value for the cell.
-----------------------------------------
step2 :: Game -> Game
step2 game = 
    let newCell c p = if length p == 1 then Cell (row c) (col c) p else c
        uniques = uniquesForAllCells game
    in zipWith (\c p -> newCell c p) game uniques

unionPossiblesForCells :: [Cell] -> [Int]
unionPossiblesForCells cells = 
    let allPossible = map possibles cells
    in foldr (\x a -> union a x) [] allPossible

uniquesForAllCells :: Game -> [[Int]]
uniquesForAllCells game = map (uniquesForCell game) game

uniquesForCell :: Game -> Cell -> [Int]
uniquesForCell game cell = 
    let rowUniques = uniquesForRow game cell
        colUniques = uniquesForCol game cell
        boxUniques = uniquesForBox game cell
    in union rowUniques . union colUniques $ boxUniques

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

uniquesForDisjointBlockAndCell :: [Cell] -> Cell -> [Int]
uniquesForDisjointBlockAndCell block cell = (possibles cell) \\ (unionPossiblesForCells block)

--------------------------
-- Double-Pairs Refinement
--------------------------
-- Check a row (col, box) for double pairs.  There may be multiple double pairs.  
-- If so, handle them separately
refine :: Game -> Game
refine game = 
    let gameR = foldr (\x a -> refineRow x a) game [0..8]
        gameC = foldr (\x a -> refineCol x a) gameR [0..8]
    in foldr (\x a -> refineBox x a) gameC [0..8]

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

refineBlock :: [Cell] -> [Cell] -> Game -> Game
refineBlock cells block=
    let vals = possibles (head cells)
        restOfBlock = foldr (\x a -> delete x a) block cells
        updatedRest = map (\c -> removePossibles vals c) restOfBlock
    in map (\c -> listItemOrOriginal updatedRest c)

partitionDps :: [Cell] -> [[Cell]]
partitionDps = filter (\x -> length x == 2) . groupBy ((==) `on` possibles) . sortBy (compare `on` possibles) . dpsForBlock

dpsForBlock ::[Cell] -> [Cell]
dpsForBlock = filter (\c -> length (possibles c) == 2)

-- Remove specified values from the possibles for a cell
removePossibles :: [Int] -> Cell -> Cell
removePossibles vals cell = Cell (row cell) (col cell) (possibles cell \\ vals)

-----------------
-- Sudoku Helpers
-----------------
-- Get the box for a cell
box :: Cell -> Int
box (Cell r c _) = 3 * (r `div` 3) + (c `div` 3)

cellsForRow :: Int -> Game -> [Cell]
cellsForRow r = filter (\cell -> row cell == r)

cellsForCol :: Int -> Game -> [Cell]
cellsForCol c = filter (\cell -> col cell == c)

cellsForBox :: Int -> Game -> [Cell]
cellsForBox b = filter (\cell -> box cell == b)

setCellValue :: Int -> Int -> Int -> Game -> Game
setCellValue r c v = map (\cell -> if Cell r c [v] == cell then Cell (row cell) (col cell) [v] else cell)

-- Known values for the game
kv :: [Cell] -> [Int]
kv = concat . filter (\p -> length p == 1) . map possibles

-- Count of known values for the game
gct :: Game -> Int
gct = length . kv

-- Useful for display
rows :: Game -> [[Cell]]
rows = groupsOf 9

--------------------------
-- General Purpose Helpers
--------------------------
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf 0 list = [list]
groupsOf n list = take n list : groupsOf n (drop n list)

-- Given a value and a list, return the corresponding value from the list
listItemOrOriginal :: (Eq a) => [a] -> a -> a
listItemOrOriginal list item =  
    case find (item ==) list of
        Nothing -> item
        Just listItem -> listItem
