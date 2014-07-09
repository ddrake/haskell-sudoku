-- import qualified Data.Set as S
import Data.List
import Control.Monad

data Cell = Cell { row :: Int, col :: Int, value :: Maybe Int } deriving (Show, Read)
instance Eq Cell where x == y = row x == row y && col x == col y

type Game = [Cell]

-- Get the box for a cell
box :: Cell -> Int
box (Cell r c _) = 3 * (r `div` 3) + (c `div` 3)

-- Get an empty game (no values set)
emptyGame :: Game 
emptyGame = [Cell r c Nothing | r <- [0..8], c <- [0..8] ] 

getIndex :: Int -> Int -> Int
getIndex row col = row  * 9 + col

-- Given a game, a row, a column and a value, return a game where the cell's value is updated
updateGame :: Game -> Int -> Int -> Int -> Game
updateGame game row col newValue = 
  let (xs, _:ys) = splitAt (getIndex row col) game
  in xs ++ Cell row col (Just newValue) : ys 

updateGameForList :: Game -> [Int] -> Game
updateGameForList game ([row, col, newValue]) = updateGame game row col newValue

initValues :: Game -> [[Int]] -> Game
initValues game values = foldl (\g l -> updateGameForList g l) game values

cellsForRow :: Game -> Int -> [Cell]
cellsForRow g r = filter (\cell -> (row cell) == r) g

cellsForCol :: Game -> Int -> [Cell]
cellsForCol g c = filter (\cell -> (col cell) == c) g

cellsForBox :: Game -> Int -> [Cell]
cellsForBox g b = filter (\cell -> (box cell) == b) g

cellValues :: [Cell] -> [Int]
cellValues [] = []
cellValues ((Cell r c Nothing):xs) = cellValues xs
cellValues ((Cell r c (Just v)):xs) = v:cellValues xs

possiblesForCell :: Game -> Cell -> [Int]
possiblesForCell game (Cell r c (Just v)) = [v]
possiblesForCell game cell = 
  let rowValues = cellValues . cellsForRow game . row $ cell
      colValues = cellValues . cellsForCol game . col $ cell
      boxValues = cellValues . cellsForBox game . box $ cell
  in (\\) [1..9] . union rowValues . union colValues $ boxValues

possiblesForCells :: Game -> [Cell] -> [Int]
possiblesForCells game cells = foldr (\x a -> union a (possiblesForCell game x)) [] cells

possiblesForAllCells :: Game -> [[Int]]
possiblesForAllCells game = map (possiblesForCell game) game

zippedPossibles :: Game -> [(Cell, [Int])]
zippedPossibles game = zip game (possiblesForAllCells game)

step :: [(Cell, [Int])] -> Game
step lst = 
  let cellFor (c, lst) = if length lst == 1 then Cell (row c) (col c) (Just $ head lst) else c
  in map (\x -> cellFor x) lst

knownCount :: Game -> Int
knownCount game = foldr (\x b -> if value x == Nothing then b else b+1) 0 game 

-- If a cell has a possible value that is not possible for any other cell in its row, that must be
-- the value for the cell.
uniquesForRow :: Game -> Cell -> [Int]
uniquesForRow game cell = 
  let restOfRow = delete cell (cellsForRow game (row cell))
      otherPossibles = possiblesForCells game restOfRow
      in (possiblesForCell game cell) \\ otherPossibles

uniquesForCol :: Game -> Cell -> [Int]
uniquesForCol game cell = 
  let restOfCol = delete cell (cellsForCol game (col cell))
      otherPossibles = possiblesForCells game restOfCol
      in (possiblesForCell game cell) \\ otherPossibles

uniquesForBox :: Game -> Cell -> [Int]
uniquesForBox game cell = 
  let restOfBox = delete cell (cellsForBox game (box cell))
      otherPossibles = possiblesForCells game restOfBox
      in (possiblesForCell game cell) \\ otherPossibles

uniquesForCell :: Game -> Cell -> [Int]
uniquesForCell game cell = 
  let rowUniques = uniquesForRow game cell
      colUniques = uniquesForCol game cell
      boxUniques = uniquesForBox game cell
  in union rowUniques (union colUniques boxUniques) 

uniquesForAllCells :: Game -> [[Int]]
uniquesForAllCells game = map (uniquesForCell game) game

zippedUniques :: Game -> [(Cell, [Int])]
zippedUniques game = zip game (uniquesForAllCells game)


-- find a box (col, box) that has a 'matched pair', e.g. two cells, each with exactly two possibles [3,7]
-- this matched pair is guaranteed to also be in a col or row
-- we can eliminate 3 and 7 as possibles for every other cell in that col or row
matchedPairsForBox :: Game -> Int -> [(Cell, [Int])]
matchedPairsForBox game box =
  let boxCells = cellsForBox game box
      sortedPossibles = map sort . map (possiblesForCell game) $ boxCells
      possiblesForBox = zip boxCells sortedPossibles
  in = sort `on` filter (\x -> length (snd x) == 2) possiblesForBox

-- The idea here is to update the possibles for a row (the one that shares a matched pair with a box)
-- except for the matched pair itself
zippedPossiblesForRowExcluding :: Game -> [Cell] -> [[Int]]

-- A similar rule holds for 'matched triples', but in that case we want to look at doubles and triples of cells whose UNION
-- consists of three values.

--let g = emptyGame
--let g' = initValues g [
-- [0,0,5], [0,5,6], [0,7,7], 
-- [1,1,4], [1,3,8], [2,1,8], 
-- [2,2,1], [2,3,4], [2,5,5], 
-- [2,8,2], [3,6,7], [4,0,7], 
-- [4,2,2], [4,6,4], [4,8,9], 
-- [5,2,4], [6,0,4], [6,3,6],
-- [6,5,1], [6,6,3], [6,7,5], 
-- [7,5,4], [7,7,6], [8,1,3], 
-- [8,3,5], [8,8,8]]

