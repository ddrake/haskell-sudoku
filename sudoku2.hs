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

-- Given a game, a row, a column and an updated list of possible values, 
-- return a game where the cell's value is updated to a singleton list containing the value
updateGame :: Game -> Int -> Int -> [Int] -> Game
updateGame game row col newPossibles = 
  let (xs, _:ys) = splitAt (index row col) game
  in xs ++ Cell row col newPossibles : ys 


cellsForRow :: Game -> Int -> [Cell]
cellsForRow g r = filter (\cell -> (row cell) == r) g

cellsForCol :: Game -> Int -> [Cell]
cellsForCol g c = filter (\cell -> (col cell) == c) g

cellsForBox :: Game -> Int -> [Cell]
cellsForBox g b = filter (\cell -> (box cell) == b) g


knownValues :: [Cell] -> [Int]
knownValues cells = concat . filter (\c -> length c == 1) $ cells

possiblesForCell :: Game -> Cell -> [Int]
possiblesForCell game (Cell r c (Just v)) = [v]
possiblesForCell game cell = 
  let rowValues = knownValues . cellsForRow game . row $ cell
      colValues = knownValues . cellsForCol game . col $ cell
      boxValues = knownValues . cellsForBox game . box $ cell
  in (\\) [1..9] . union rowValues . union colValues $ boxValues


possiblesForAllCells :: Game -> [[Int]]
possiblesForAllCells game = map (possiblesForCell game) game

step :: Game -> [[Int]] -> Game
step game poss = zipwith (Cell )

zippedPossibles :: Game -> [(Cell, [Int])]
zippedPossibles game = zip game (possiblesForAllCells game)



possiblesForCells :: Game -> [Cell] -> [Int]
possiblesForCells game cells = foldr (\x a -> union a (possiblesForCell game x)) [] cells

-- let g = emptyGame
-- let g' = initvalues g 
-- let inits = [5,0,0, 0,0,6, 0,7,0,  0,4,0, 8,0,0, 0,0,0,  0,8,1, 4,0,5, 0,0,2,  0,0,0, 0,0,0, 7,0,0,  7,0,2, 0,0,0, 4,0,9,  0,0,4, 0,0,0, 0,0,0,  4,0,0, 6,0,1, 3,5,0,   0,0,0, 0,0,4, 0,6,0,  0,3,0, 5,0,0, 0,0,8] :: [Int]

