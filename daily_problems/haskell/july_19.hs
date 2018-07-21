
-- This problem was asked by Dropbox.
-- 
-- Conway's Game of Life takes place on an infinite two-dimensional board of 
-- square cells. Each cell is either dead or alive, and at each tick, the 
-- following rules apply:
-- 
--     Any live cell with less than two live neighbours dies.
--     Any live cell with two or three live neighbours remains living.
--     Any live cell with more than three live neighbours dies.
--     Any dead cell with exactly three live neighbours becomes a live cell.
-- 
-- A cell neighbours another cell if it is horizontally, vertically, or 
-- diagonally adjacent.
-- 
-- Implement Conway's Game of Life. It should be able to be initialized with a 
-- starting list of live cell coordinates and the number of steps it should run 
-- for. Once initialized, it should print out the board state at each step. 
-- Since it's an infinite board, print out only the relevant coordinates, i.e. 
-- from the top-leftmost live cell to bottom-rightmost live cell.
-- 
-- You can represent a live cell with an asterisk (*) and a dead cell with a 
-- dot (.).

import Data.List

type Board = [(Int, Int, Bool)]

countNs :: Board -> Int -> Int -> Int
countNs brd i j = livingNs
    where
        ns = [ (i + a, j + b) | a <- [-1..1], b <- [-1..1], (a,b) /= (0,0) ]
        lv = filter (\t -> brd `at` t) ns
        livingNs = length lv

at :: [(Int, Int, Bool)] -> (Int, Int) -> Bool
at brd (i, j) = res
    where
        fs = filter (\(x, y, _) -> (x, y) == (i, j)) brd
        res = if null fs
                then False
                else let (_, _, living) = head fs in living

updateOne :: (Int, Int, Bool) -> Int -> (Int, Int, Bool)
updateOne (i, j, v) nCount = newCell
    where
        living = if nCount < 2
                    then False
                    else if v && (nCount `elem` [ 2, 3 ])
                            then True
                            else if not v && nCount == 3
                                    then True
                                    else False
        newCell = (i, j, living)

getX (x, _, _) = x
getY (_, y, _) = y

allCells :: Board -> Board
allCells brd = allCs
    where
        xSort = sortOn (\(i, _, _) -> i) brd
        ySort = sortOn (\(_, j, _) -> j) brd
        minX = (getX (head xSort)) - 1
        maxX = (getX (last xSort)) + 1
        minY = (getY (head ySort)) - 1
        maxY = (getY (last ySort)) + 1
        allCs = [ (a, b, (brd `at` (a,b))) | a <- [minX..maxX], b <- [minY..maxY] ]

conway brd = filter (\(_,_,v) -> v) $ fmap (\t@(i, j, v) -> updateOne t (countNs brd i j)) allCs
    where
        allCs = allCells brd
         
main :: IO ()
main = do
    let start = [ (0,0,True), (1,0,True), (-1, 0, True) ]
        res = foldl (\b _ -> conway b) start [1..9]
    mapM (putStrLn . show) res
    return ()
