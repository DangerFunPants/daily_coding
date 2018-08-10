-- This problem was asked by Dropbox.
-- 
-- Sudoku is a puzzle where you're given a partially-filled 9 by 9 grid with 
-- digits. The objective is to fill the grid with the constraint that every 
-- row, column, and box (3 by 3 subgrid) must contain all of the digits from 1 
-- to 9.
-- 
-- Implement an efficient sudoku solver.

import Control.Monad.State
import Control.Lens
import Data.List

type Board = [[Int]]

type BoardST = State (Board, Board)

testBoard :: Board
testBoard = [ [ 5, 3, 0, 0, 7, 0, 0, 0, 0 ]
            , [ 6, 0, 0, 1, 9, 5, 0, 0, 0 ]
            , [ 0, 9, 8, 0, 0, 0, 0, 6, 0 ]
            , [ 8, 0, 0, 0, 6, 0, 0, 0, 3 ]
            , [ 4, 0, 0, 8, 0, 3, 0, 0, 1 ]
            , [ 7, 0, 0, 0, 2, 0, 0, 0, 6 ]
            , [ 0, 6, 0, 0, 0, 0, 2, 8, 0 ]
            , [ 0, 0, 0, 4, 1, 9, 0, 0, 5 ]
            , [ 0, 0, 0, 0, 8, 0, 0, 7, 9 ]
            ]

markSq :: Int -> Int -> Int -> BoardST ()
markSq i j n = state $ \(s, b) -> ((), (s & (element i . element j .~ n), b))

readSq :: Int -> Int -> BoardST Int
readSq i j = do
    r <- getRow i
    return $ r !! j

getRow :: Int -> BoardST [Int]
getRow i = state $ \(s, b) -> ((s !! i), (s, b))

getCol :: Int -> BoardST [Int]
getCol j = state $ \(s, b) -> ((fmap (\x -> x !! j) s), (s, b))

getQuad :: Int -> Int -> BoardST Board
getQuad i j = do
    let startRow = 3 * (i `div` 3)
        endRow = startRow + 2
        startCol = 3 * (j `div` 3)
        endCol = startCol + 2
    rows <- mapM getRow [startRow..endRow]
    let cs = fmap (filter (\(i, v) -> i `elem` [startCol..endCol])) (fmap (zip [0..]) rows) 
        cs' = (fmap . fmap) snd cs
    return cs'

validForRow :: Int -> BoardST [Int]
validForRow i = do
    r <- getRow i
    return $ filter (\x -> not (elem x r)) [1..9]

validForCol :: Int -> BoardST [Int]
validForCol j = do
    c <- getCol j
    return $ filter (\y -> not (elem y c)) [1..9]

validForQuad :: Int -> Int -> BoardST [Int]
validForQuad i j = do
    q <- getQuad i j
    let q' = concat q
    return $ filter (\x -> not (elem x q')) [1..9]

validMarks :: Int -> Int -> BoardST [Int]
validMarks i j = do
    r <- validForRow i
    c <- validForCol j
    q <- validForQuad i j
    let valid = (intersect . intersect r) c q
    return $ valid

setSolution :: BoardST ()
setSolution = state $ \(s, b) -> ((), (s, s))

getNext :: Int -> Int -> (Int, Int)
getNext i j
    | i < 8 = ((succ i), j)
    | otherwise = (0, (succ j))

solvePuzzle :: (Int, Int) -> BoardST Bool
solvePuzzle (0, 9) = do
    setSolution
    return True
solvePuzzle (i, j) = do
    v <- readSq i j
    if v == 0
        then do
            vs <- validMarks i j
            if (null vs)
                then return False
                else do
                    bs <- mapM loopFn vs
                    return True
        else solvePuzzle (getNext i j)
    where
        loopFn mk = do
            markSq i j mk
            b <- solvePuzzle (getNext i j)
            markSq i j 0
            return b

main :: IO ()
main = do
    let (a, (s, b)) = runState (solvePuzzle (0, 0)) (testBoard, [])
    mapM (putStrLn . show) b
    return ()

