-- This problem was asked by Google.
-- 
-- A knight's tour is a sequence of moves by a knight on a chessboard such that 
-- all squares are visited once.
-- 
-- Given N, write a function to return the number of knight's tours on an N by 
-- N chessboard.

tour :: (Int, Int) -> [(Int, Int)] -> Int
tour _ [] = 1
tour (ci,cj) toVisit = sum recCall
    where
        nextMove = [ (ci+2,cj+1)
                   , (ci+2,cj-1)
                   , (ci-2,cj+1)
                   , (ci-2,cj-1)
                   , (ci+1,cj+2)
                   , (ci+1,cj-2)
                   , (ci-1,cj+2)
                   , (ci-1,cj-2)
                   ]
        validMoves = filter (\e -> e `elem` toVisit) nextMove
        recCall = fmap (\p -> tour p (filter (\p' -> (p' /= p)) toVisit)) validMoves

nTour :: Int -> [Int]
nTour n = fmap (\t -> tour t [(i,j) | i<-[1..n],j<-[1..n], (i,j) /= t]) [(i,j) | i<-[1..n], j<-[1..n]]

