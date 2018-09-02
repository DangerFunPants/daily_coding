-- This problem was asked by Amazon.
-- 
-- Given a matrix of 1s and 0s, return the number of "islands" in the matrix. A 
-- 1 represents land and 0 represents water, so an island is a group of 1s that 
-- are neighboring and their perimiter is surrounded by water.
-- 
-- For example, this matrix has 4 islands.
-- 
-- 1 0 0 0 0
-- 0 0 1 1 0
-- 0 1 1 0 0
-- 0 0 0 0 0
-- 1 1 0 0 1
-- 1 1 0 0 1

countIslands :: [(Int, Int)] -> Int
countIslands [] = 0
countIslands (p:ps) = 1 + (countIslands adj)
    where 
        adj = closure p ps

closure :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
closure p rest = recCall
    where
        -- find all points adj to the current point
        adjs = [ p' | p' <- rest, isAdj p p' ]
        -- now remove the adjacent points from the master list.
        remd = [ p' | p' <- rest, not (isAdj p p') ]
        -- find the closure of the list of the curent adjacent points
        -- under the adjacency relation.
        recCall = foldl (\l p -> closure p l) remd adjs


isAdj :: (Int, Int) -> (Int, Int) -> Bool
isAdj (x, y) p' = p' `elem` [ (x+i,y+j) | i<-[-1..1], j<-[-1..1], i/=0 || j/=0 ]

test1 :: IO ()
test1 = do
    let ts = [ [ 1, 0, 0, 0, 0 ]
             , [ 0, 0, 1, 1, 0 ]
             , [ 0, 1, 1, 0, 0 ]
             , [ 0, 0, 0, 0, 0 ]
             , [ 1, 1, 0, 0, 1 ]
             , [ 1, 1, 0, 0, 1 ]
             ]
        coords = [ (x, y) | x <- [0..4], y <- [0..5] ]
        ps = fmap fst $ filter (\(p, v) -> v == 1) (((zip coords) . concat) ts)
        res = countIslands ps
    putStrLn $ show res

main :: IO ()
main = test1
