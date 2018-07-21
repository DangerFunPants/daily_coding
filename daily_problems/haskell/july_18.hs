-- This problem was asked by Microsoft.
-- 
-- You have an N by N board. Write a function that, given N, returns the number 
-- of possible arrangements of the board where N queens can be placed on the 
-- board without threatening each other, i.e. no two queens share the same row, 
-- column, or diagonal.

checkBoard :: [[Int]] -> Int -> Int -> Bool
checkBoard b i j = res
    where
        ts = [ (a, b) | a <- [0..(pred . length) b], b <- [0..(pred . length) b] ]
        row = filter (\(x, _) -> x == i) ts
        col = filter (\(_, y) -> y == j) ts
        diag = filter (\(a, b) -> (a + b) == (i + j) || (a - b) == (i - j)) ts
        attackers = concat [row,col,diag]
        invalid = fmap (\(x, y) -> (b !! x !! y) == 1) attackers
        res = not (True `elem` invalid)

-- This implementation results in ~ O(N^N) complexity 
nQueens :: Int -> Int -> [[Int]] -> Int
nQueens n i b
    | n == i = 1
    | otherwise = res
    where
        -- try to place a queen in the i_th row of the board
        valids = filter (checkBoard b i) [0..(pred n)]
        recCalls = fmap (\v -> nQueens n (succ i) (newBoard v)) valids
        newBoard v = [ [ if (x,y) == (i, v) then 1 else (b !! x !! y) | y <- [0..(pred n)] ] | x <- [0..(pred n)] ]
        res = sum recCalls

eliminate :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
eliminate (i,j) vs = diags
    where
        rc = filter (\(x,y) -> not ((x == i) || (y == j))) vs
        diags = filter (\(x,y) -> not ((x + y) == (i + j) || (x - y) == (i - j))) rc
        res = diags

-- This implementation certainly seems faster from an anecdotal perspective but I think 
-- that the upper bound on the complexity is still an exponential...
nQueens' :: Int -> Int -> [(Int, Int)] -> Int
nQueens' n i vs
    | n == i = 1
    | vs == [] = 0
    | otherwise = res
        where
            possibles = filter (\(x, _) -> x == i) vs
            recCall = fmap (\(x, y) -> nQueens' n (succ i) (newBoard (x,y))) possibles
            newBoard (x,y) = eliminate (x,y) vs
            res = sum recCall
            

mkBoard n = [ [ 0 | _ <- [0..(pred n)] ] | _ <- [0..(pred n)] ]

mkBoard' n = [ (x, y) | x <- [0..(pred n)], y <- [0..(pred n)] ]

test1 :: IO ()
test1 = do
    let ts = [ 4..10 ]
        res = fmap (\i -> nQueens i 0 (mkBoard i)) ts
    putStrLn $ "N queens solutions: "++show (zip ts res)

test2 :: IO ()
test2 = do
    let ts = [ 4..10 ]
        res = fmap (\i -> nQueens' i 0 (mkBoard' i)) ts
    putStrLn $ "N queens solutions: "++show (zip ts res)

main :: IO ()
main = do
    test1
    test2
