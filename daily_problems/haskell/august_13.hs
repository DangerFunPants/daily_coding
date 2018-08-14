-- This problem was asked by Amazon.
-- 
-- Given a N by M matrix of numbers, print out the matrix in a clockwise spiral.
-- 
-- For example, given the following matrix:
-- 
-- [[1,  2,  3,  4,  5],
--  [6,  7,  8,  9,  10],
--  [11, 12, 13, 14, 15],
--  [16, 17, 18, 19, 20]]
-- 
-- You should print out the following:
-- [ 1, 2, 3, 4, 5
-- , 10, 15, 20, 19, 18
-- , 17, 16, 11, 6, 7
-- , 8, 9, 14, 13, 12 
-- ]


data Dirs
    = DUp
    | DDown
    | DLeft
    | DRight

mkPairs _ _ [] = []
mkPairs d p@(ci, cj) ps = nextP:(mkPairs nextD nextP nextPs)
    where
        (nextP, nextD) = nxt d p ps 
        nextPs = filter (/= nextP) ps

nxt :: Dirs -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), Dirs)
nxt DUp (ci, cj) ps = 
    case (ci+1,cj) `elem` ps of
        True -> ((ci+1, cj), DUp)
        False -> nxt DRight (ci, cj) ps
nxt DDown (ci, cj) ps = 
    case (ci-1,cj) `elem` ps of
        True -> ((ci-1,cj), DDown)
        False -> nxt DLeft (ci,cj) ps
nxt DLeft (ci, cj) ps = 
    case (ci,cj-1) `elem` ps of
        True -> ((ci,cj-1), DLeft)
        False -> nxt DUp (ci,cj) ps
nxt DRight (ci, cj) ps = 
    case (ci,cj+1) `elem` ps of 
        True -> ((ci,cj+1), DRight)
        False -> nxt DDown (ci,cj) ps

spiral n m mat = fmap (\(i,j) -> (mat !! i) !! j) pairs
    where
        pairs = mkPairs DRight (0,-1) [(i,j) | i<-[0..(m-1)],j<-[0..(n-1)]]

test1 :: IO ()
test1 = do
    let ts = [ [1,  2,  3,  4,  5]
             , [6,  7,  8,  9,  10]
             , [11, 12, 13, 14, 15]
             , [16, 17, 18, 19, 20]
             ]
        res = spiral 5 4 ts
    putStrLn $ show res

main :: IO ()
main = test1

