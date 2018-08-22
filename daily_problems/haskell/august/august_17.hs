-- This problem was asked by Google.
-- 
-- On our special chessboard, two bishops attack each other if they share the 
-- same diagonal. This includes bishops that have another bishop located 
-- between them, i.e. bishops can attack through pieces.
-- 
-- You are given N bishops, represented as (row, column) tuples on a M by M 
-- chessboard. Write a function to count the number of pairs of bishops that 
-- attack each other. The ordering of the pair doesn't matter: (1, 2) is 
-- considered the same as (2, 1).
-- 
-- For example, given M = 5 and the list of bishops:
-- 
--     (0, 0)
--     (1, 2)
--     (2, 2)
--     (4, 0)
-- 
-- The board would look like this:
-- 
-- [b 0 0 0 0]
-- [0 0 b 0 0]
-- [0 0 b 0 0]
-- [0 0 0 0 0]
-- [b 0 0 0 0]
-- 
-- You should return 2, since bishops 1 and 3 attack each other, as well as 
-- bishops 3 and 4.

import qualified Data.Set as S

countPairs ((i,j):bs) m = overlap + (countPairs bs m)
    where
        ds = [ (i+n,j+k) 
             | n<-[-m..m], k<-[-m..m]
             , ((i+n) <= m && (j+k) <= m)
             , ((i+n) > 0 && (j+k) > 0)
             , (abs n) == (abs k)
             ]
        overlap = length $ (S.fromList ds) `S.intersection` (S.fromList bs)
countPairs [] _ = 0

test1 :: IO ()
test1 = do
    let ts = [ [ (0, 0)
               , (1, 2)
               , (2, 2)
               , (4, 0)
               ]
             , [ (0, 0)
               , (1, 2)
               , (2, 2)
               , (4, 0)
	       , (0, 4)
               ]
	     ]
        mod = fmap (fmap (\(i,j) -> (succ i, succ j))) ts
        res = fmap (\m -> countPairs m 5) mod
    putStrLn $ show res

main :: IO ()
main = do
    putStrLn $ "Hola Mundas!"
