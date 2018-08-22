
-- This problem was asked by Google.
-- 
-- We can determine how "out of order" an array A is by counting the number of 
-- inversions it has. Two elements A[i] and A[j] form an inversion if A[i] > 
-- A[j] but i < j. That is, a smaller element appears after a larger element.
-- 
-- Given an array, count the number of inversions it has. Do this faster than 
-- O(N^2) time.
-- 
-- You may assume each element in the array is distinct.
-- 
-- For example, a sorted list has zero inversions. The array [2, 4, 1, 3, 5] 
-- has three inversions: (2, 1), (4, 1), and (4, 3). The array [5, 4, 3, 2, 1] 
-- has ten inversions: every distinct pair forms an inversion.

data BTree a 
    = Node a (BTree a) (BTree a)
    | Nil

bInsert :: (Ord a) => (BTree a) -> a -> ((BTree a), Int)
bInsert Nil a = ((Node a Nil Nil), 0)
bInsert (Node v lt rt) a = res
    where
        (tr, count) = case (a < v) of
                        True -> bInsert lt a
                        False -> bInsert rt a
        count' = case (a < v) of
                    True -> count + 1
                    False -> count
        res = case (a < v) of
                True -> ((Node v tr rt), count')
                False -> ((Node v lt tr), count')

inversions :: (Ord a, Num a) => (BTree a) -> [a] -> Int
inversions tr (a:as) = count + recCall
    where
        (tr', count) = bInsert tr a
        recCall = inversions tr' as
inversions _ [] = 0

test1 :: IO ()
test1 = do
    let ts = [2, 4, 1, 3, 5] 
        res = inversions Nil ts
    putStrLn $ "List: " ++ show ts ++ " Has: " ++ show res
        
main :: IO ()
main = do
    putStrLn $ "Hola Mundas!"
