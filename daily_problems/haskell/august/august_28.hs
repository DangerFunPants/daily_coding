-- This problem was asked by Facebook.
-- 
-- Given an array of integers, write a function to determine whether the array 
-- could become non-decreasing by modifying at most 1 element.
-- 
-- For example, given the array [10, 5, 7], you should return true, since we 
-- can modify the 10 into a 1 to make the array non-decreasing.
-- 
-- Given the array [10, 5, 1], you should return false, since we can't modify 
-- any one element to get a non-decreasing array.

-- Obvious by strong induction that:
-- i[0] < i[1] && (i[k] < i[k+1]) -> (i[k+1] < i[k+2]) -> forall n i[n] < i[n+1]
--

-- Check if there exists at most one instance of an index
-- such that i[n] > i[n+1].
--
-- suppose that there exists one instance, then change i[n] to be <= i[n+1]
nonDecreasing :: (Ord a) => [a] -> Bool
nonDecreasing (x:xs) = nonDecreasing' 0 x xs
nonDecreasing [] = True

nonDecreasing' 2 _ _ = False
nonDecreasing' _ _ [] = True
nonDecreasing' c l (x:xs) = 
    if l <= x
        then nonDecreasing' c x xs
        else nonDecreasing' (succ c) x xs

test1 :: IO ()
test1 =
    let ts = [ [ 10, 5, 7 ]
             , [ 10, 5, 1 ]
             , [ 4, 2, 3 ]
             , [ 4, 2, 1 ]
             , [ 2, 2, 3, 2, 4 ]
             ]
        expected = [ True, False, True, False, True ]
        res = zip expected (nonDecreasing <$> ts)

    in putStrLn $ show res

main :: IO ()
main = test1
