-- This problem was asked by Amazon.
-- 
-- An sorted array of integers was rotated an unknown number of times.
-- 
-- Given such an array, find the index of the element in the array in faster 
-- than linear time. If the element doesn't exist in the array, return null.
-- 
-- For example, given the array [13, 18, 25, 2, 8, 10] and the element 8, 
-- return 4 (the index of 8 in the array).
-- 
-- You can assume all the integers in the array are unique.

splitList :: (Ord a, Eq a) => [a] -> a -> Int -> Maybe Int
splitList [] _ _ = Nothing
splitList (a:[]) elem off = do
    case elem == a of 
        True -> return off
        False -> Nothing
splitList as elem off = do
    let (f, l) = (head as, last as)
    if f < l
        then if (f <= elem) && (elem <= l) 
                then binSearch as elem off
                else Nothing
        else do
            let leftRes = splitList leftList elem off
                rightRes = splitList rightList elem (off + midPoint)
                (leftList, rightList) = splitAt midPoint as
                midPoint = (length as) `div` 2
            case leftRes of 
                Nothing -> case rightRes of
                                Nothing -> Nothing
                                (Just v) -> return v
                (Just m) -> return m

binSearch :: (Ord a, Eq a) => [a] -> a -> Int -> Maybe Int
binSearch [] _ _ = Nothing
binSearch (a:[]) elem off = if a == elem then Just off else Nothing
binSearch as elem off = if elem == midElem
                            then (Just (off + midPoint))
                            else if inLeft
                                    then binSearch leftHalf elem off
                                    else binSearch rightHalf elem (off + midPoint)
    where
        midPoint = (length as) `div` 2
        midElem = as !! midPoint
        inLeft = elem < midElem
        (leftHalf, rightHalf) = splitAt midPoint as

test1 :: IO ()
test1 = do
    let ts = [ 13, 18, 25, 2, 8, 10 ]
        res = splitList ts 8 0
    putStrLn $ show res

main :: IO ()
main = do
    test1
