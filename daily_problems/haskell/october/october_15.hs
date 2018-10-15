-- This problem was asked by Microsoft.
-- 
-- Let's represent an integer in a linked list format by having each node 
-- represent a digit in the number. The nodes make up the number in reversed 
-- order.
-- 
-- For example, the following linked list:
-- 
-- 1 -> 2 -> 3 -> 4 -> 5
-- 
-- is the number 54321.
-- 
-- Given two linked lists in this format, return their sum in the same linked 
-- list format.
-- 
-- For example, given
-- 
-- 9 -> 9
-- 
-- 5 -> 2
-- 
-- return 124 (99 + 25) as:
-- 
-- 4 -> 2 -> 1

sumLists n k = reverse . show $ (f n) + (f k)
    where
        f = (read . reverse) :: (String -> Int)

test1 :: IO ()
test1 = do
    let n = "99"
        k = "52"
        res = sumLists n k
    putStrLn $ show res

main :: IO ()
main = test1
