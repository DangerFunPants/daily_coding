-- This question was asked by ContextLogic.
-- 
-- Implement division of two positive integers without using the division, 
-- multiplication, or modulus operators. Return the quotient as an integer, 
-- ignoring the remainder.

-- div' :: Int -> Int -> Int -> Int -> Int
-- div' acc c a b = 
--     if acc > a
--         then c
--         else div' (acc + b) (succ c) a b

div' :: Int -> Int -> Int -> Int
div' acc a b = 
    if a < 0
        then acc
        else div' (succ acc) (a - b) b

divide :: Int -> Int -> Int
divide = div' (-1)

test1 :: IO ()
test1 = do
    let res = 121 `divide` 11 
    putStrLn $ show res

main :: IO ()
main = test1
