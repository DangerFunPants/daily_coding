-- This question was asked by Google.
-- 
-- Given an integer n and a list of integers l, write a function that randomly 
-- generates a number from 0 to n-1 that isn't in l (uniform).

import System.Random
import Text.Show.Pretty

genRandom' :: (Random a, Num a) => a -> IO a
genRandom' n = getStdRandom $ randomR (0, n)

genRandom :: Int -> [Int] -> IO Int
genRandom n l = do
    r <- genRandom' n
    if r `elem` l
        then genRandom n l
        else return r

test1 :: IO ()
test1 = do
    let trialCount = 100000
    vs <- mapM (\_ -> genRandom 10 [2, 3]) [1..trialCount]
    let hist = fmap (\v -> (v, ((fromIntegral . length) $ filter (==v) vs) / trialCount))  [0..10]
    putStrLn $ ppShow hist

main :: IO ()
main = test1
