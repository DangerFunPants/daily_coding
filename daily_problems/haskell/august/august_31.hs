-- This problem was asked Microsoft.
-- 
-- Using a read7() method that returns 7 characters from a file, implement 
-- readN(n) which reads n characters.
-- 
-- For example, given a file with the content “Hello world”, three read7() 
-- returns “Hello w”, “orld” and then “”.

import GHC.IO.Handle.FD (stdin)
import System.IO

read7 :: Handle -> IO String
read7 h = mapM (\_ -> hGetChar h) [1..7]

readN :: Handle -> Int -> IO String
readN h c = stream >>= \v -> return $ take c v
    where
        readCount = ceiling $ (fromIntegral c) / (fromIntegral 7)
        stream = foldl (\s _ -> read7 h `mappend` s) (return "") [1..readCount]

test1 :: IO ()
test1 = do
    v <- readN stdin 10
    putStrLn $ show v

main :: IO ()
main = test1
