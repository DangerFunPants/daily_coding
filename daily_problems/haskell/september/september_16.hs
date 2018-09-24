-- This problem was asked by Coursera.
-- 
-- Given a 2D board of characters and a word, find if the word exists in the 
-- grid.
-- 
-- The word can be constructed from letters of sequentially adjacent cell, 
-- where "adjacent" cells are those horizontally or vertically neighboring. The 
-- same letter cell may not be used more than once.
-- 
-- For example, given the following board:
-- 
-- [
--   ['A','B','C','E'],
--   ['S','F','C','S'],
--   ['A','D','E','E']
-- ]
-- 
-- exists(board, "ABCCED") returns true, exists(board, "SEE") returns true, 
-- exists(board, "ABCB") returns false.

import Text.Show.Pretty (ppShow)

findWord' :: (Int, Int) -> [String] -> String -> [(Int, Int)] -> Bool
findWord' _ _ [] _ = True
findWord' (p_i, p_j) ws (x:xs) used = foldl (\v acc -> acc || v) False res
    where
        nextWs = [ (i, j) | (i, j) <- [ (p_i + 1, p_j) 
                                      , (p_i, p_j - 1)
                                      , (p_i - 1, p_j)
                                      , (p_i, p_j + 1)
                                      ]
                                      , i >= 0, i <= mag_i
                                      , j >= 0, j <= mag_j
                                      , not ((i, j) `elem` used) ]
        mag_j = (pred . length . head) ws
        mag_i = (pred . length) ws
        cs = [ (i, j) | (i, j) <- nextWs, (ws !! i !! j) == x ]
        res = fmap (\(i, j) -> findWord' (i, j) ws xs ((i, j):used)) cs


findWord (x:xs) ws = foldl (\v acc -> v || acc) False all
    where
        ts = [ (i, j) | i <- [0..mag_i], j <- [0..mag_j], (ws !! i !! j) == x ]
        all = fmap (\t -> findWord' t ws xs [t]) ts
        mag_j = (pred . length . head) ws
        mag_i = (pred . length) ws

test1 :: IO ()
test1 = do
    let ts = [ "ABCE"
             , "SFCS"
             , "ADEE"
             ]
        search = [ "ABCCED"
                 , "SEE"
                 , "ABCB"
                 ]
        res = fmap (\w -> findWord w ts) search
    putStrLn $ show res

main :: IO ()
main = test1
