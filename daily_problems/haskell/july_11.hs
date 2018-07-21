#!/usr/bin/env runhaskell

-- This problem was asked by Google.
-- 
-- The edit distance between two strings refers to the minimum number of 
-- character insertions, deletions, and substitutions required to change one 
-- string to the other. For example, the edit distance between “kitten” and 
-- “sitting” is three: substitute the “k” for “s”, substitute the 
-- “e” for “i”, and append a “g”.
-- 
-- Given two strings, compute the edit distance between them.

import qualified Data.Map as M
import Control.Monad.State

type MapST k v = State (M.Map k v)

insMap :: Ord k => k -> v -> (MapST k v) ()
insMap k v = state $ \s -> ((), (M.insert k v s))

readMap :: Ord k => k -> (MapST k v) (Maybe v)
readMap k = state $ \s -> ((M.lookup k s), s)

editDistance :: String -> String -> (MapST (String, String) Int) Int
editDistance lt@(l:ls) rt@(r:rs) = do
    v <- readMap ((l:ls), (r:rs))
    case v of
        Just v -> (return v)
        Nothing -> do
            if (l == r)
                then do
                    res <- editDistance ls rs
                    insMap (lt, rt) res
                    return res
                else do
                    res <- mapM (uncurry editDistance) [ (lRep, rRep), (lDel, rDel), (lIns, rIns) ]
                    let m = minimum res
                    insMap ((l:ls), (r:rs)) (succ m)
                    return (succ m)
    where
        lRep = ls
        rRep = rs
        lDel = ls 
        rDel = (r:rs)
        lIns = (l:ls)
        rIns = rs
editDistance l r = return $ sum (fmap length [l, r])

test :: (String, String) -> IO ()
test t@(l, r) = do
    let (res, st) = runState (editDistance l r) M.empty
    putStrLn $ "Edit distance for: "++(show t)++" is: "++(show res)
    putStrLn $ "State: "++(show $ length st)

main :: IO () 
main = do
    let ts = [ ("kitten", "sitting")
             , ("then", "hen")
             , ("saturday", "sunday")
             , ("acd", "abcd")
             , ("abcd", "acd")
             , ("abcde", "abde")
             , ("intention", "execution")
             , ("abcdefghijklmnopqrstuvwxyz", (reverse "abcdefghijklmnopqrstuvwxyz"))
             ]
    mapM test ts
    return ()

