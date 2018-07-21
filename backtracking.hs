#!/usr/bin/env runhaskell

import Control.Monad.State
import Control.Lens

type Matrix = [[Int]]
type Board = State (Matrix, [Matrix])

mkBoard :: Int -> Matrix
mkBoard n = [[0 | i <- [1..n]] | j <- [1..n]]

getE :: Int -> Int -> Board Int
getE i j = state $ \(s, ss) -> (((s !! i) !! j), (s, ss))

setE :: Int -> Int -> Int -> Board ()
setE i j v = state $ \(s, ss) -> ((), (s & (element i . element j .~ v), ss))

getN :: Board Int
getN = state $ \(s, ss) -> ((length s), (s, ss))

getRow :: Int -> Board [Int]
getRow i = state $ \(s, ss) -> ((s !! i), (s, ss))

getCol :: Int -> Board [Int]
getCol j = state $ \(s, ss) -> ((fmap (!! j) s), (s, ss))

getDiag :: Int -> Int -> Int -> Board [Int]
getDiag i j n = 
    state $ \(s, ss) -> 
        let ds = fmap (\(a, b) -> (s !! a) !! b) (diag)
        in (ds, (s, ss))
    where
        diag = filter (\(a, b) -> (a + b) == (i + j) || (a - b) == (i - j)) ts
        ts = [ (a, b) | a <- [0..(pred n)], b <- [0..(pred n)] ]

addValidBoard :: Board ()
addValidBoard = state $ \(s, ss) -> ((), (s, (s:ss)))

checkBoard :: Int -> Int -> Board Bool
checkBoard i j = do
    n <- getN
    r <- getRow i
    c <- getCol j
    d <- getDiag i j n
    if True `elem` (fmap (elem 1) [r, c, d])
        then return False
        else return True
    
nQueens :: Int -> Board [Bool]
nQueens i = do
    n <- getN
    if i == n
        then do
            addValidBoard
            return [True]
        else do
            viable <- mapM (checkBoard i) [0..(pred n)]
            if (True `elem` viable)
                then
                    do
                        let vs = fmap fst $ filter (\(i, v) -> v) (zip ([0..] :: [Int]) viable)
                        bs <- mapM loopFn vs
                        return (concat bs)
                else (return [False])
    where 
        loopFn j = do
            setE i j 1
            b <- nQueens (succ i)
            setE i j 0
            return b

main :: IO ()
main = do
    let (a, s) = runState (nQueens 0) ((mkBoard 6), [])
    -- putStrLn $ "found: "++(show a)
    (mapM . mapM) (putStrLn . show) (snd s)
    return ()
