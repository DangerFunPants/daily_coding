#!/usr/bin/env runhaskell

-- The flight itinerary problem is as follows:
--
-- Given an unordered list of flights taken by someone, each represented as
-- (origin, destination) pairs, and a starting airport, compute the person's
-- itinerary. If no such itinerary exists, return null. All flights must be
-- used in the itinerary.
--
-- For example, given the following list of flights:
--
--     HNL ➔ AKL
--     YUL ➔ ORD
--     ORD ➔ SFO
--     SFO ➔ HNL
--
-- and starting airport YUL, you should return YUL ➔ ORD ➔ SFO ➔ HNL ➔
-- AKL. (This also happens to be the actual itinerary for the trip I'm about to
-- take.)

import Control.Monad.State

type IState = State [String]

addCity :: String -> IState ()
addCity c = state $ \s -> ((), c:s)

findItinerary' :: [(String, String)] -> String -> IState Bool
findItinerary' [] loc = do
    addCity loc
    return True
findItinerary' fs loc = do
    let possible = filter (\(st, _) -> st == loc) fs
    bs <- mapM (\(st, end) -> findItinerary' (filter (/= (st, end)) fs) end) possible
    let ts = filter id bs
    if (null ts)
        then return False
        else do
            addCity loc
            return True

findItinerary :: [(String, String)] -> String -> (Bool, [String])
findItinerary [] loc = (True, [loc])
findItinerary fs loc = res
    where
        possible = filter (\(st, end) -> st == loc) fs
        bs = fmap (\(st, end) -> findItinerary (filter (/= (st, end)) fs) end) possible
        ts = filter (\(a, b) -> a) bs
        (b, dstList) = (head ts)
        res = if (null ts)
                then (False, [])
                else (True, (loc:dstList))

test1 :: IO ()
test1 = do
    let tst = [("hnl", "akl"), ("yul", "ord"), ("ord", "sfo"), ("sfo", "hnl")]
        res = findItinerary tst "yul"
    putStrLn $ show res

test2 :: IO ()
test2 = do
    let tst = [("hnl", "akl"), ("yul", "ord"), ("ord", "sfo"), ("sfo", "hnl")]
        res = runState (findItinerary' tst "yul") []
    putStrLn $ show res

main :: IO ()
main = do
    test2