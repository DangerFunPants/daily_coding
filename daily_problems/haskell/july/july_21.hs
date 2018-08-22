
-- This problem was asked by Facebook.
-- 
-- Given an unordered list of flights taken by someone, each represented as 
-- (origin, destination) pairs, and a starting airport, compute the person's 
-- itinerary. If no such itinerary exists, return null. If there are multiple 
-- possible itineraries, return the lexicographically smallest one. All flights 
-- must be used in the itinerary.
-- 
-- For example, given the list of flights [('SFO', 'HKO'), ('YYZ', 'SFO'), 
-- ('YUL', 'YYZ'), ('HKO', 'ORD')] and starting airport 'YUL', you should 
-- return the list ['YUL', 'YYZ', 'SFO', 'HKO', 'ORD'].
-- 
-- Given the list of flights [('SFO', 'COM'), ('COM', 'YYZ')] and starting 
-- airport 'COM', you should return null.
-- 
-- Given the list of flights [('A', 'B'), ('A', 'C'), ('B', 'C'), ('C', 'A')] 
-- and starting airport 'A', you should return the list ['A', 'B', 'C', 'A', 
-- 'C'] even though ['A', 'C', 'A', 'B', 'C'] is also a valid itinerary. 
-- However, the first one is lexicographically smaller.

import Data.List (sort)

findItinerary :: [(String, String)] -> String -> Maybe [String]
findItinerary [] loc = return [loc]
findItinerary flights loc = do
    let possible = filter (\(dep, arr) -> dep == loc) flights
    recCall <- mapM mapFn possible
    if (null recCall)
        then fail "No itinerary found"
        else return $ loc:((head . sort) recCall) 
    where
        mapFn (dep, arr) = findItinerary flights' arr
            where
                flights' = filter (\t -> (dep,arr) /= t) flights

test1 :: IO ()
test1 = do
    let ts = [ [ ("sfo", "hko"), ("yyz", "sfo"), ("yul", "yyz"), ("hko", "ord") ]
             , [ ("sfo", "com"), ("com", "yyz") ]
             , [ ("a", "b"), ("a", "c"), ("b","c"),("c","a") ]
             ]
        res =  [ (findItinerary (ts !! 0) "yul")
               , (findItinerary (ts !! 1) "com")
               , (findItinerary (ts !! 2) "a") 
               ]
    mapM (putStrLn . show) res
    return ()

main :: IO ()
main = do
    test1
