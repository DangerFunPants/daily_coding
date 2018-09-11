-- This problem was asked by Airbnb.
-- 
-- We're given a hashmap with a key courseId and value a list of courseIds, 
-- which represents that the prerequsite of courseId is courseIds. Return a 
-- sorted ordering of courses such that we can finish all courses.
-- 
-- Return null if there is no such ordering.
-- 
-- For example, given {'CSC300': ['CSC100', 'CSC200'], 'CSC200': ['CSC100'], 
-- 'CSC100': []}, should return ['CSC100', 'CSC200', 'CSCS300'].

import qualified Data.Map as M
import Data.List (delete)

getEmpty :: M.Map String [String] -> Maybe String
getEmpty m = 
    if null res
        then Nothing
        else Just $ (fst . head) res
    where
        res = M.toList $ M.filter null m 

findOrder :: M.Map String [String] -> Maybe [String]
findOrder m = do
    n <- getEmpty m
    let newMap = fmap (\v -> delete n v) (M.delete n m)
    if null newMap
        then return [n]
        else do
            recCall <- findOrder newMap
            return $ [n] ++ recCall

test1 :: IO ()
test1 = do
    let ts = M.fromList [ ("csc300", [ "csc100", "csc200" ] )
                        , ("csc200", [ "csc100" ] )
                        , ("csc100", [ ] )
                        ]
        res = findOrder ts 
    putStrLn $ show res

main :: IO ()
main = test1
