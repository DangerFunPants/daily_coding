#!/usr/bin/env runhaskell


-- This problem was asked by Google.
--
-- Suppose we represent our file system by a string in the following manner:
--
-- The string "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext" represents:
--
-- dir
--     subdir1
--     subdir2
--         file.ext
--
-- The directory dir contains an empty sub-directory subdir1 and a
-- sub-directory subdir2 containing a file file.ext.
--
-- The string
-- "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t
-- \t\tfile2.ext" represents:
--
-- dir
--     subdir1
--         file1.ext
--         subsubdir1
--     subdir2
--         subsubdir2
--             file2.ext
--
-- The directory dir contains two sub-directories subdir1 and subdir2. subdir1
-- contains a file file1.ext and an empty second-level sub-directory
-- subsubdir1. subdir2 contains a second-level sub-directory subsubdir2
-- containing a file file2.ext.
--
-- We are interested in finding the longest (number of characters) absolute
-- path to a file within our file system. For example, in the second example
-- above, the longest absolute path is "dir/subdir2/subsubdir2/file2.ext", and
-- its length is 32 (not including the double quotes).
--
-- Given a string representing the file system in the above format, return the
-- length of the longest absolute path to a file in the abstracted file system.
-- If there is no file in the system, return 0.
--
-- Note:
--
-- The name of a file contains at least a period and an extension.
--
-- The name of a directory or sub-directory will not contain a period.

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Control.Monad

data Files
    = Directory String [Files]
    | File String
    deriving (Show)

-- Try to define some of the so-called "leaves" of our grammar
path_list = whole_path
whole_path = endBy pair (char '\n')
pair = do
    tabs <- (try (many (char '\t')))
    name <- pathName
    return ((length tabs), name)
pathName = many1 (choice [letter, oneOf "_.", digit])

parsePath :: String -> Either ParseError [(Int, String)]
parsePath inp = parse path_list "My non-descript error message" inp

procList :: [(Int, Int, String)] -> [Int] -> Int -> Int -> [(Int, Int, String)]
procList ((id, lvl, str):rest) parents prevLvl prevId
    | lvl == prevLvl = (id, (head parents), str):(procList rest parents lvl id)
    | lvl > prevLvl = (id, prevId, str):(procList rest (prevId:parents) lvl id)
    | lvl < prevLvl = (id, newParent, str):(procList rest newParentList lvl id)
    where
        newParentList = drop (prevLvl - lvl) parents
        newParent = head newParentList
procList [] _ _ _ = []

buildGraph :: (Int, Int, String) -> [(Int, Int, String)] -> Files
buildGraph (id, p, str) nodes = files
    where
        children = filter (\(_, ps, _) -> ps == id) nodes -- Direct descendants.
        recChild = fmap (\c -> buildGraph c nodes) children 
        files = if '.' `elem` str 
                    then File str -- Implicit assumption that files can't have children
                    else Directory str recChild

bfsMax :: Files -> [String]
bfsMax (Directory name cs) = newPaths
    where
        subPs = concat $ fmap (bfsMax) cs
        newPaths = fmap (\s -> (name++"/"++s)) subPs
bfsMax (File name) = [name]

main :: IO ()
main = do
    let (Right res) = parsePath "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext\n"
        graphRep = fmap (\((lvl, str), v) -> (v, lvl, str)) (zip res [1..(length res)])
        strs = fmap (\p -> (replicate (fst p) '\t')++(snd p)) res
        processed = procList graphRep [0] (-1) (0)
        g = buildGraph (0,0,"") processed
        m = bfsMax g

    putStrLn $ show m
    putStrLn $ show (maximum (fmap length m))