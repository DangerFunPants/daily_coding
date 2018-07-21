#!/usr/bin/env runhaskell

-- This problem was asked by Microsoft.
-- 
-- Compute the running median of a sequence of numbers. That is, given a stream 
-- of numbers, print out the median of the list so far on each new element.
-- 
-- Recall that the median of an even-numbered list is the average of the two 
-- middle numbers.
-- 
-- For example, given the sequence [2, 1, 5, 7, 2, 0, 5], your algorithm should 
-- print out:
-- 
-- 2
-- 1.5
-- 2
-- 3.5
-- 2
-- 2
-- 2

data BST a 
    = Node a (BST a) (BST a) 
    | Nil
    deriving (Show, Eq)

insertNode :: Ord a => (BST a) -> a -> (BST a)
insertNode (Node v lc rc) v' = 
    case v' < v of 
        True -> Node v (insertNode lc v') rc
        False -> Node v lc (insertNode rc v')
insertNode (Nil) v' = Node v' Nil Nil

inOrder :: Ord a => (BST a) -> [a]
inOrder (Node v lc rc) = (inOrder lc)++[v]++(inOrder rc)
inOrder Nil = []

medianTraversal :: (Ord a, Num a, Integral a, Fractional b) => (BST a) -> b
medianTraversal tr = med
    where
        ns = inOrder tr
        med = case (odd (length ns)) of
                True -> (fromIntegral $ ns !! medInd)
                False -> (fromIntegral (sum [ (ns !! medInd), (ns !! (pred medInd)) ])) / (fromIntegral 2)
        medInd = (length ns) `div` 2

testTree :: (BST Int)
testTree = foldl (\t v -> insertNode t v) Nil [1..20]

-- This isn't actually any more efficient than the other implementation, needs linear time for each 
-- median calculation and as such turns out to be quadratic time. There is probably a way 
-- to find the median of a BST in lg time???
calcMedians :: (Ord a, Num a, Integral a, Fractional b) => [a] -> [b]
calcMedians ls = (tail . reverse) (fmap snd (foldl foldMethod [(Nil, 0)] ls))
    where
        foldMethod ts@((t, _):rest) v' = newT
            where
                newT = (thisOne, med):ts
                thisOne = insertNode t v'
                med = medianTraversal thisOne

test1 :: IO ()
test1 = do
    let ts = [2, 1, 5, 7, 2, 0, 5]
        res = calcMedians ts
    putStrLn $ "Res for "++(show ts)++" : "++(show res)

main :: IO ()
main = do
    test1
