-- This problem was asked by Apple.
-- 
-- Given a tree, find the largest tree/subtree that is a BST.
-- 
-- Given a tree, return the size of the largest tree/subtree that is a BST.

import Text.Show.Pretty
import Data.List (sortOn)

data BTree a
    = Node a (BTree a) (BTree a)
    | Nil
    deriving (Show, Eq)

instance Foldable BTree where
    -- foldMap :: (Monoid m) => (a -> m) -> t a -> m
    foldMap f (Node v lc rc) = foldMap f lc `mappend` f v `mappend` foldMap f rc
    foldMap _ Nil = mempty

-- At each node there are 3 possibilities
--  1. Including the root (current) node yields the largest tree
--  2. The left node yields the largest tree
--  3. The right node yields the largest tree
findSubtree (Node v lc rc) f = (last . (sortOn length)) [ leftTree, rightTree, keepRoot ]
    where
        leftTree = findSubtree lc (pure True)
        rightTree = findSubtree rc (pure True)
        keepRoot = 
            if f v
                then Node v (findSubtree' lc (<v)) (findSubtree' rc (>=v))
                else Nil
findSubtree Nil _ = Nil

-- If the node can be included in the current tree,
-- then recurse.
-- Otherwise return Nil
-- Essentially given the current node, build the largest BST possible 
-- that includes the current node. 
findSubtree' Nil _ = Nil
findSubtree' (Node v lc rc) f = 
    if f v
        then Node v (findSubtree' lc (<v)) (findSubtree' rc (>=v))
        else Nil

test1 :: IO ()
test1 = do
    let ts = Node 5
                (Node 3
                    (Node 2 Nil Nil)
                    (Node 4 Nil Nil))
                (Node 10
                    (Node 7 Nil Nil)
                    (Node 11 Nil Nil))
        res = findSubtree ts (pure True)
    putStrLn $ ppShow res

main :: IO ()
main = test1
