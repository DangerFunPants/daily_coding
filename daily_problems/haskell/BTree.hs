module BTree where

data BTree a
    = Node a (BTree a) (BTree a)
    | Leaf a
    deriving (Show, Eq)

instance Functor (BTree) where
    fmap f (Node a lc rc) = Node (f a) (fmap f lc) (fmap f rc)
    fmap f (Leaf a) = (Leaf (f a))

instance Foldable BTree where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Node a lc rc) = ((f a) `mappend` leftM) `mappend` rightM
        where
            rightM = foldMap f rc
            leftM = foldMap f lc
    foldMap f (Leaf a) = f a

