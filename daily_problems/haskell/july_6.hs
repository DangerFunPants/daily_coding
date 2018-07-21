#!/usr/bin/env runhaskell

-- This problem was asked by Google.
--
-- Given a singly linked list and an integer k, remove the kth last element
-- from the list. k is guaranteed to be smaller than the length of the list.
--
-- The list is very long, so making more than one pass is prohibitively
-- expensive.
--
-- Do this in constant space and in one pass.

data LL a
    = Node a (LL a)
    | Nil

instance Show a => Show (LL a) where
    show (Node a _) = (show a)
    show (Nil) = "Nil"

instance (Foldable LL) where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Node v nxt) = (f v) `mappend` (foldMap f nxt)
    foldMap _ (Nil) = mempty
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f i (Node v Nil) = (f v i)
    foldr f i (Node v nxt) = foldr f (f v i) nxt
    
kth_last :: LL a -> Int -> LL a
kth_last ll k = res
    where
        res = loop ll hi
        hi = foldl (\(Node _ nxt) _ -> nxt) ll [1..k] 
        loop lo (Nil) = lo
        loop (Node _ nxt1) (Node _ nxt2) = loop nxt1 nxt2

test1 :: IO ()
test1 = do
    let ll = foldl (\l1 i -> Node i l1) Nil (reverse [1..10])
        res = kth_last ll 5
    putStrLn $ foldMap (\v -> (show v)++" -> ") ll
    putStrLn $ show res

main :: IO ()
main = do
    test1
    putStrLn "Hola Mundas!"