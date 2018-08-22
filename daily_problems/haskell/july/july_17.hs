#!/usr/bin/env runhaskell

-- This problem was asked by Google.
-- 
-- The power set of a set is the set of all its subsets. Write a function that, 
-- given a set, generates its power set.
-- 
-- For example, given the set {1, 2, 3}, it should return {{}, {1}, {2}, {3}, 
-- {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}.
-- 
-- You may also use a list or array to represent a set.

import Control.Monad (filterM)
import Data.Monoid ((<>))

data List a
    = Cons a (List a)
    | Nil
    deriving (Eq)

instance Show a => Show (List a) where
    show Nil = "[]"
    show ls = "[ "++(showEs ls)++" ]"
        where
            showEs (Cons a rs@(Cons _ rest)) = (show a)++", "++(showEs rs)
            showEs (Cons a rest) = (show a)++(showEs rest)
            showEs Nil = ""

instance Monoid (List t) where
   mempty = Nil
   mappend (Cons a rest) b = Cons a (mappend rest b)
   mappend Nil b = b

instance Functor (List) where
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)
    fmap _ Nil = Nil

instance Applicative (List) where
    pure a = Cons a Nil
    -- <*> :: [a -> b] -> [a] -> [b]
    fs <*> vs = conList $ fmap (\f -> (fmap (\v -> f v) vs)) fs
        where
            conList (Cons (ls) rest) = ls <> (conList rest)
            conList Nil = Nil

instance Monad (List) where
    return a = Cons a Nil
    -- >>= :: Monad m => m a -> (a -> m b) -> m b
    (Cons a rest) >>= f = (f a) <> (rest >>= f)
    Nil >>= _ = Nil

instance Foldable (List) where
    -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
    foldMap f (Cons a rest) = (f a) <> (foldMap f rest)
    foldMap _ Nil = mempty

    -- foldr :: Traversable t => (a -> b -> b) -> b -> t a -> b
    foldr f i (Cons a rest) = res
        where
            recCall = foldr f i rest
            res = f a recCall
    foldr _ i Nil = i

-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   sequence :: Monad m => t (m a) -> m (t a)
--   {-# MINIMAL traverse | sequenceA #-}
--
--class Functor f => Applicative (f :: * -> *) where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b
--  (*>) :: f a -> f b -> f b
--  (<*) :: f a -> f b -> f a
--  {-# MINIMAL pure, (<*>) #-}

instance Traversable (List) where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse _ Nil = pure Nil
    traverse f ls@(Cons a rest) = foldl (\i fb -> fmap (\v -> Cons v i) fb) (pure Nil) (fmap f ls)

    -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
    sequenceA Nil = pure Nil
    

filterM' :: Monad m => (a -> m Bool) -> (List a) -> m (List a)
filterM' pred (Cons a rest) = do
    res <- pred a
    rem <- filterM' pred rest
    if res
        then return (Cons a rem)
        else return rem
filterM' _ Nil = return Nil

    

powerset' :: (List a) -> (List (List a))
powerset' s = do
    p <- filterM' (\_ -> Cons True (Cons False Nil)) s
    return p

test1 :: IO ()
test1 = do
    let ts = Cons 1 (Cons 2 (Cons 3 Nil))
        res = powerset' ts
    putStrLn $ "Power set of: "++(show ts)++" : "++(show res)

main :: IO ()
main = do
    test1
