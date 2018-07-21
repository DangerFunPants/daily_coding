#!/usr/bin/env runhaskell

-- This problem was asked by Facebook.
--
-- Given a string of round, curly, and square open and closing brackets, return
-- whether the brackets are balanced (well-formed).
--
-- For example, given the string "([])[]({})", you should return true.
--
-- Given the string "([)]" or "((()", you should return false.

newtype State s a = State
    { runState :: s -> (a, s)
    }

instance Functor (State s) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (State g) = State r
        where
            r = \s -> 
                let (a, s') = g s
                    b = f a
                    in (b, s')

instance Applicative (State s) where
    -- g :: s -> (a, s)
    -- f :: s -> ((a -> b), s)
    -- h :: (a -> b)
    pure a = State (\s -> (a, s))
    (State f) <*> (State g) = State r
        where 
            r = \s -> 
                let (h, s') = f s
                    (a, s'') = g s'
                    b = h a
                    in (b, s'')

instance Monad (State s) where
    -- return :: Monad m => a -> m a
    return a = State (\s -> (a, s))
    -- >>= :: Monad m => m a => (a -> m b) -> m b
    (State f) >>= g = State r
        where 
            r = \s ->
                    let (a, s') = f s
                        (State h) = g a 
                        (b, s'') = h s'
                        in (b, s'')

type PStack = State String

opens = "({["
closes = ")}]"

match :: Char -> Char -> Bool
match '(' ')' = True
match '{' '}' = True
match '[' ']' = True
match _ _ = False

pop :: PStack Char
pop = State $ \s -> ((head s), (tail s))

push :: Char -> PStack ()
push c = State $ \s -> ((), (c:s))

size :: PStack Int
size = State $ \s -> ((length s), s)

validateString :: String -> PStack Bool
validateString (p:ps) = do
    if (elem p opens)
        then 
            do
                push p
                (validateString ps)
        else
            do
                m <- pop
                if (match m p)
                    then (validateString ps)
                    else (return False) 

validateString [] = do
    s <- size
    if s == 0
        then (return True)
        else (return False)

test1 :: IO ()
test1 = do
    let tests = [ "([])[]({})"
                , "([)]"
                , "((()"
                ]
        res = fst <$> fmap (($ "") . (runState . (\s -> (validateString s)))) tests
    putStrLn $ (show res)

main :: IO ()
main = do
    test1