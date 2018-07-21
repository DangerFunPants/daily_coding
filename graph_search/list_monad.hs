import Control.Monad

powerset :: [a] -> [[a]]
powerset s = do
    ps <- filterM (\_ -> [True, False]) s
    return ps


