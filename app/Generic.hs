module Generic where

flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)
