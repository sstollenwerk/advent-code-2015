module Generic where

flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)


partsSizeN :: Int -> [a] -> [[a]]
partsSizeN n = filter ((== n) . length) . map (take n) . tails