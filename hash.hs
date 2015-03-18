import Data.List

toRow :: (Int, Int) -> [[(Int, Int)]] -> [[(Int, Int)]]
toRow _ [] = []
toRow x (m:atrix) = let a = fit (fst x) m 0 in
                    if a >= 0 then ((take a m) ++ (take (fst x) $ repeat x)
                                  ++ (drop (a + fst x) m)) : atrix
                    else m : (toRow x atrix)

fit :: Int -> [(Int,Int)] -> Int -> Int
fit 0 _ k = 0
fit a [] k = -1
fit a (x:xs) k = if fst x == 0 then fit (a-1) xs (k+1)
                 else if (aux >= 0) then aux + k + 1
                      else -1
                      where aux = fit (a+k) xs 0

goList :: [(Int, Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
goList l m = foldr (toRow) m (sort l)

--createPools :: [[Int]] -> [[Int]]
--createPools [] = 0
