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
             
foo :: [[(Int, Int)]] -> Int
foo xs = let l = map (foldr (\x y -> snd x + y) 0) xs in
         findMin l (minimum l)

--Puts a new element in the pool n
fillPool :: Int -> (Int,Int) -> [[(Int,Int)]] -> [[(Int,Int)]]
fillPool _ _ [] = []
fillPool n x (m:atrix) = if n == 0 then (x:m):atrix
                          else m : fillPool (n-1) x atrix

--Puts each row in the rigth format
filterList :: [(Int,Int)] -> [(Int,Int)]
filterList [] = []
filterList (x:xs) = x : (filterList $ drop (fst x - 1) xs)

--
findMin :: [Int] -> Int -> Int
findMin [] _ = 0
findMin (x:xs) n = if x == n then 0
                   else 1 + findMin xs n

line :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
line [] l = l
line (x:xs) l = line xs (fillPool (foo l) x l)

rows :: [[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]]
rows [] l = l
rows (x:xs) l = rows xs $ line (filterList x) l
