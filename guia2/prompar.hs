supernota :: Int -> [Int] -> Int
supernota n [] = 0
supernota n (x:xs) | x >= n = 1 + supernota n xs
                   | x < n = supernota n xs
				   
				 
				 
regulares :: [Int] -> Int
regulares (x:xs) | x <= 6 = supernota 4 (x:regulares xs)
                 | x > 6 = supernota 4 (regulares xs)
				 
				 
