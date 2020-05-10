--Prom 9 a 

maximo :: [Int] -> Int
maximo [a] = a
maximo (x:y:xs) | (x > y) = maximo (x:xs)
                | (y > x) = maximo (y:xs) 

--Prom 9 b

sumapares :: [(Int,Int)] -> Int
sumapares [] = 0
sumapares ((x,y):xs) | 0 == 0 = (fst(x,y)+ snd(x,y)) + (sumapares xs)


--Prom 9 c

--todos :: [Int] -> Bool
--todos [] = True
--todos (x:xs) | x == 0 || x == 1 = todos xs
--             | x > 1 || x < 0 = False

--Prom 9 d
--quitar :: [Int] -> [Int]
--quitar [] = []
--quitar(x:xs) | x == 0 = quitar xs 
--             | x /= 0 = x:(quitar xs)
			
--Prom 9 e

--ultimo :: [Int] -> Int
--ultimo [a] = a
--ultimo (x:xs) | 0 == 0 = (ultimo xs)

--Prom 9 f
--repetir :: Int -> Int -> [Int]
--repetir a b | a *[b]

--Prom 9 g
--concat :: [[Int]] -> [Int]
--concat [[]] = []
--concat ([x]:xs) | = x:( concat [xs])

--Prom 9 h
--rev :: [Int] -> [Int]
--rev [] = []
--rev (x:xs) = (rev xs )++ [x] 


