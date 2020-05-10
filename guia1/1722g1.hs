--Prom18

esbisiesto :: Int -> Bool 
esbisiesto x | mod x 400  == 0 = True
             | (mod x 4 == 0) && (mod x 100 /= 0) = True
             | otherwise = False

--Prom 19
dispersion :: Int -> Int -> Int -> Int
dispersion a b c | let x = ((max a (max b c)) - (min a (min b c))) = x   


-- Prom 20

celsius :: Float -> Float 
celsius x | let a = (x * 1.8 + 32 ) = a

-- Prom 21

far :: Float -> Float
far x | let b = (x-32)/ 1.8 = b

-- Prom 22
hacefrio :: Float -> Bool
hacefrio x | x < (8 * 1.8 +32) = True
           | otherwise = False