esmultiplo :: Int -> Int -> Bool
esmultiplo a b | mod b a == 0 = True
               | mod b a /= 0 = False