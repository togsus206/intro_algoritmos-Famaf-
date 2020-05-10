module Language.ListAssoc where

data ListAssoc a b = Empty
                   | Node a b (ListAssoc a b)
      deriving (Show)
                   
la_long :: Integral c => ListAssoc a b -> c
la_long Empty = 0
la_long (Node _ _ (v)) = 1+ la_long v

la_aListaDePares :: ListAssoc a b -> [(a,b)]
la_aListaDePares Empty = []
la_aListaDePares (Node a b v) = (a,b): la_aListaDePares v

la_existe :: Eq a => ListAssoc a b -> a -> Bool
la_existe Empty _ =  False
la_existe (Node a b c) t | a == t = True 
                         | otherwise = la_existe c t 

la_buscar :: Eq a => ListAssoc a b -> a -> Maybe b
la_buscar Empty _ = Nothing
la_buscar (Node a b t) f | a == f = Just b 
                         | otherwise = la_buscar t f

la_agregar :: Eq a => a -> b -> ListAssoc a b -> ListAssoc a b
la_agregar a b Empty = Node a b Empty
la_agregar a b (Node c d r) | a == c = Node a b r 
                            | otherwise = Node c d (la_agregar a b r)


la_borrar :: Eq a => a -> ListAssoc a b -> ListAssoc a b
la_borrar _ Empty = Empty 
la_borrar r (Node a b t) | r == a = la_borrar r t 
                         | otherwise = (Node a b (la_borrar r t))






