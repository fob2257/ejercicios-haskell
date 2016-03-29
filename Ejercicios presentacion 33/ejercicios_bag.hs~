import Data.Char
import Data.List

type Bag a = [(a,Int)]

--bagInsert :: a -> Bag a -> Bag a 
bagInsert a [] = [(a,1)]
bagInsert a ((b,n):xs) = if (a == b) then
                          (b,n+1):xs 
                         else
                          (b,n):bagInsert a xs

-- list2bag :: [a] -> Bag a
-- Num de ocurrencias de un elemento en la lista
list2bag [] = []
list2bag (x:xs) = bagInsert x (list2bag xs)
