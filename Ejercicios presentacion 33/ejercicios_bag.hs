import Data.Char
import Data.List

type Bag a = [(a,Int)]

--ejercicio a
--bagInsert :: a -> Bag a -> Bag a 
bagInsert a [] = [(a,1)]
bagInsert a ((b,n):xs) = if (a == b) then
                          (b,n+1):xs 
                         else
                          (b,n):bagInsert a xs

-- ejercicio b
-- Num de ocurrencias de un elemento en la lista
-- list2bag :: [a] -> Bag a
list2bag [] = []
list2bag (x:xs) = bagInsert x (list2bag xs)

-- ejercicio c
bagEmpty x = if ( x == [] ) then True else False

-- ejercicio d
-- Devuelve su cardinalidad (total de las ocurrencias, segundo elemento de la tupla)
bagCar [] = 0
bagCar ((b,n):xs) = n + bagCar xs

-- ejercicio e
-- Devuelve True si un elemento dado está en el bag
bagElem n [] = False
bagElem n ((b,m):xs) = if (n == b) then True 
                        else bagElem n xs

-- ejercicio f
-- Devuelve True si un elemento está contenido en el bag con la misma cantidad de ocurrencias
bagOccur (n,m) [] = False
bagOccur (n,m) ((v,b):xs) = if n==v && m==v then True else bagOccur (n,m) xs

-- ejercicio g
-- Devuelve True si dos bags tienen los mismos elementos con la misma cantidad de ocurrencias
bagEqual [] [] = True
bagEqual _ [] = False
bagEqual [] _ = False
bagEqual ((z,x):xs) ((c,v):vs) = if (z==c && x==v) then True && bagEqual xs vs else False

-- ejercicio h
-- Devuelve True si el primer bag es subbag del segundo
bagSubbag [] [] = True
bagSubbag _ [] = False
bagSubbag [] _ = False
bagSubbag ((z,x):xs) ((n,m):ms) = if z==n && x==m then True && bagSubbag xs ms 
                                    else bagSubbag ((z,x):xs) ms

-- ejercicio i
{- Toma 2 bags y calcula el bag interseccion. La intersección de 2 bags X e Y contiene
   a todos los elementos comunes a X e Y con la menos cantidad posible de ocurrencias -}
bagInter [] _ = []
bagInter ((z,x):xs) ((c,v):vs) = if (a > 0) then (z,x):bagInter xs ((c,v):vs) else bagInter xs ((c,v):vs)
                                  where
                                    a = bagminimo [(z,x)] ((c,v):vs)

bagminimo [] [] = 0
bagminimo _ [] = 0
bagminimo [] _ = 0
bagminimo [(a,n)] ((b,m):ys) = if (a == b) then min n m else bagminimo [(a,n)] ys

-- ejercicio j
-- Calcula el bag union de dos bags
bagSum [] vs = []
bagSum ((z,x):xs) vs = bagSum xs (insertarbag z x vs)

insertarbag z 1 vs = bagInsert z vs
insertarbag z x vs = bagInsert z (insertarbag z (x-1) vs)