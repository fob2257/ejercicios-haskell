import Data.Char
import Data.List

{-- ejercicio a 
    Suma todos los elementos de una lista de numeros
--}
suma :: [Integer]->Integer
suma [] = 0;
suma (x:xs) = x+suma(xs);

{-- ejercicio b
    Devuelve True si algún elemento de una lista de valores booleanos
    es True, y False en caso contrario
--}
alguno :: [Bool] -> Bool
alguno [] = False;
alguno (x:xs) = 
              if (x==True) then
                True
              else
                alguno(xs)

{-- ejercicio c
    Devuelve True si todos los elementos de una lista de valores booleanos
    son True, y False en caso contrario
--}
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) =
              if (x && todos(xs)) then
                True
              else
                False

{-- ejercicio d
    Dada una lista de caracteres, devuelve la lista de sus ordinales
--}
codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) = [ord x] ++ codes(xs)

{-- ejercicio e
    Calcula la lista de los restos de la división de los elementos
    de una lista de números dada por otro número dado
--}
restos :: [Integer] -> Integer -> [Integer]
restos [] a = []
restos (x:xs) a = [(mod x a)] ++ restos(xs) a

{-- ejercicio f 
    Dada una lista de números, devuelva la lista de sus cuadrados
--}
cuadrados :: [Integer] -> [Integer]
cuadrados [] = []
cuadrados (x:xs) = (x^2):cuadrados(xs)
{-- ^es lo mismo que..
     cuadrados (x:xs) = [x^2]++cuadrados(xs)
--}

{-- ejercicio g
    Dada una lista de listas, devuelve la lista de sus longitudes
--}
longitudes :: [[Int]] -> [Int]
longitudes [] = []
longitudes (x:xs) = [(length x)] ++ longitudes(xs)

{-- ejercicio h
    Dada una lista de pares de números, devuelve la lista de aquellos 
    pares en los que la primera componente es menor que el triple de la
    segunda
--}
orden :: [(Integer, Integer)] -> [(Integer, Integer)]
orden [] = []
orden ((x,y):xs) = if x < y*3 then 
                    (x,y):orden xs 
                   else orden xs

{-- ejercicio i
    Dada una lista de enteros, devuelve la lista de los elementos pares
--}
pares :: [Int] -> [Int]
pares lista = [ x | x<-lista, igualPar x];

igualPar :: Int -> Bool
igualPar x = mod x 2 ==0;

{-- ejercicio j
    Dada una lista de caracteres, devuelve la lista de aquellos que son letras
--}
letras :: [Char] -> [Char]
letras lista = [x | x<-lista, listachar x]

listachar:: Char -> Bool
listachar x = if(ord x >= 65 && ord x <= 122) then True else False

{-- ejercicio k
    Dada una lista de listas xss y un número n, devuelve la lista de aquellas 
    listas de xss con longitud mayor que n
--}
masDe :: [[xss]] -> Int -> [[xss]]
masDe [] n = []
masDe (x:xs) n = if (length x > n) then
                    (x):masDe xs n
                  else
                    masDe xs n
