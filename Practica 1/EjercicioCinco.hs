{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}

-- 5. Definir las siguientes funciones usando foldr:

-- Estructura de la función foldr: foldr f acc xs
-- Donde: f es la funcion que se aplica, acc es el valor inicial del acumulador y xs la lista a procesar.

-- foldr (+) 0 [1, 2, 3]
-- Se evalúa como: 1 + (2 + (3 + 0)) = 6

-- foldr (:) [] [1,2,3]
-- Se evalúa como: 1 : (2 : (3 : [])) = [1,2,3]

-- any (>3) [1,2,5] = foldr (\x acc -> x > 3 || acc) False [1,2,5]
-- Resultado: True

-- a) map' :: (a -> b) -> [a] -> [b] que dada una funcion y una lista, aplica la funcion a cada elemento de la lista.

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- b) filler :: (a -> Bool) -> [a] -> [a], que dado un predicado y una lista xs, devuelve una lista con los elementos de xs que satisfacen el predicado.

filler :: (a -> Bool) -> [a] -> [a]
filler p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

-- c) unzip :: [(a, b)] -> ([a], [b]), que dada una lista de tuplas xs retorna una tupla de listas donde cada una corresponde a los primeros y segundos elementos de los pares respectivamente. Ej: unzip [('a', 1), ('b', 2), ('c', 3)] = (['a', 'b', 'c'], [1, 2, 3]).

unzip :: [(a, b)] -> ([a], [b])
unzip xs = foldr (\(x, y) (acc1, acc2) -> (x : acc1, y : acc2)) ([], []) xs

-- d) pair2List :: (a, [b]) -> [(a, b)] que dado un par formado por un valor x y una lista xs convierta a la lista xs en una lista de pares, formada con los elementos de xs y x. Ej: pair2List (x, [y1, y2, y3]) = [(x, y1), (x, y2), (x, y3)].

pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, xs) = foldr (\y acc -> (x, y) : acc) [] xs

-- e) maxSec :: [(Int), (Int)] -> (Int, Int), que dada una lista de pares de naturales que represnete a una lista de segmentos de la recta, calcule el segmento mas largo de la misma.

maxSec :: [(Int, Int)] -> (Int, Int)
maxSec xs = foldr (\(x1, x2) (y1, y2) -> if (x2 - x1) > (y2 - y1) then (x1, x2) else (y1, y2)) (0, 0) xs