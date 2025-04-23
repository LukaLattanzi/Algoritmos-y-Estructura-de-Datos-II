{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use foldr" #-}

add' :: (Int, Int) -> Int
add' (x, y) = x + y

zeroto :: Int -> [Int]
zeroto x = [0 .. x]

-- Una funcion es polimorfica si su tipo contiene una o mas variables de tipo.

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- Las funciones pueden ser definidas usando expresiones condicionales.

abs :: Int -> Int
abs n = if n > 0 then n else -n

-- Las expresiones condicionales pueden estar anidadas:

signum :: Int -> Int
signum n =
  if n < 0
    then -1
    else
      if n == 0 then 0 else 1

-- Una alternativa a los condicionales es el uso de ecuaciones con guardas.

abs' :: (Ord a, Num a) => a -> a
abs' n
  | n > 0 = n
  | otherwise = -n

-- Toda lista no vacia se construye usando el operador (:) llamado "cons" que agrega un elemento al principio de la lista. [1,2,3,4] resulta de 1:(2:(3:(4:[])))

-- Las funciones pueden construirse sin nombres usando expresiones lambda y en el teclado se ingresa \.

odds' :: (Num b, Enum b) => b -> [b]
odds' n = map f [0 .. n - 1]
  where
    f x = x * 2 + 1

odds'' :: (Num b, Enum b) => b -> [b]
odds'' n = map (\x -> x * 2 + 1) [0 .. n - 1]

-- Una manera de contruir conjuntos a partir de conjuntos existentes es con la notacion por comprension.

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- > concat [[1,2,3],[4,5],[6]] => [1,2,3,4,5,6]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- Como un numero n es primo si y solo si sus unicos factores son 1 y n podemos definir:

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

-- La funcion zip, mapea dos listas a una lista con los pares de elementos correspondientes. zip :: [a] -> [b] -> [(a,b)]. Usando zip podemos definir una funcion que retorna la lista de pares de elementos adyacentes.

pairs :: [a] -> [(a, a)]
pairs xs = zip' xs (tail xs)

-- pairs [1, 2, 3, 4] == [(1, 2), (2, 3), (3, 4)]

-- Podemos usar pairs para decicir si una lista esta ordenada.

sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- Podemos usar zip para generar una funcion que retorne la listas de posiciones de un valor determinado

positions :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
positions x xs = [i | (x', i) <- zip' xs [0 ..], x == x']

-- > positions 0 [1,0,0,1,0,1,1,0] => [1,2,4,7]

-- Una String es una lista de caracteres [Char]. Todas las funciones sobre listas son aplicables a String.

count :: (Eq a) => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

-- count 's' "Mississippi" => 4

-- Como hemos visto las funciones pueden definirse en terminos de otras funciones.

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- En haskell las funciones pueden ser definidas en terminos de ellas mismas (recursion).

factorial' :: (Eq a, Num a, Enum a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

{-
factorial 3
= 3 * factorial 2
= 3 * (2 * factorial 1)
= 3 * (2 * (1 * factorial 0))
= 3 * (2 * (1 * 1))
= 6
-}
product' :: (Num a) => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

-- Usando la recursion podemos definir el reverso de una lista.

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

{-
reverse [1,2,3]
= reverse [2,3] ++ [1]
= (reverse [3] ++ [2]) ++ [1]
= ((reverse [ ] ++ [3]) ++ [2]) ++ [1]
= (([ ] ++ [3]) ++ [2]) ++ [1]
= [3,2,1]
-}

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

{-
El algoritmo de ordenacion quicksort:
  La lista vacia esta ordenada.
  La listas no vacias pueden ser ordenadas, ordenando los valores de la cola <= que la cabeza, ordenando los valores > que la cabeza y rearmando el resultado con las listas resultantes a ambos lados de la cabeza.
-}

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort chicos ++ [x] ++ qsort grandes
  where
    chicos = [a | a <- xs, a <= x]
    grandes = [b | b <- xs, b > x]
