-- 2. Definir usando listas por comprension las siguientes funciones:

-- a) cambios :: [a] -> [Int], que dada una lista, devuelve la lista de los indices en que la lista cambia. Es decir, dada la lista s retorna la lista con los i tal que si //= si+1. cambios [1,1,1,3,3,1,1] = [2,4].

cambios :: (Eq a) => [a] -> [Int]
cambios xs = [i | (i, (a, b)) <- zip [0 ..] (zip xs (tail xs)), a /= b]

-- b) oblongoNumber :: [Int] que genera la lista de los numeros oblongos. Un numero es oblongo si es el producto de dos naturales consecutivos. Por ejemplo, 0, 2, 6, 12, 20, 30, 42, 56, 72, 90 son los primeros diez numeros oblongos.

oblongoNumber :: [Int]
oblongoNumber = [n * (n + 1) | n <- [0 ..]]

-- c) abundantes :: [Integer] que es la lista de todos los numeros abundantes. Un numero natural n se denomina abuntande si es menor que la suma de sus divisores propios. Por ejemplo, 12 es abundante porque 1 + 2 + 3 + 4 + 6 = 16 > 12.

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1 .. n - 1], n `mod` x == 0]

abundantes :: [Integer]
abundantes = [n | n <- [1 ..], n < sum (divisores n)]

-- d) eco :: String -> String que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas veces como indica su posicion. No usar listas por comprension.

eco :: String -> String
eco xs = concat [replicate n x | (x, n) <- zip xs [1 ..]]

-- e) euler :: Int -> Int tal que euler n es la suma de todos los multiplos de 3 o 5 menores que n. Por ejemplo, euler 10 = 23 porque 3 + 5 + 6 + 9 = 23.

euler :: Int -> Int
euler n = sum [x | x <- [1 .. n - 1], x `mod` 3 == 0 || x `mod` 5 == 0]

-- f) expandir :: [Int] -> [Int] que reemplace en una lista de numeros positivos cada numero n por n copias de si mismo: expandir [1,2,3] = [1,2,2,3,3,3].

expandir :: [Int] -> [Int]
expandir xs = concat [replicate n n | n <- xs]

expandir' :: [Int] -> [Int]
expandir' xs = concat [replicate x n | (x, n) <- zip xs xs]