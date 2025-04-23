-- 1. Definir las siguientes funciones en forma recursiva:

-- a) borrarUltimo que dada una lista borra el ultimo elemento de la lista. No utlizar reverse ni tail.

borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x : xs) = x : borrarUltimo xs

-- b) collect :: [(k,v)] toma una lista de pares (clave, valor) y asocia cada clave unica con todos los valores que los que estaba apareada originalmente.
-- Por ejemplo, collect [(1,2),(1,3),(2,4),(2,5)] = [(1,[2,3]),(2,[4,5])].

collect :: (Eq k) => [(k, v)] -> [(k, [v])]
collect [] = []
collect ((k, v) : xs) = (k, v : valores) : collect (filtrar xs k)
  where
    valores = [v' | (k', v') <- xs, k' == k]
    filtrar [] _ = []
    filtrar ((k', v') : xs) k = if k' == k then filtrar xs k else (k', v') : filtrar xs k

-- c) serie que se comporta de la siguiente manera: serie [1,2,3] = [[],[1],[1,2],[1,2,3]]. Dar su tipo mas general.

serie :: [a] -> [[a]]
serie [] = [[]]
serie (x : xs) = [] : map (x :) (serie xs)

-- d) paresIguale :: Int -> Int -> Int -> Int -> Bool toma 4 numeros enteros y retorna True si de dos en dos sin iguales (en cualquier orden), en los demas casos retorna False. Por ejemplo: paresIguales 3 1 3 1 = True, paresIguales 3 3 1 1 = True, paresIguales 3 1 1 1 = False, paresIguales 3 1 2 2 = False.

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = (a == b && c == d) || (a == c && b == d) || (a == d && b == c)

-- e) isosceles :: Int -> Int -> Int -> Bool que dadas las longitudes de los lados de un triangulo nos dice si es un triangulo isosceles.

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = (a == b && a + b > c) || (a == c && a + c > b) || (b == c && b + c > a)

-- f) ror que dada una lista xs y un entero n tal que n <= length xs, rota los primeros n elementos de xs a la derecha. Por ejemplo, ror 3 [1,2,3,4,5] = [4,5,1,2,3]. Definir una version recursiva de ror sin usar drop, take ni tail.

ror :: Int -> [a] -> [a]
ror 0 xs = xs
ror n (x : xs)
  | n >= length (x : xs) = error "n no puede ser mayor que la longitud de la lista"
  | otherwise = ror (n - 1) (xs ++ [x])

-- g) upto :: Int -> Int -> [Int] que dado dos numeros enteros n y m devuelve la lista [n,n + 1,n + 2, ..., m]. Por ejemplo, upto 3 7 = [3,4,5,6,7]. En caso de que n <= m y la lista [] en otro caso. No usar listas por comprension.

upto :: Int -> Int -> [Int]
upto n m
  | n > m = []
  | otherwise = n : upto (n + 1) m

-- h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tanta veces como indica su posicion. No usar listas por comprension

eco :: String -> String
eco xs = ecoAux xs 1

ecoAux :: String -> Int -> String
ecoAux [] _ = []
ecoAux (x : xs) n = replicate n x ++ ecoAux xs (n + 1)