-- 3. Dar dos ejemplos de funciones que tengan los siguientes tipos:

-- a) (Int -> Int) -> (Bool -> Bool)

-- Función principal
f1 :: (Int -> Int) -> (Bool -> Bool)
f1 _ = not -- ignora el argumento y devuelve not

-- Ejemplo de uso:
doble :: Int -> Int
doble x = 2 * x

-- Entonces: f1 doble = not
-- Y podés hacer: (f1 doble) True   --> False
--                 (f1 doble) False  --> True

f2 :: (Int -> Int) -> (Bool -> Bool)
f2 g b = if g 0 == 0 then not b else b

-- Ejemplo de funciones Int -> Int:

-- g1: devuelve 0 siempre
cero :: Int -> Int
cero _ = 0

-- g2: suma 1
suma1 :: Int -> Int
suma1 x = x + 1

-- Entonces:
-- f2 cero = not
-- f2 suma1 = id

-- Pruebas:
-- (f2 cero) True   --> False
-- (f2 suma1) True  --> True

-- b) Bool -> (Int -> Bool)

f3 :: Bool -> (Int -> Bool)
f3 True = \x -> even x -- si el bool es True, devuelve función que dice si es par
f3 False = \x -> odd x -- si es False, devuelve función que dice si es impar

-- Pruebas:
-- (f1 True) 4   --> True  (porque 4 es par)
-- (f1 False) 4  --> False (porque 4 no es impar)
-- (f1 True) 7   --> False
-- (f1 False) 7  --> True

-- Tipo: Bool -> (Int -> Bool)

f4 :: Bool -> (Int -> Bool)
f4 True = \x -> x >= 0 -- si el bool es True, devuelve función que chequea si el número es no negativo
f4 False = \x -> x < 0 -- si es False, devuelve función que chequea si es negativo

-- Pruebas:
-- (f2 True) 5    --> True
-- (f2 True) (-3) --> False
-- (f2 False) 5   --> False
-- (f2 False) (-3)--> True

-- c) Char -> Char

f5 :: Char -> Char
f5 c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

f6 :: Char -> Char
f6 c = toEnum (fromEnum c + 1)

-- d) f7 :: Int -> (Int -> Bool) -> [Int]

f7 :: Int -> (Int -> Bool) -> [Int]
f7 n cond = [x | x <- [0 .. n], cond x]

-- e) [a] -> (a -> [b]) -> [b]

f8 :: [a] -> (a -> [b]) -> [b]
f8 xs f = [y | x <- xs, y <- f x]

-- f8 [1,2,3] (\x -> [x, x])
-- Resultado: [1,1,2,2,3,3]       -- duplica cada elemento

-- f) [[a]] -> (a -> Bool) -> [a]

f9 :: [[a]] -> (a -> Bool) -> [a]
f9 xss cond = [x | xs <- xss, x <- xs, cond x]

-- f9 [[1,2,3], [4,5], [6]] even
-- Resultado: [2,4,6]        -- pares en listas anidadas

-- g) (a, b, c) -> Bool

f10 :: Eq a => (a, a, a) -> Bool
f10 (x, y, z) = x == y && y == z

-- f10 (1, 1, 1)
-- Resultado: True

-- h) (a, b, c) -> Int -> c

f12 :: (a, b, c) -> Int -> c
f12 (_, _, z) _ = z

-- f12 (1, 2, "hola") 5
-- Resultado: "hola"

-- i) (a, a, a) -> Int -> a

f13 :: (a, a, a) -> Int -> a
f13 (x, y, z) n
  | n == 0 = x
  | n == 1 = y
  | n == 2 = z
  | otherwise = error "Índice fuera de rango: debe ser 0, 1 o 2"

-- f13 ('a', 'b', 'c') 0
-- Resultado: 'a'