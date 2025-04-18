-- Operaciones aritmeticas simples en ghci

-- > 1 + 2
-- > 49 * 100
-- > 1000 / 10
-- > 1000 `div` 10 | (división entera)
-- > 1000 `mod` 10 | (módulo)
-- > 2 ^ 10 | (potencia)

-- El algebra booleana && representa el "y" lógico, || representa el "o" lógico y not representa la negación lógica.

-- > True && False
-- True || False
-- not True
-- not False

-- Comprobacion de igualdad y desigualdad:

-- > 1 == 1
-- > 1 /= 2
-- > 1 < 2
-- > 1 > 2
-- > 1 <= 2
-- > 1 >= 2
-- > 1 `elem` [1,2,3] | (pertenencia a una lista)

-- funciones sobre numeros:

-- > succ 1 | (sucesor)
-- > pred 1 | (predecesor)
-- > abs (-1) | (valor absoluto)
-- > min 1 2 | (mínimo)
-- > max 1 2 | (máximo)

-- Una introducción a las listas:

-- Para concatenar dos listas se usa el operador ++:
-- > [1,2] ++ [3,4]
-- [1,2,3,4]

-- Para concatenar algo algo al final de una lista se usa el operador : (cons):
-- > 1 : [2,3]
-- [1,2,3]

-- Si queremos obtener un elemento de una lista, usamos el operador !!:
-- > [1,2,3] !! 0 | (primer elemento de la lista)
-- 1

-- Funciones basicas que pueden operar con las listas.Applicative

-- > head [5,4,3,2,1] -> 5 | (primer elemento de la lista)
-- > tail [5,4,3,2,1] -> [4,3,2,1] | (lista sin el primer elemento)
-- > last [5,4,3,2,1] -> 1 | (último elemento de la lista)
-- > init [5,4,3,2,1] -> [5,4,3,2] | (lista sin el último elemento)

-- length [5,4,3,2,1] -> 5 | (longitud de la lista)
-- null [5,4,3,2,1] -> False | (verifica si la lista es vacía)
-- null [] -> True | (verifica si la lista es vacía)
-- reverse [5,4,3,2,1] -> [1,2,3,4,5] | (invierte la lista)
-- take 3 [5,4,3,2,1] -> [5,4,3] | (toma los primeros 3 elementos de la lista)
-- drop 3 [5,4,3,2,1] -> [2,1] | (descarta los primeros 3 elementos de la lista)
-- maximum [5,4,3,2,1] -> 5 | (máximo de la lista)
-- minimum [5,4,3,2,1] -> 1 | (mínimo de la lista)
-- sum [5,4,3,2,1] -> 15 | (suma de los elementos de la lista)
-- product [5,4,3,2,1] -> 120 | (producto de los elementos de la lista)
-- elem 3 [5,4,3,2,1] -> True | (verifica si 3 está en la lista)

-- Rangos de listas:
-- Los rangos son una manera de crear listas que contengan una secuencia de elementos enumerables.Applicative
-- Para crear una lista que contenga todos los numeros del 1 al 20 escribimos:
-- > [1..20]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

-- Tambien podemos especificar el numeor de pasos entre elementos de un rango:
-- > [1,3..20]
-- [1,4,7,10,13,16,19]

-- Para obtener una lista con todos los numeros del 20 hasta el 1 no podemos usar [20..1].
-- En su lugar, tenemos que usar la siguiente sintaxis:
-- > [20,19..1]
-- [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

-- Un par de funciones que generan listas infinintas son:

-- Cycle: toma una lista y crea un ciclo de listas iguales infinitos.
-- > take 10 (cycle [1,2,3])
-- [1,2,3,1,2,3,1,2,3,1]

-- repeat: toma un elemento y produce una lista infinita que contiene ese unico elemento repetido.
-- > take 10 (repeat 5)
-- [5,5,5,5,5,5,5,5,5,5]

-- Listas Iensionales: Muy similares a los conjuntos definidos en matematicas.

-- Ejemplo de una lista de comprensión:
-- > [x*2 | x <- [1..10]]
-- [2,4,6,8,10,12,14,16,18,20]

-- Podemos agregarle una condicion a una lista intensional.

-- > [x*2 | x <- [1..10], x*2 >= 12]
-- [12,14,16,18,20]

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Podemos incluir varios predicados en la lista de comprensión:
-- > [x*2 | x <- [1..10], x*2 >= 12, x*2 < 20]
-- [12,14,16,18]

length' :: (Num a) => [t] -> a
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

-- Tuplas
-- Utilizamos las tuplas cuando sabemos de antemano cuantos componentes de algun dato debemos tener.

-- Dos funciones clave para operar con tuplas:
-- > fst (1,2) -> 1 | (primer elemento de la tupla)
-- > snd (1,2) -> 2 | (segundo elemento de la tupla)

-- He aquí un problema que combina tuplas con listas intensionales: ¿Qué triángulo recto cuyos lados miden enteros menores que 10 tienen un perímetro igual a 24? Primero, vamos a intentar generar todos los triángulos con lados iguales o menores que 10:

triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2]

-- Simplemente estamos extrayendo valores de estas tres listas y nuestra función de salida las esta combinando en una tripla. Si evaluamos esto escribiendo triangles en GHCi, obtendremos una lista con todos los posibles triángulos cuyos lados son menores o iguales que 10. Ahora, debemos añadir una condición que nos filtre únicamente los triángulos rectos. Vamos a modificar esta función teniendo en consideración que el lado b no es mas largo que la hipotenusa y que el lado a no es más largo que el lado b.

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(a, b, c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2]

-- Ya casi hemos acabado. Ahora, simplemente modificaremos la función diciendo que solo queremos aquellos que su perímetro es 24.

rightTriangles' :: [(Int, Int, Int)]
rightTriangles' = [(a, b, c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2, a + b + c == 24]

