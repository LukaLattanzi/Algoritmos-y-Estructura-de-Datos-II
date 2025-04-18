-- Recursion
-- La recursion es en realidad una forma de definir funciones en la que dicha funcion es utilizada en la propia definicion de la funcion.
--
-- La funcion maximum toma una lista de cosas que pueden ser ordenadas y devuelve la mas grande.
-- Para definirla de manera reursiva podriamos establecer un caso base diciendo que el maximo de una lista unitaria es el unico elemento que contiene la lista.
-- Luego podemos decir que el maximo de una lista mas larga es la cabeza de esa lista si es mayor que el maximo de la cola, o el maximo de la cola en caso que no lo sea.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Máximo de una lista vacía"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

-- El primer caso dice que si una lista esta vacia ¡Error!
-- El segundo patron tambien representa un caso base, dice que si nos dan una lista unitaria simplemente devolvemos ese unico elemento.
-- En el tercer patron es donde esta la accion, usamos un patron para dividir la lista en cabeza y cola. Usamos una seccion where para definir maxTail como el maximo del resto de la lista. Luego comprobamos si la cabeza es mayor que el resto de la ola, si lo es, devolvemos la cabeza, si no, el maximo del resto de la lista.

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x : xs) = x `max` maximum'' xs

-- Vamos a implementar replicate, esta toma un Int y algun elemento y devuelve una lista que contiene varias repeticiones de ese mismo elemento.

replicate' :: (Ord t1, Num t1) => t1 -> t2 -> [t2]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

-- Ahora vamos a implementar take, esta toma un cierto numero de elementos de una lista.

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs

-- QuickSort
-- Existe un algoritmo muy interesante para ordenar elementos de una lista llamado quicksort. Este algoritmo es muy eficiente y se basa en la idea de dividir y conquistar. La idea es la siguiente:
-- 1. Elegir un elemento de la lista como pivote.
-- 2. Dividir la lista en dos sublistas: una con los elementos menores o iguales al pivote y otra con los elementos mayores al pivote.
-- 3. Aplicar recursivamente el algoritmo a las dos sublistas.
-- 4. Combinar las sublistas y el pivote en una lista ordenada.

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

-- Pensando de forma recursiva
-- Hemos usado un poco la recursion y como te habras dado cuenta existen unos pasos comunes. Normalmente primero definimos los casos base y luego definimos una funcion que hace algo entre un elemento y la funcion aplicada al resto de elementos.
-- Por supuesto tambien existen los casos base. Por lo general un caso base es un escenario en el que la aplicacion de una recursion no tiene sentido. Cuando trabajamos con listas, los casos base suelen tratar con listas vacias. Cuando utilizamos arboles los casos base son normalmente los nodos que no tienen hijos.
-- Cuando queremos resolver un problema de forma recursiva, primero pensamos donde no se aplica una solucion recursiva y si podemos utilizar esto como un caso base. Luego pensamos en las identidades, por donde deberiamos romper los parametros (por ejemplo, las lista se rompen en cabeza y cola) y en que parte deberiamos aplicar la funcion recursiva.