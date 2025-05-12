-- 3. La funcion splitCond toma un predicado (condicion) y una lista, retorna una tupla de listas donde la primera componente tiene los elementos que cumplen con el predicado y en la segunda los que no. Ejemplo: splitCond even [1..10] = ([2,4,6,8,10],[1,3,5,7,9]) Definir la funcion splitCond:

-- a) recursivamente sin usar funciones predefinidas y sin usar foldr

splitCond :: (a -> Bool) -> [a] -> ([a], [a])
splitCond p [] = ([], [])
splitCond p (x : xs)
  | p x = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where
    (ys, zs) = splitCond p xs

-- b) usando foldr dando la funcion asociada al foldr como una funcion anonima

splitCond' :: (Foldable t) => (a -> Bool) -> t a -> ([a], [a])
splitCond' p = foldr (\x (ys, zs) -> if p x then (x : ys, zs) else (ys, x : zs)) ([], [])

-- c) usando listas por comprension sin usar funciones predefinidas, salvo operadores logicos

splitCond'' :: (a -> Bool) -> [a] -> ([a], [a])
splitCond'' p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])