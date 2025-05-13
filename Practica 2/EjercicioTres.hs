-- 3. Dado el tipo de dato CList

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show, Eq)

toList :: CList a -> [a]
toList cl = case cl of
  EmptyCL -> []
  CUnit x -> [x]
  Consnoc x mid y -> x : extract mid ++ [y]
  where
    extract EmptyCL = []
    extract (CUnit z) = [z]
    extract (Consnoc u m v) = u : extract m ++ [v]

-- ghci> toList (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
-- [1,2,3,4,5]
-- ghci> toList (Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6)
-- [1,2,3,4,5,6]

{-
Listas con longitud impar: En estas, el elemento central se representa con un CUnit. Esto ocurre porque el elemento central no tiene un par correspondiente en la estructura de la lista.

Listas con longitud par: En estas, la sublista central se representa con un EmptyCL. Esto ocurre porque no hay un único elemento central, y la estructura se divide simétricamente.
-}

fromList :: [a] -> CList a
fromList [] = EmptyCL
fromList [x] = CUnit x
fromList xs =
  let (h : rest) = xs
   in Consnoc h (fromList (init rest)) (last xs)

-- ghci> fromList [1,2,3,4,5]
-- Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5
-- ghci> fromList [1,2,3,4,5,6]
-- Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6

-- a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:

-- 1. Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

headCL :: CList a -> a
headCL EmptyCL = error "headCL: lista vacía"
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

-- ghci> headCL (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
-- 1
-- ghci> headCL (Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6)
-- 1

tailCL :: CList a -> CList a
tailCL EmptyCL = error "tailCL: lista vacía"
tailCL (CUnit _) = EmptyCL -- Si es una lista con un solo elemento, el resultado es una lista vacía.
tailCL (Consnoc _ EmptyCL y) = CUnit y -- Si la sublista es vacía, queda solo el último elemento.
tailCL (Consnoc _ (CUnit z) y) = Consnoc z EmptyCL y -- Si la sublista tiene un único elemento, ajustamos la estructura.
tailCL (Consnoc _ mid y) = Consnoc (headCL mid) (tailCL mid) y -- Caso general: ajustamos la sublista recursivamente.

-- ghci> tailCL (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
-- Consnoc 2 (Consnoc 3 EmptyCL 4) 5
-- ghci> tailCL (Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6)
-- Consnoc 2 (Consnoc 3 (CUnit 4) 5) 6

-- b) Definir una función reverseCL que toma una CList y devuelve su inversa.

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x mid y) = Consnoc y (reverseCL mid) x

-- ghci> reverseCL (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
-- Consnoc 5 (Consnoc 4 (CUnit 3) 2) 1
-- ghci> reverseCL (Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6)
-- Consnoc 6 (Consnoc 5 (Consnoc 4 EmptyCL 3) 2) 1

-- c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = [] : map (head xs :) (inits (tail xs))

-- inits [1,2,3] == [[], [1], [1,2], [1,2,3]]

initsCL :: CList a -> CList (CList a)
initsCL cl =
  let listOfLists = inits (toList cl) -- Aplicamos inits a la lista normal
      listOfCLists = map fromList listOfLists -- Convertimos cada lista interna a un CList
   in fromList listOfCLists -- Convertimos la lista de CLists a un CList

-- ghci> initsCL (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
--- Consnoc EmptyCL (Consnoc (CUnit 1) (Consnoc (Consnoc 1 EmptyCL 2) EmptyCL (Consnoc 1 (CUnit 2) 3)) (Consnoc 1 (Consnoc 2 EmptyCL 3) 4)) (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)

-- d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)

-- tails [1,2,3] == [[1,2,3], [2,3], [3], []]

lastsCL :: CList a -> CList (CList a)
lastsCL cl =
  let listOfLists = tails (toList cl) -- [[a]]
      listOfCLists = map fromList listOfLists -- [CList a]
   in fromList listOfCLists -- CList (CList a)

-- ghci> lastsCL (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5)
-- Consnoc (Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5) (Consnoc (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) (Consnoc (Consnoc 3 (CUnit 4) 5) EmptyCL (Consnoc 4 EmptyCL 5)) (CUnit 5)) EmptyCL

-- e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas

concatCL :: CList (CList a) -> CList a
concatCL cl =
  let listOfLists = map toList (toList cl) -- Convertimos cada CList interno a una lista normal
      concatenatedList = concat listOfLists -- Concatenamos todas las listas normales
   in fromList concatenatedList -- Convertimos la lista concatenada de vuelta a un CList

{-
ghci> let cl1 = fromList [1, 2]
> Cosnoc 1 EmptyCL 2
ghci> let cl2 = fromList [3, 4, 5]
> Consnoc 3 (CUnit 4) 5
ghci> let cl3 = fromList [6]
> CUnit 6

ghci> let clOfClists = fromList [cl1, cl2, cl3]
> Consnoc (Consnoc 1 EmptyCL 2) (CUnit (Consnoc 3 (CUnit 4) 5)) (CUnit 6)

ghci> let concatenated = concatCL clOfClists
> Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6

ghci> toList concatenated
> [1,2,3,4,5,6]
-}