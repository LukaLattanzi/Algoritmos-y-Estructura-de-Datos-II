{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use list comprehension" #-}

-- 4. Dar el tipo de la siguiente funciones o expresiones:

-- a) foo1 p = if p then (p &&) else (p &&)

foo1 :: Bool -> Bool -> Bool
foo1 p = if p then (p &&) else (p &&)

-- b) foo2 x y z = x (y z)

foo2 :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
foo2 x y z = x (y z)

-- c) foo3 x y z = x y z

foo3 :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
foo3 x y z = x y z

-- d) foo4 x y z = x y : z

foo4 :: (t -> a) -> t -> [a] -> [a]
foo4 x y z = x y : z

-- e) foo5 x y z = x : y z

foo5 :: a -> (t -> [a]) -> t -> [a]
foo5 x y z = x : y z

-- f) foo6 x y z = x ++ y z

foo6 :: [a] -> (t -> [a]) -> t -> [a]
foo6 x y z = x ++ y z

-- g) foo7 a b = if b a then head a else []

foo7 :: [[a]] -> ([[a]] -> Bool) -> [a]
foo7 a b = if b a then head a else []

-- h) foo8 a b = if b a then a else []

foo8 :: [a] -> ([a] -> Bool) -> [a]
foo8 a b = if b a then a else []

-- i) foo9 a b = if b a then [a] else []

foo9 :: a -> (a -> Bool) -> [a]
foo9 a b = if b a then [a] else []