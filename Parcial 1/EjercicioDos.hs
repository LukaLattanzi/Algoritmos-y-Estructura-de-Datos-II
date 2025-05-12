-- 2. Dar el tipo de las siguientes funciones:

-- a) clima1 x y z = if z y then y + x else x

clima1 :: (Num a) => a -> a -> (a -> Bool) -> a
clima1 x y z = if z y then y + x else x

-- b) clima2 :: [a] -> a -> (a -> Bool) -> [a] -> a

clima2 :: [[a]] -> t -> (t -> Bool) -> [a] -> [a]
clima2 x y z f = if z y then head x else tail f
