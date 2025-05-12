-- 1. Dar almenos dos ejemplos de funciones que tengan los siguientes tipos:

-- a) (Bool -> Bool) -> (Int -> Int)

foo1 :: (Bool -> Bool) -> (Int -> Int)
foo1 f n = if f (n >= 1) then 1 else 0

foo2 :: (Bool -> Bool) -> (Int -> Int)
foo2 f n = if f (n == 0) then 0 else 1

-- b) Bool -> (Int -> Int)

foo3 :: Bool -> (Int -> Int)
foo3 b n = if b then n + 1 else n - 1

foo4 :: Bool -> (Int -> Int)
foo4 b n = if b then n * 2 else n `div` 2