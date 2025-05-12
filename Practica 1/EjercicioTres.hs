{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- 3. Dar dos ejemplos de funciones que tengan los siguientes tipos:

-- a) (Int -> Int) -> (Bool -> Bool)

foo1 :: (Int -> Int) -> (Bool -> Bool)
foo1 f b = if b then f 0 == 0 else f 1 == 1

foo2 :: (Int -> Int) -> (Bool -> Bool)
foo2 f b = if b then f 0 > 0 else f 1 < 0

-- b) Bool -> (Int -> Bool)

foo3 :: Bool -> (Int -> Bool)
foo3 b = if b then even else odd

foo4 :: Bool -> (Int -> Bool)
foo4 b = if b then (> 0) else (< 0)

-- c) Char -> Char

foo5 :: Char -> Char
foo5 c = if c == 'a' then 'b' else 'c'

foo6 :: Char -> Char
foo6 c = if c == 'z' then 'y' else 'x'

-- d) Int -> (Int -> Bool) -> [Int]

foo7 :: Int -> (Int -> Bool) -> [Int]
foo7 n f = [x | x <- [1 .. n], f x]

foo8 :: Int -> (Int -> Bool) -> [Int]
foo8 n f = [x | x <- [1 .. n], not (f x)]

-- e) [a] -> (a -> [b]) -> [b]

foo9 :: [a] -> (a -> [b]) -> [b]
foo9 xs f = concat [f x | x <- xs]

foo10 :: [a] -> (a -> [b]) -> [b]
foo10 xs f = concat [f x | x <- xs, not (null (f x))]

-- f) [[a]] -> (a -> Bool) -> [a]

foo11 :: [[a]] -> (a -> Bool) -> [a]
foo11 xss f = concat [filter f xs | xs <- xss]

foo12 :: [[a]] -> (a -> Bool) -> [a]
foo12 xss f = concat [filter (not . f) xs | xs <- xss]

-- g) (a, b, c) -> Bool

foo13 :: Eq a => (a, a, c) -> Bool
foo13 (x, y, z) = x == y

foo14 :: Eq a => (a, b, a) -> Bool
foo14 (x, y, z) = x == z

-- h) (a, b, c) -> Int -> c

foo15 :: (a, b, c) -> Int -> c
foo15 (_, _, z) _ = z

foo16 :: (a, b, c) -> Int -> c
foo16 (_, _, z) n = if n > 0 then z else error "n debe ser mayor que 0"

-- i) (a, a, a) -> Int -> a

foo17 :: (a, a, a) -> Int -> a
foo17 (x, _, _) n = if n > 0 then x else error "n debe ser mayor que 0"

foo18 :: (a, a, a) -> Int -> a
foo18 (_, y, _) n = if n > 0 then y else error "n debe ser mayor que 0"