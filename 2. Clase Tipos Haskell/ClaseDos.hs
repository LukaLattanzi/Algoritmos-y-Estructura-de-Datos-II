{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (Bool (..), Maybe (..), String)

-- En haskell podemos definir un nuevo nombre para un tipo existente usando una declaracion type.

type String = [Char]

-- Los sinonimos de tipo hace que ciertas declaraciones de tipos sean mas faciles de leer.

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x - 1, y)

-- Como las funciones, la declaracion de tipos puede tener parametros:

type Par a = (a, a)

mult :: Par Int -> Int
mult (x, y) = x * y

copy :: a -> Par a
copy x = (x, x)

-- Las declaraciones de tipo pueden anidarse, Type Pos = (Int, Int). Pero no pueden ser recursivos type Tree = (Int,[Tree]).

-- Un nuevo tipo completo puede ser definido dando sus valores con la declaracion data.

data Bool = False | True deriving (Eq, Show)

-- True y False son los llamdos constructores del tipo Bool, estos deben empezar con mayusculas.

data Answer = Yes | No | Unknown deriving (Eq, Show)

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

-- Los constructores en las declaraciones data pueden tener parametros.

data Shape = Circle Float | Rect Float Float deriving (Eq, Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = 3.14 * (r * r)
area (Rect x y) = x * y

-- No es sorpresa que las declaraciones data pueden tambien tener parametros de tipos.

data Maybe a = Nothing | Just a deriving (Eq, Show)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Las declaraciones data pueden ser recursivas.

data Nat = Zero | Succ Nat deriving (Eq, Show)

add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

-- Nat es un nuevo tipo con constructores Zero :: Nat y Succ :: Nat -> Nat.
-- Un valor de tipo Nat puede ser Zero o de la forma Succ n donde n :: Nat.
{-
Podemos pensar a los valores del tipo Nat como numeros naturales donde:
    Zero representa a 0 y Succ representa la funcion sucesor (1+)
    Succ (Succ (Succ Zero)) = (1 + 1 + 1 + 0) = 3
-}

subtract' :: Nat -> Nat -> Nat
subtract' Zero n = Zero
subtract' n Zero = n
subtract' (Succ m) (Succ n) = subtract' m n

-- Usando recursion es facil definir funciones que conviertan los valores entre los tipos Nat e Int.

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- Dos naturales pueden ser sumados convirtiendolos en enteros, sumarlos, y convertilos nuevamente a naturales.

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

-- Sin embargo usando recursion la misma funcion se puede definir sin la necesidad de conversion.

add'' :: Nat -> Nat -> Nat
add'' Zero n = n
add'' (Succ m) n = Succ (add'' m n)

-- Ejercicio definir la multiplicacion para Nat.

mulnat :: Nat -> Nat -> Nat
mulnat m n = int2nat (nat2int m * nat2int n)

mulnat' :: Nat -> Nat -> Nat
mulnat' _ Zero = Zero
mulnat' m (Succ n) = add' m (mulnat' m n)

-- Ejercicio definir la exponenciacion para Nat.

expnat :: Nat -> Nat -> Nat
expnat m n = int2nat (nat2int m ^ nat2int n)

expnat' :: Nat -> Nat -> Nat
expnat' _ Zero = Succ Zero
expnat' m (Succ n) = mulnat m (expnat m n)

-- Las declaraciones data pueden ser recursivas y con parametros.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Usando recursion es facil definir funciones que conviertan los valores entre los tipos List a y [a].

tolist :: List a -> [a]
tolist Nil = []
tolist (Cons x xs) = x : tolist xs

fromlist :: [a] -> List a
fromlist [] = Nil
fromlist (x : xs) = Cons x (fromlist xs)

-- Sintaxis de registro (Record), pdemos crear un tipo que describa a una persona.

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

examplePerson :: Person
examplePerson =
  Person
    { lastName = "Finklestein",
      firstName = "Buddy",
      height = 184.2,
      age = 43,
      flavor = "chocolate",
      phoneNumber = "526-2928"
    }

-- Nota que no es necesario respetar el orden en que fueron declarados los campos.

-- Ademas de pattern matching en el lado izq. de una definicion, podemos usar un expresion case.

esCero :: Nat -> Bool
esCero n = case n of
  Zero -> True
  _ -> False

-- Los patrones de los diferentes cosas son intentados en orden, se usa la identacion para marcar un bloque de casos.