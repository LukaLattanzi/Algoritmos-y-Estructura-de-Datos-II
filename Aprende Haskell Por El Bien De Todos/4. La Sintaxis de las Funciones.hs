-- Guardas

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
  | bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
  | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
  | otherwise = "¡Enhorabuena, eres una ballena!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- Las variables where son una manera de definir variables locales dentro de una función. Se definen al final de la función, y se pueden usar en cualquier parte de la función.

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- Let it be
-- Las expresiones let sirven para ligar variables a cualquier lugar y son expresiones en si misma, pero son muy locales, asi qu eno pueden extenderse en guardas.

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- Su forma es let <definicion> in <expresion>. Las variables que definamos en la expresion let

-- Expresion case
-- Las expresiones case son como las expresiones if else o las expreciones let. No solo podemos evaluar expresiones basandonos en los posibles valores de una varuable sino que podemos realiazar un ajuste de patrones.

head'' :: [a] -> a
head'' [] = error "¡head no funciona con listas vacías!"
head'' (x:_) = x

head' :: [a] -> a
head' xs = case xs of
  [] -> error "¡head no funciona con listas vacías!"
  (x : _) -> x