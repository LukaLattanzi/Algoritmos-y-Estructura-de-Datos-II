-- 1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios. Cualquier otro color se expresa en terminos de las proporciones de estos tres colores que es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada color de manera biunivoca, por lo que usualmente se utilizan estos valores como representacion de un color.

-- Definir un tipo Color en este modelo y una funcion mezclar que permita obtener el promedio componente a componente entre dos colores.

type ColorRGB = (Int, Int, Int)

mezclar :: ColorRGB -> ColorRGB -> ColorRGB
mezclar (r1, g1, b1) (r2, g2, b2) = (promedio r1 r2, promedio g1 g2, promedio b1 b2)
  where
    promedio x y = div (x + y) 2