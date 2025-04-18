-- Funciones Currificadas
-- Oficialmente cada funcion de Haskell solo puede tomar un parametro. Todas las funciones que hemos usado hasta el momento y aceptaban mas de un parametro han sido funciones currificadas. Que significa esto? Que cada funcion que acepta mas de un parametro en realidad es una funcion que acepta un solo parametro y devuelve otra funcion que acepta el segundo parametro. Por ejemplo, la funcion suma (+) es una funcion currificada. Si le pasamos un numero a esta funcion, nos devuelve otra funcion que acepta otro numero y devuelve la suma de ambos.

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Que es lo que realmente pasa cuando realizamos multThree 3 5 9 o ((multThree 3) 5) 9. 
-- Primero, 3 es aplicado a multThree, esto crea una funcion que toma un parametro y devuelve una funcion. Luego 5 es aplicado a esta, de forma que se creara una funcion que toma un parametro y lo multiplica por 15. 9 es aplicado a esa funcion y el resultado es 135. 
-- Recuerda que el tipo de esta funcion tambien podria escribirse como multThree :: (Num a) => a -> (a -> (a -> a)).

