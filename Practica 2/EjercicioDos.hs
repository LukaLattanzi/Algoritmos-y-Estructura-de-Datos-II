-- 2. Consideremos un editor de lineas simple. Supongamos que una Linea es una secuencia de caracteres c1, c2, . . . , cn junto con una posición p, siendo 0 <= p <= n, llamada cursor (consideraremos al cursor a la izquierda de un caracter que será borrado o insertado, es decir como el cursor de la mayoría de los editores). Se requieren las siguientes operaciones sobre líneas:

{-
vacia :: Linea
moverIzq :: Linea -> Linea
moverDer :: Linea -> Linea
moverInicio :: Linea -> Linea
moverFin :: Linea -> Linea
insertar :: Char -> Linea -> Linea
borrar :: Linea -> Linea
-}

-- La descripcion informal es la siguiente: (1) la constante vacia denota la linea vacia, (2) la operacion moverIzq mueve el cursor a una posicion a la izquierda (siempre que ellos sea posible), (3) analogamente para moverDer, (4) moverInicio mueve el cursor al comienzo de la linea, (5) moverFin mueve el cursor al final de la linea, (6) la operacion borrar elimina el caracter que se encuentra a la izquierda del cursor, (7) insertar agrega un caracter en el lugar donde se encontraba el cursor y lo mueve a una posicion a la derecha.

-- Definir un tipo Linea e implementar las operaciones dadas.

type Linea = (String, Int)

vacia :: Linea
vacia = ("", 0)

moverIzq :: Linea -> Linea
moverIzq (s, p) = if p > 0 then (s, p - 1) else (s, p)

moverDer :: Linea -> Linea
moverDer (s, p) = if p < length s then (s, p + 1) else (s, p)

moverInicio :: Linea -> Linea
moverInicio (s, _) = (s, 0)

moverFin :: Linea -> Linea
moverFin (s, _) = (s, length s)

-- La función toma un carácter `c` y una línea `(s, p)`.
-- `take p s` obtiene los primeros `p` caracteres de la cadena `s` (todo lo que está antes del cursor).
-- `[c]` es el carácter que se inserta en la posición del cursor.
-- `drop p s` obtiene los caracteres desde la posición `p` en adelante (todo lo que está después del cursor).
-- Finalmente, se concatena todo y se incrementa la posición del cursor en 1.

insertar :: Char -> Linea -> Linea
insertar c (s, p) = (take p s ++ [c] ++ drop p s, p + 1)

-- La función toma una línea `(s, p)`.
-- Si el cursor está en una posición mayor a 0 (`p > 0`):
-- `take (p - 1) s` obtiene los primeros `p - 1` caracteres de la cadena `s` (todo lo que está antes del carácter a borrar).
-- `drop p s` obtiene los caracteres desde la posición `p` en adelante (todo lo que está después del carácter a borrar).
-- Se concatena todo, eliminando así el carácter a la izquierda del cursor, y se decrementa la posición del cursor en 1.
-- Si el cursor está en la posición 0, no se realiza ninguna operación y se devuelve la línea original.

borrar :: Linea -> Linea
borrar (s, p) = if p > 0 then (take (p - 1) s ++ drop p s, p - 1) else (s, p)
