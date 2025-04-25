-- Arboles Balanceados

-- Las operaciones de busqueda, insercion y borrado son del orden de la altura del arbol.

-- Si insertamos datos en orden, la altura del arbol es maxima y los rendimientos son los de una lista.

-- Si mantenemos la altura lo mas baja posible nuestros algoritmos son mas rapidos.

-- Para lograr esto debemos mantener nuestros arboles balanceados.

-- La altura del arbol debe estar en el orden del log n.

-- Red Black Tree, Leftist Heaps, Maxiphobic Heaps, ...

-- Red Black Tree

-- Es un BST cuyos nodos estan coloreados de Rojo o de Negro:

data Color = Red | Black

data RedBlackTree a = Empty | Node Color (RedBlackTree a) a (RedBlackTree a)

-- Invariantes:

-- 1) Todos los nodos rojos tienen un padre negro. (Local)

-- 2) Todos los caminos de la raiz a una hoja tienen el mismo numero de nodos negros (altura negra). (Global)

-- Esto significa que la altura siempre esta en el orden del log n.

exampleRedBlackTree :: RedBlackTree Int
exampleRedBlackTree = Node Black (Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Red (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty))

{-
        4 (Black)
       /         \
    2 (Red)      6 (Red)
   /   \        /   \
1 (Black) 3 (Black) 5 (Black) 7 (Black)
-}
{-
Verificación de las invariantes:
Invariante local:

Todos los nodos rojos (2 y 6) tienen un padre negro (4).
Los nodos negros (1, 3, 5, 7) no tienen restricciones adicionales.
Cumple la invariante local.
Invariante global:

Todos los caminos desde la raíz hasta las hojas (Empty) tienen el mismo número de nodos negros:
Camino izquierdo: 4 (Black) → 2 (Red) → 1 (Black) → Empty → 2 nodos negros.
Camino izquierdo-derecho: 4 (Black) → 2 (Red) → 3 (Black) → Empty → 2 nodos negros.
Camino derecho: 4 (Black) → 6 (Red) → 5 (Black) → Empty → 2 nodos negros.
Camino derecho-derecho: 4 (Black) → 6 (Red) → 7 (Black) → Empty → 2 nodos negros.
Todos los caminos tienen la misma cantidad de nodos negros.
Cumple la invariante global.
-}

-- Implementamos member para RBTs.

memberRBT :: (Ord a) => a -> RedBlackTree a -> Bool
memberRBT _ Empty = False
memberRBT a (Node _ l x r)
  | a == x = True
  | a < x = memberRBT a l
  | a > x = memberRBT a r