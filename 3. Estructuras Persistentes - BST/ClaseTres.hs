import Control.Concurrent.STM (check)
import Prelude hiding ((++))

-- Que es una estructura de datos? Las listas, colas, pilas, arboles, etc. son estructuras de datos.

{-
Una estructura de datos queda definida si damos:
    El conjunto de valores que puede tomar.
    Un conjunto de operaciones definidas sobre estos valores.
    Un conjunto de propiedades que relacionan todo lo anterior.
-}
-- Muchos de los algoritmos mas difundidos entan escritos para estructuras efimeras.

{-
En las estructuras efimeras, cualquier cambio significa eliminar informacion.
Las estructuras persistentes, soportan un historial de versiones.
La flexibilidad de las estructuras persistentes tienen un costo:
    Debemos adaptar las estructuras y algoritmos al modelo persistente. A veces no es posible.
    La eficiencia de algunos algoritmos para estructuras efimeras, no se van a poder alcanzar con modelos persistentes.
-}

-- En un lenguaje funcional puro, todas las estructuras son persistentes.

{-
Las estructuras persistentes al modificarse no se sobrescriben, se duplican los datos y se modifica la copia.
Las diferentes versiones versiones comparten la informacion, a esto se le llama sharing.
El manejo automatico de la memoria es imprescindible, para esto es necesario implementar un garbage collection.
-}

{-
Listas simplemente enlazadas efimeras:
    Dadas dos listas xs e ys, al concatenarlas zs = xs ++ ys, la operacion elimina las listas xs e ys.
    La operacion zs = xs ++ ys no recorrio ningun elemento de las listas.
-}

{-
Listas simplemente enlazadas persistentes:
    Dadas dos listas xs e ys, al concatenarlas zs = xs ++ ys, la operacion mantiene las listas xs e ys y crea una nueva zs.
    Se utilizo tiempo para copiar todos los elementos de xs, donde la operacion zs = xs ++ ys tomo un tiempo proporcional a la longitud de xs.
-}

-- En haskell, la concatenacion la definimos como:

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

{-
> [1,2,3] ++ [4,5,6]
= 1 : (2 : (3 : [4,5,6]))
-}

{-
Estructuras de datos Arboles:
Un arbol es una estructura de dato donde la informacion almacenada se conecta sin formar ciclos mediante ramas.
Esas conexiones cumple la relacion padre-hijo.
El primer nodo del arbol se lo denomina raiz y a los ultimos se los llama hojas.
Todos los demas puntos de ramificacion se denominaran nodos.
-}

-- Un arbol binario es un arbol en el que cada nodo pued tener como maximo dos hijos.

-- En haskell representamos un arbol binario con la siguiente tipo de datos.

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show, Eq)

-- Definimos funciones sobre los arboles mediante pattern-matching y recursion.

member :: (Eq t) => t -> Bin t -> Bool
member a Hoja = False
member a (Nodo izq x der) = a == x || member a izq || member a der

-- Cuantos elementos visita member para encontrar el elemento buscado?

{-
En el peor de los casos, el arbol es una lista enlazada, y la funcion member recorre todos los elementos del arbol.
En el mejor de los casos, el arbol es una lista enlazada, y la funcion member recorre todos los elementos del arbol.
-}

-- Arboles binarios de busqueda (BST):

{-
Los arboles binarios de busqueda son arboles ordenados.
El elemento de cada nodo es mayor que los elementos del subarbol izquierdo y menor que los elementos del subarbol derecho.
Supone definida una relacion de orden entre los elementos de sus nodos.
Puede haber distintos BST para un mismo conjunto de elementos.
-}

{-
Con mas precision, un arbol binario de busqueda (BST) es un arbol binario t tal que:
    Si t es una Hoja es un BST.
    Si t es un nodo (l a r), tanto l como r tienen que ser BST, y se tiene que cumplir que:
        si y es un valor en algun nodo de l, entonces y <= a
        si y es un valor en algun nodo de r, entonces a < y
-}

{-
Árbol Ejemplo:
    5
   / \
  3   7
 / \   \
2   5   8
-}

-- Cargar arbol de ejemplo:
-- Arbol :: Bin Int
-- Arbol = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 5 Hoja)) 5 (Nodo Hoja 7 (Nodo Hoja 8 Hoja))

-- Tipos de recorridos de un arbol binario:

{-
Preorden: (raíz, izquierdo, derecho). Para recorrer un árbol binario no vacío en preorden, se deben realizar las siguientes operaciones recursivamente en cada nodo, comenzando con el nodo de raíz:
    Visite la raíz
    Atraviese el sub-árbol izquierdo
    Atraviese el sub-árbol derecho
-}

preorder :: Bin a -> [a]
preorder Hoja = []
preorder (Nodo l a r) = [a] ++ preorder l ++ preorder r

{-
Inorden: (izquierdo, raíz, derecho). Para recorrer un árbol binario no vacío en inorden (simétrico), se deben realizar las siguientes operaciones recursivamente en cada nodo:
    Atraviese el sub-árbol izquierdo
    Visite la raíz
    Atraviese el sub-árbol derecho
-}

inorder :: Bin a -> [a]
inorder Hoja = []
inorder (Nodo l a r) = inorder l ++ [a] ++ inorder r

{-
Postorden: (izquierdo, derecho, raíz). Para recorrer un árbol binario no vacío en postorden, se deben realizar las siguientes operaciones recursivamente en cada nodo:
    Atraviese el sub-árbol izquierdo
    Atraviese el sub-árbol derecho
    Visite la raíz
-}

postorder :: Bin a -> [a]
postorder Hoja = []
postorder (Nodo l a r) = postorder l ++ postorder r ++ [a]

-- Re implementamos member para un BST:

member' :: (Ord t) => t -> Bin t -> Bool
member' a Hoja = False
member' a (Nodo l b r)
  | a == b = True
  | a > b = member' a l
  | otherwise = member' a r

-- Cuantos elementos visita member para encontrar el elemento buscado?

{-
En el mejor caso:
    El elemento buscado esta en la raiz del arbol.
    Mejor caso: O(1) (el elemento está en la raíz).

En el peor caso:
    El elemento esta en la ultima posicion o no esta presente.
    Peor caso: O(h), donde h es la altura del árbol.
        En un árbol degenerado, h = n (número de nodos), por lo que la complejidad es O(n).
        En un árbol balanceado, h ≈ log n, por lo que la complejidad es O(log n).

En el caso promedio:
    Si el arbol esta balanceado, la cantidad de nodos visitados es proporcional a la altura del arbol, que en un arbol balanceado es aproximadamente O(log n), donde n es el numero de nodos en el arbol.
-}

-- El valor minimo en un BST:

minimum' :: Bin a -> a
minimum' (Nodo Hoja a r) = a
minimum' (Nodo l a r) = minimum' l

-- El valor maximo en un BST:

maximum' :: Bin a -> a
maximum' (Nodo l a Hoja) = a
maximum' (Nodo l a r) = maximum' r

-- Implementar checkBST :: Bin a -> Bool

checkBST :: (Ord a) => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo l a r) =
  (l == Hoja || maximum' l < a) -- Verifica que el máximo del subárbol izquierdo sea menor que el valor del nodo actual
    && (r == Hoja || minimum' r > a) -- Verifica que el mínimo del subárbol derecho sea mayor que el valor del nodo actual
    && checkBST l -- Verifica que el subárbol izquierdo sea un BST
    && checkBST r -- Verifica que el subárbol derecho sea un BST

-- Insercion en un BST:

-- Para insertar, recorremos el arbol hasta encontrar una hoja, que transformamos en un nuevo nodo.

insert :: (Ord t) => t -> Bin t -> Bin t
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r)
  | a <= b = Nodo (insert a l) b r
  | otherwise = Nodo l b (insert a r)

-- Para borrar un elemento en un BST primero lo tengo que encontar:

delete :: (Ord a) => a -> Bin a -> Bin a
delete _ Hoja = Hoja
delete z (Nodo l b r)
  | z < b = Nodo (delete z l) b r -- Buscar en el subárbol izquierdo
  | z > b = Nodo l b (delete z r) -- Buscar en el subárbol derecho
  | z == b = deleteaux z (Nodo l b r) -- Encontrado, proceder a eliminar

deleteaux :: (Ord a) => a -> Bin a -> Bin a
deleteaux _ (Nodo Hoja _ Hoja) = Hoja -- Caso 1: Nodo hoja, eliminarlo
deleteaux _ (Nodo Hoja _ r) = r -- Caso 2: Solo tiene hijo derecho, reemplazar por el hijo derecho
deleteaux _ (Nodo l _ Hoja) = l -- Caso 3: Solo tiene hijo izquierdo, reemplazar por el hijo izquierdo
deleteaux _ (Nodo l _ r) =
  let y = minimum' r -- Caso 4: Tiene ambos hijos, encontrar el mínimo del subárbol derecho
   in Nodo l y (delete y r) -- Reemplazar el nodo por el mínimo encontrado y eliminarlo del subárbol derecho

-- El tiempo de ejecucion de las operaciones depende de la altura del arbol.
-- Los BST pueden degenerar en una lista si los datos se insertan en orden, si esto ocurre, su rendimiento no sera mejor que una lista.
-- Existen implementaciones de BST que nos aseguran arboles bajos (balanceados): AVL y arboles Rojos y Negros.