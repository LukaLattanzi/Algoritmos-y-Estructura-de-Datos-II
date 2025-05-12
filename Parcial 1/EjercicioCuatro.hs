-- 4. Un gestor de proyecto asigna a cada tarea un identificador y un peso. Mientras mayor peso mas importante la tarea y se debe realizar primero. Para implementar el gestor se te pide que:

type Peso = Int

type Id = String

type Tarea = (Id, Peso)

data Gestor = Vacio | GS Gestor Tarea Gestor deriving (Eq, Show)

-- Definir la funcion agenda que dado un Gestor retorna una lista de IDs ordenados del mas al menos urgente, si hay empate se hace el que llego primero.

inorder :: Gestor -> [Tarea]
inorder Vacio = []
inorder (GS l t r) = inorder l ++ [t] ++ inorder r

agenda :: Gestor -> [Id]
agenda Vacio = []
agenda (GS l (id, p) r) = agenda r ++ [id] ++ agenda l