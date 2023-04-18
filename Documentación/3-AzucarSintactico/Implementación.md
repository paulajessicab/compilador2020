Video recomendado: https://www.youtube.com/watch?v=43XaZEn2aLc


Se define la sintaxis azucarada PCF1.
Se agrega sintáxis redundante para hacer más fácil la escritura y lectura de los programas.

Se modifica el archivo Lang.hs para agregar un shallow AST (STm + SDecl) con nodos que representan los términos y declaraciones con syntactic sugar.
Se modifica el parser para que trabaje sobre ese shallow AST. 
En el archivo Elab.hs, se implementa la etapa de desugaring (STerm a NTerm) y luego la de elaboración (NTerm a Term).

* Elementos de SS implementados:

Términos let.. in
1) Se agregó el parser ´termLet´
2) Se agrega el nodo ´SLet info Name [(Name, STy)] STy (STm info var) (STm info var)´
3) Se implementa desugaring

Funciones de varios argumentos (en términos y declaraciones):
1) En el shallow AST se permite una lista de bindings [(Name, Ty)]
2) En el parser se implementa ´binders´
3) La etapa de desugaring reemplaza el listado de binders por la aplicación sucesiva de nodos internos.

Funciones recursivas (en términos y declaraciones):
1) En el shallow AST se agrega un nodo ´SLetRec info Name [(Name, STy)] STy (STm info var) (STm info var)´
2) Se agregan al parser ´termLetRec´ y ´declRec´.
3) Se agrega el desugaring correspondiente. let rec requiere al menos un argumento para ser transformado en fix en la etapa de desugaring, si no lo tiene sale con failPosPCF.

Ya no se permiten declaraciones sin tipos.

Se permiten operadores unarios sin aplicar (hacemos la eta-expansión solo si no están aplicados).
1) Se agrega al shallow AST el nodo ´SUnaryOp info UnaryOp (Maybe (STm info var))´
2) Se modifica el parser para permitir operadores unarios sin aplicar.
3) Se realiza el desugaring, si es ´SUnaryOp i op Nothing´ se eta-expande.

Se agregan sinónimos de tipo.
1) Se agrega a SDecl el nodo ´STypeAlias { aliasPos :: Pos, aliasName :: Name, aliasTy :: STy }´
2) Se agrega a MonadPCF un entorno para los sinónimos de tipos. Se agregan las operaciones para manipular dicho entorno.
3) En la etapa de desugaring, se usa el entorno de sinónimos de tipos para transformarlo en un tipo concreto.

Se agregan multibinders.

1) Se modifica binding para tomar una lista de nombres (ej. x, y, z) y un tipo (ej. Nat). Luego mapea la lista de nombres con el tipo para devolver [(Name, Ty)] (ej. [(x, Nat), (y, Nat), (z, Nat)]). Esto se hace directamente en el parser, no se modifica el shallow AST.