Implementación:

Lang.Tm representa términos fully named y locally nameless.
- Términos Fully Named: Lang.NTerm es Lang.Tm con var = Name (String)
- Términos Locally Nameless: Lang.Term es Lang.Tm con var = Var (Free vars con nombre y bounded vars con índices de De Brujin).

En el archivo Subst.hs se encuentran las implementaciones de close, open y subst, openN, closeN y substN.
subst P Q sustituye el primer índice libre de P por Q y no requiere un nombre fresco.
fix bindea de a dos variables a la vez. Debemos abrirlas y cerrarlas juntas para mantener los términos locally closed.

Hay que contar índices?

La implementación de subst sí cuenta índices para saber qué índices sustituir, pero no hay lógica para evitar capturas: están imposibilitadas por la representación. Esta complejidad queda totalmente contenida en este módulo. El resto de los módulos siempre manipula términos LC.