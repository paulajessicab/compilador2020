Cómo IR sigue teniendo operaciones complejas (expresiones aritméticas, condicionales, etc.) sigue siendo un árbol y es de muy alto nivel como para compilarse directamente a código máquina.

El código de bajo nivel se organiza en bloques básicos.

**Bloque básico:**
Cada bloque básico consiste en:
* Una etiqueta / dirección de memoria / locación (Loc)
* Una secuencia de instrucciones simples (Inst)
* Un terminador -salto o retorno de función- (Terminator)

`type BasicBlock = (Loc, [Inst], Terminator)`

**Instrucciones:**
* asignación a registro temporario
* asignacion a una posición de memoria 

**Observaciones:**
* En cualquier compilador, primero se asume una cantidad invinita de registros temporarios. Luego se determina cómo adaptarse a la cantidad limitada de registros reales.
* En esta etapa las expresiones solo operan sobre valores y etiquetas (no son recursivas).
* A diferencia de assembler normal, LLVM tiene registros y operaciones tipadas.
* El código LLVM tiene que estar en SSA.

**Static Single Assignment (SSA):**
Cada variable puede estar definida en un único lugar. Se puede usar en más de un lugar. En un lenguaje funcional de alto nivel, SSA se da de forma natural.
En general, cuando se encuentran dos definiciones con el mismo nombre, se le asigna a una un nombre fresco y reemplazamos ese nombre en todos los usos en los que corresponda por scope.

**Implementación:**
* La traducción de código intermedio a bajo nivel se hace mediante la función runCanon en CIR.hs. (Genera CanonProg)
* Para representar el código LLVM se usa el paquete llvm-hs-pure.
* InstSel.hs toma un CanonProg y devuelve un módulo de LLVM.
* La ejecución de código LLVM se hace por fuera (clang). Se agrega una llamada a system desde Main.hs para poder hacerlo desde dentro del compilador.
* Se usa Phi para traducir a IFZ forzando SSA. Evitamos la definición de una misma variable en el then y en el else.
* La función Phi es un operador que, dado t = ifz c then a else b, asigna a t el valor a si vengo de then y b si vengo de else. El assembler de LLVM ya la provee.