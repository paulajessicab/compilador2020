**Máquina abstracta ->** máquina que opera sobre sintaxis
**Máquina virtual ->** máquina que opera sobre instrucciones de un lenguaje de bajo nivel

Se implementa la traducción al código de una máquina virtual (BVM) de nivel más bajo. El control no es un término de PCF, es una secuencia de instrucciones llamada bytecode.

Esta será una máquina cuyos estados están compuestos por una secuencia de instrucciones, un entorno y una pila de valores.

Para las operaciones aritméticas se usa notación polaca inversa.

Para permitir los saltos (por ejemplo, al llamar a una función o para recorrer solo una de las ramas de un ifz), se guardan como datos extra ciertas longitudes.

1) Se implementó la compilación a bytecode en Bytecompile.hs.
2) Se implementa el esquema de compilación de IFZ bc(ifz c then a else b) del siguiente modo:
    a) Se traduce la condición
    b) Se intruce el opcode IFZ
    c) Se introduce la longitud de (bc a) + 2, por el JUMP y la longitud de la segunda rama.
    d) Se introduce la traducción de la primer rama
    e) Intruduce el opcode JUMP y la longitud de la segunda rama
    f) Se traduce la segunda rama.
    
    Si c == 0, se ejecuta bc a, luego de eso llega al JUMP y esto produce un salto de longitud de (bc b) que evita la ejecución de la segunda rama.
    Si c != 0, se salta longitud de (bc a) + 2 para evitar la ejecución de la primer rama y continuar con la ejecución de (bc b).

3) Se implementaron let bindings internos de forma eficiente agregando las instrucciones SHIFT y DROP.

Notas:
    * Se eliminó el opcode de FIXPOINT y se implementó el operador FIX.
    * Más adelante se pide reemplazar la implementación de los operadores unarios, se deja comentada la implementación original.
    * Se modifica Main.hs según lo solicitado.