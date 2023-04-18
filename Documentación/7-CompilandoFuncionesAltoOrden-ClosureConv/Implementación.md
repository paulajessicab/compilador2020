Lenguajes con funciones como valor (sin anidamiento) -> funciones representadas mediante punteros (C)
Lenguajes con anidamiento de funciones (sin funciones como valor) -> No es necesario crear clausuras. Las funciones llamadas dentro de otra solo pueden ser aplicadas antes de salir de la llamante. (Pascal)

Lenguajes con funciones como valor + anidamiento -> se necesitan crear clausuras pues se necesitan resolver referencias de funciones internas ligadas fuera de ella. (lenguajes funcionales de alto orden -Haskell-, Python) 

**Closure conversion (Peter Landin - 1964):** etapa en la que se convierte a las funciones de alto orden en objetos concretos, clausuras, que las representan. Todas las funciones pasar a ser top-level.

Una clausura es una estructura en memoria dinámica que consiste en:
* un puntero a código que computa el resultado (genérico)
* un entorno que le da significado a las variables libres de la función

Los accesos a variables del entorno involucran extraer valores del heap.
Los argumentos normales se pasan mediante la convención de llamadas del sistema.

Una alternativa a las clausuras, es generar código en runtime para aplicación parcial. El problema es que generar código en runtime es complejo y caro.

**Tipos de clausuras:**
* Clausuras planas: el entorno está directamente en la clausura, en vez de ser apuntado por la clausura. (Es la forma que usamos en el trabajo)
* Clausuras enlazadas: Se utilizan clausuras de las funciones de anidamiento superior. 

+ Más barato crear clausuras enlazadas (menos variables)
+ Más rápido acceder a clausuras planas (no hay que seguir una cadena de punteros)
+ En general, los compiladores modernos usan clausuras planas.

**Hoisting:**
Cuando las funciones locales ya no dependen de su entorno léxico podemos elevarlas a funciones globales.
Se toma el resultado de la conversión de clausuras y se convierte las funciones lambda dentro de las creaciones de clausuras en definiciones globales, con nombres frescos.

**Detalles de implementación:**
* Se usan clausuras planas.
* Se hace closure conversion y hoisting en la misma pasada.
* Creamos el árbol de términos intermedios IrTm y el tipo de declaraciones globales IrDecl.
* Se usa una mónada de estados para ayudarnos a generar nombres frescos. Se usa la mónada Writer para recolectar las definiciones globales.
