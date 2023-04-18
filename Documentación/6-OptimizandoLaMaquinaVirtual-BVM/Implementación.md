**Llamada de cola:** se produce cuando una función directamente retorna el resultado de una llamada sin operar sobre el. Se puede optimizar la ejecución omitiendo la dirección de retorno intermedia, y pasando la de la función llamante en su lugar.

**Funciones recursivas de cola:** son aquellas funciones en las cuales sus llamadas recursivas ocurren solo en posición de cola.

La optimización de llamadas de cola parece no ser tan importante, pero al aplicar la misma optimización sobre funciones recursivas de cola se pueden lograr mejoras sustanciales (por ejemplo, pasar de O(n) a O(1) al implementar la suma de este modo).

La optimización se hizo implementando la operación TAILCALL y la función T(-) (donde la compilación general C(-) y la compilación de un término en posición de cola T(-) son mutuamente recursivas).

a) Se terminó de implementar la ejecución de IFZ.
b) Se modificaron todas las etapas del compilador para agregar los operadores binarios, se eliminaron los operadores unarios de la sintaxis interna y pasaron a estar como syntactic sugar.
c) Se implementó TAILCALL en la BVM de Haskell y de C (en aplicaciones, ifz y letbindings).
d) Se cambió la compilación de funciones para optimizar las llamadas de cola.
   Pruebas:
   Se generaron los archivos Test/sumaNoTailRecursive.pcf y Test/sumaTailRecursive.pcf, en donde se implementa la suma sin recursividad de cola y con recursividad de cola respectivamente. Sobre sumaTailRecursive, se hicieron pruebas aplicando la optimización en la recursión de cola y sin aplicarla.

| Operación | sumaNoTailRecursive | sumaTailRecursive (sin opt.) | sumaTailRecursive (opt.)  |
|---|---|---|---|---|
| 45678 + 58966  | 0.010s | 0.009s | 0.008s |
| 45678 + 589668 | 0.079s | 0.080s | 0.074s |
| 9999999 + 8888888 | 1.201s | 1.216s | 1.111s|

* Los valores corresponden al promedio de 10 ejecuciones.