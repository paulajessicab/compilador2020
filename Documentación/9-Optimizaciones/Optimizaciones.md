**Optimizaciones**

**Fundamentos:**

Mientras que los programadores se deben preocupar por escribir programas limpios y modulares en alto nivel, los compiladores buscan generar código ensamblador lo más eficiente posible.


Tipos de optimizaciones según el recurso que impactan:
* Temporales: reducen el tiempo de ejecución
* Espaciales: reducen la cantidad de memoria necesaria
* Energéticas: reducen la cantidad de energía necesaria

Las optimizaciones son transformaciones del código.

Pueden ser aplicadas en cualquier etapa del compilador.

Tienen que ser seguras, es decir, no deben cambiar el significado del programa.

Generalmente, requieren algún tipo de análisis del programa para determinar si la transformación es segura o si tiene un costo-beneficio útil.

No garantiza que realmente va a mejorar la performance ni que se va a generar código óptimo.

Técnicas de optimización aplicadas en el presente trabajo:

* Constant Folding:
Si los operandos de una operación se conocen en tiempo de compilación, se aplica la operación de forma estática.

Ejemplo:
`let ans : Int = (1 + 2) + y`
quedaría
`let ans : Int = 3 + y`

* Algebraic Simplification
Se utilizan reglas de simplificación algebraicas.

Ejemplo:
`let ans : Int =  y + 0`
quedaría
`let ans : Int = y`

* Constant Propagation
Si el valor de una variable es una constante, se reemplaza el uso de esa variable por la constante.
El valor de la variable debe propagarse a partir del punto de asignación. Es una operación de sustitución.

* Copy propagation

Si una variable es asignada a otra, reemplaza los usos de la variable asignada con la copiada.
Hay que tener en cuenta las reglas de scoping del lenguaje.

* Inlining

Reemplazar la llamada a una función por el cuerpo de la función. Hay que reescribir los argumentos para que sean variables locales.
Elimina saltos y manipulaciones del stack. Puede que se necesite renombre de variables para evitar capturas.
Hay que tener cuidado al hacer inlining de funciones recursivas. Se puede hacer una reescritura usando un loop pre-header.

Desventajas:
Incrementa el tamaño del código y esto puede impactar a nivel caché de procesador (+ cache misses)
Si no se tiene cuidado, puede producirse una explosión en el tamaño del código.

Heurísticas para saber cuándo hacer inlining, evitando la explosión de código:
1. Expandir solo las funciones que se llaman muy frecuentemente.
2. Expandir solo las funciones con cuerpos pequeños
3. Expandir las funciones que se llaman solo una vez (criterio elegido para este proyecto).

* Dead Code Elimination

Si una declaración libre de efectos secundarios nunca se puede observar, es seguro eliminarla.
Una variable está muerta si nunca se usa luego de que fue definida.
Las variables muertas pueden ser creadas como consecuencia de otras optimizaciones.
Solo es seguro aplicarlo si ese código es puro (no tiene efectos secundarios visibles externamente).


Fuentes: 
CS153: Compilers - Lecture 19: Optimization. Stephen Chong. Harvard. https://canvas.harvard.edu/courses/92158
Modern Compiler Implementation in ML. Andrew W. Appel.

**Decisiones de diseño:**

* Se implementaron las optimizaciones mencionadas (Constant Folding, Algebraic Simplification, Constant Propagation, Inlining, Dead Code Elimination)
* Para reducir el número de pasadas necesarias, Constant Folding, Algebraic Simplification, Constant Propagation e Inlining se hacen en la misma pasada.
* La simplificación algebráica solo tuvo en cuenta el resultado de operar con 0 dentro de los naturales (neutro para la suma, neutro a derecha para la resta y absorbente a izquierda para la resta)
* Solo se hace inlining de las funciones que se llaman solo una vez, se hace una pasada previa contando todas las llamadas.
* Para que no quede ejecutando infinitamente, se pone un límite duro en el número de iteraciones (optimizationLimit).

**Prueba 1**

Se probó la compilación del archivo Test/Cases/test_2021_11_opt0_73.pcf con optimizationLimit = 0 (muestra de control) y optimizationLimit = 5.
Se tomaron los tiempos de 10 ejecuciones de cada uno.

* optimizationLimit = 0

Código optimizado:

```
Let prd = fun (x : Nat) -> pred x
Let sc = fun (x : Nat) -> succ x
Let fib = fix (fib : Nat -> Nat) (x : Nat) ->
            ifz
              x
            then
              1
            else
              ifz prd x then 1 else fib (pred x) + fib (x - 2)
Let ack = fix (ack : Nat -> Nat -> Nat) (m : Nat) ->
            fun (n : Nat) ->
              ifz
                m
              then
                sc n
              else
                ifz n then ack (prd m) 1 else ack (prd m) (ack m (prd n))
Let dead = fun (x : Nat) -> x + x
Let l = ifz 0 then 1 else 2
Let r = ifz 1 then 1 else 2
Let n1 = 2 + succ 3 - ack 1 (succ 1)
Let big = fun (x : Nat) -> sc (sc (sc (sc (sc x))))
Let small = fix (small : Nat -> Nat) (x : Nat) -> x + 5
Let n2 = small (small (small (small (small 2)))) + big 2
Let DEBUG = 1
Let f2 = fun (x : Nat) -> x + 5
Let f1 = fun (x : Nat) -> f2 x
Let x = ifz DEBUG then f2 5 else f1 (2 + 3)
Let n3 = ack 3 2 + n2 + x 
```

Ejecuciones:

real    0m0.493s
real    0m0.537s
real    0m0.517s
real    0m0.596s
real    0m0.517s
real    0m0.502s
real    0m0.523s
real    0m0.526s
real    0m0.514s
real    0m0.529s

Promedio: 0m0.525s

* optimizationLimit = 5

Código optimizado:
```
Let prd = fun (x : Nat) -> pred x
Let sc = fun (x : Nat) -> succ x
Let small = fix (small : Nat -> Nat) (x : Nat) -> x + 5
Let n2 = small (small (small (small (small 2)))) + sc (sc (sc (sc (sc 2))))
Let f2 = fun (x : Nat) -> x + 5
Let x = ifz 1 then f2 5 else f2 5
Let n3 = (fix (ack : Nat -> Nat -> Nat) (m : Nat) ->
            fun (n : Nat) ->
              ifz
                m
              then
                sc n
              else
                ifz n then ack (prd m) 1 else ack (prd m) (ack m (prd n))) 3
                                                                           2 + n2 + x
```

Ejecuciones:

real    0m0.535s
real    0m0.553s
real    0m0.493s
real    0m0.500s
real    0m0.502s
real    0m0.522s
real    0m0.514s
real    0m0.493s
real    0m0.524s
real    0m0.485s

Promedio: 0m0.512s

**Prueba 2**

* optimizationLimit = 0

Código optimizado:

```
Let x = 1
Let y = 2 + x
Let f = fun (y : Nat) -> 1 + x
Let suma = fun (x : Nat) -> fun (y : Nat) -> x + y
Let suma5 = suma 5
Let countdown = fix (countdown : Nat -> Nat) (n : Nat) ->
                  ifz n then 0 else countdown (pred n)
Let ans = countdown 345 + 7	 
```

Ejecuciones:
real    0m0.418s
real    0m0.390s
real    0m0.389s
real    0m0.386s
real    0m0.370s
real    0m0.370s
real    0m0.380s
real    0m0.404s
real    0m0.375s
real    0m0.403s

Promedio: 0m0.388s

* optimizationLimit = 5

Código optimizado:

```
Let ans = (fix (countdown : Nat -> Nat) (n : Nat) ->
             ifz n then 0 else countdown (pred n)) 345 + 7
```

Ejecuciones:
real    0m0.360s
real    0m0.370s
real    0m0.397s
real    0m0.372s
real    0m0.401s
real    0m0.375s
real    0m0.379s
real    0m0.384s
real    0m0.391s
real    0m0.373s

Promedio: 0m0.380s

**Conclusiones:**

En las pruebas realizadas, se notó una reducción en el tamaño del código generado. Sin embargo, las reducciones en el tiempo de ejecución no fueron significativas.
Esto puede deberse probablemente a que el código inicial no era muy complejo. Para tener estadísticas más concluyentes, deberían hacerse pruebas sobre código de entrada más complejo y con un mayor número de ejecuciones.
También se podrían hacer pruebas tomando otras decisiones de implementación (por ejemplo, con otras heurísticas para inlining).

Comandos utilizados en las pruebas de performance:
time <cmd>
/usr/bin/time -v <cmd>