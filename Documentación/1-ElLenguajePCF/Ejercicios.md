- Ejercicio 1:

`let suma = fix (suma : Nat -> Nat -> Nat) (m : Nat) -> fun (n : Nat) -> ifz n then m else succ (suma m (pred n))`

Complejidad: O(n)

`let resta = fix (resta : Nat -> Nat -> Nat) (m : Nat) -> fun (n : Nat) -> ifz n then m else pred(resta m (pred n))`

Complejidad: O(n)

`let mult = fix (mult : Nat -> Nat -> Nat) (m : Nat) -> fun (n : Nat) -> ifz n then 0 else suma m (mult m (pred n))`

Complejidad: O(m.n)

`let exp = fix (exp : Nat -> Nat -> Nat) (m : Nat) -> fun (n : Nat) -> ifz n then 1 else mult m (exp m (pred n))`

Complejidad: O(n.m^2)

`let fact = fix(fact : Nat -> Nat) (m : Nat) -> ifz m then 1 else mult m (fact (pred m))`

Complejidad: O(m^3)

- Ejercicio 2a:

`let true = 0`
`let false = 1`

`let ifthenelse = fun (c : Nat) -> fun (t : Nat) -> fun (e : Nat) -> ifz c then t else e`


- Ejercicio 2b: 

`let pair = fun (x : Nat) -> fun (y : Nat) -> fun (b: Nat) -> ifthenelse b x y`

`let proj1 = fun (p : Nat -> Nat) -> p true`

`let proj2 = fun (p : Nat -> Nat) -> p false`


- Ejercicio 3:

`let gcd = fix (gcd : Nat -> Nat -> Nat) (m : Nat) -> fun (n : Nat) -> ifz n then m else (ifz m then n else (ifz (resta n m) then gcd (resta m n) n else gcd m (resta n m)))`


- Ejercicio 4:

`let rnat = fun (z : Nat) -> fun (b : Nat -> Nat -> Nat) -> fix (f : Nat -> Nat) (m : Nat) -> ifz m then z else b (f (pred m)) (pred m)`

`let sumar = fun (x : Nat) -> fun (y : Nat) -> rnat x (fun (z: Nat) -> fun (w : Nat) -> succ z) y`


- Ejercicio 5:

`let minimizadorN = fix (minimizadorN : (Nat-> Nat) -> Nat -> Nat) (f : Nat -> Nat) -> fun (n : Nat) -> ifz f n then n else minimizadorN f (succ n)`

`let minimizador = fun (f : Nat -> Nat) -> minimizadorN f 0`

let pruebaMin = fun (n : Nat) -> resta 2 n

- Ejercicio 6:

¿Es la beta-reducción una regla ecuacional válida en PCF0? Es decir, ¿son los términos (fun(x : T) -> t1) t2 y [t2/x]t1 equivalentes

La Beta-reducción no es una regla ecuacional válida en PCF0 pues para ciertos casos no cumpliría con la semántica de PCF0.

Dem/ Asumamos que la b-reducción es una regla válida para PCF0.

En particular, t2 podría ser ((\x.x) z) y y t1 podría ser \y z. z y 

La semántica operacional de la beta reducción es no determinista, es decir que puede existir más de una forma de beta reducir la expresión (\x.t1) t2. Una de las opciones sería usar la estratégia de reducción call by name (que es como la estrategia normal: el redex más superficial y más a la izquierda se evalúa primero. Por tanto, siempre que sea posible, los argumentos de una abstracción son sustituidos en su cuerpo antes de que los argumentos sean reducidos.) **pero no se realizan reducciones dentro de las abstracciones**. Como t1 no se puede reducir más, beta reducir el término completo sería hacer [t2/x]t1. Hacer esta sustitución daría como resultado \y. y (\x.x) z, y sería una forma normal dentro de los criterios de esa estratégia de reducción.
Por otro lado, la semántica de PCF0 es Call by Value, es decir que **solo las redexes más superficiales son reducidas: una redex solo se reduce cuando su expresión derecha ha sido reducida a un valor (una variable o una abstracción lambda)**, por lo tanto, la reducción que debería hacerse en el caso del término (fun(x : T) -> t1) t2 sería primero sobre t2 (lo que daría z) y luego (fun(x : T) -> t1) z (que daría como resultado \y. y z).


---
https://es.wikipedia.org/wiki/C%C3%A1lculo_lambda#Sem%C3%A1ntica

El que un término llegue a una forma normal o no, y cuanto trabajo debe realizarse para ello si se puede, depende sustancialmente de la estrategia de reducción utilizada. La distinción entre las estrategias de reducción está relacionada con la distinción en lenguajes de programación funcional entre evaluación estricta y evaluación perezosa.

* Reducciones beta completas
    Cualquier redex puede ser reducida en cualquier momento. Esto simboliza la falta de una estrategia particular.

* Orden aplicativo
    Primero se reducen las redexes más profundas y más situadas a la derecha. Intuitivamente, esto significa que los argumentos de una función son siempre reducidos antes que la propia función. El orden aplicativo intenta siempre aplicar funciones a formas normales, incluso cuando esto no es posible.
    La mayoría de lenguajes de programación (incluyendo Lisp, ML y otros lenguajes imperativos como C y Java) se describen como "estrictos", lo que significa que la aplicación de funciones a términos no normalizables es no normalizable. Esto se logra utilizando una reducción por orden aplicativo (call by value, o llamada por valor, aunque usualmente llamada evaluación estricta).

* Orden normal
    La redex más superficial y más a la izquierda se evalúa primero. Por tanto, siempre que sea posible, los argumentos de una abstracción son sustituidos en su cuerpo antes de que los argumentos sean reducidos.

* Llamada por nombre
    Como el orden normal, pero no se realizan reducciones dentro de las abstracciones. Por ejemplo: λx.(λx.x)x estaría en forma normal bajo esta estrategia, aunque contiene la redex (λx.x)x.

* Llamada por valor
    Solo las redexes más superficiales son reducidas: una redex solo se reduce cuando su expresión derecha ha sido reducida a un valor (una variable o una abstracción lambda).

* Llamada por necesidad
    Como el orden normal, pero las aplicaciones de funciones que duplicarían argumentos nombran el argumento en su lugar, que es entonces reducido solo "cuando se le necesita". A menudo llamado "evaluación perezosa", su implementación de "nombre" suele lograrse con un puntero, con la redex representada como un thunk.

El orden aplicativo no es una estrategia de normalización. El ejemplo más típico es el siguiente: se define Q = ωω donde ω = λx.xx. Esta expresión solo contiene una redex (la expresión completa), la cual resulta al ser reducida otra vez en Q. Como es la única reducción posible, Q no tiene forma normal bajo ninguna estrategia de reducción. Utilizando orden aplicativo, la expresión KIΩ = (λx.λy.x) (λx.x)Ω es reducida reduciendo primero Q, pero como no tiene forma normal, esta estrategia fracasa a la hora de encontrar una forma normal para KIQ.

En contraposición, el orden normal siempre encuentra la forma normal si esta existe. En el ejemplo anterior, KIQ es reducido bajo orden normal a I, una forma normal. Uno de los inconvenientes es que las redexes en los argumentos pueden ser copiadas, resultando en trabajo duplicado. En ese caso, el orden aplicativo se encuentra en ventaja, porque nunca sustituye argumentos que contengan redexes, y el trabajo es realizado una única vez.

La mayoría de lenguajes de programación funcionales puros (sobre todo Miranda y sus descendientes, incluyendo Haskell) utilizan evaluación perezosa, que es esencialmente idéntica a la llamada por necesidad. Esta es similar a la reducción por orden normal, pero evita la duplicación de trabajo mediante la representación indirecta de los términos repetidos, abstraída de su posición real y accedida de forma indirecta (y por tanto, varias posiciones pueden compartir el mismo término).



 
