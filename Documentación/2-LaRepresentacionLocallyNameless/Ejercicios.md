- Ejercicio 1

¿A qué términos fully named corresponden los siguientes términos con índices de De Brujin?

a) \0  ->  \x.x

b) \0\1 0  ->  \x.x(\y.x)x

c) \ \ \2 0 1  -> \x.\y.\z.xzy

d) \ (\0)(\1)  -> \x.(\y.y)(\z.x)

- Ejercicio 2

Expresar los siguientes términos fully named con índices de De Brujin

a) \x.x(\y.y)  ->  \0(\0)

b) \f.\x.fx  ->  \ \10

c) \x.x x  ->  \00

d) \x.\y.(\z.xz) y  ->  \ \ (\20)0

- Ejercicio 3

¿Cuál es el resultado de las siguientes aplicaciones de open y close?

a) open x 0  ->  x

b) open x (\ \2 0 1)  ->  \ \x 0 1

c) open x (\0)  ->  \0

d) open x (0\0 1\1 2)  ->  x \0 x \1 x

e) close x 0  ->  0

f) close x (x (\0 x))  ->  0(\0 1)

g) close x (\ \0 1)  ->  \ \0 1

h) close x \ (close y (\y x))  ->  close x (\1 x)  ->  \1 2