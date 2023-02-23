# Compiladores
Código para la materia Compiladores de [LCC](https://dcc.fceia.unr.edu.ar), [FCEIA](https://www.fceia.unr.edu.ar), [UNR](https://www.unr.edu.ar).

Este es el código a partir del cual los estudiantes empiezan a desarrollar un compilador.

Para fijar la versión de GHC y de los paquetes usaremos la herramienta [stack](https://docs.haskellstack.org/en/stable/README/).

Los pasos para instalar son:

```code
stack setup
stack build
```

Luego se puede ejecutar con 
```code
stack run
```
o cargar el entorno interactivo GHCi
```code
stack ghci

stack ghci src/TypeChecker.hs
```


Modos de uso:

* Ayuda:
```code
stack run -- -h

stack run -- --help
```

* Modo interactivo:
```code
stack run
```

* Pretty Print:
```code
stack run -- -p <filenames>

stack run -- --prettyprint <filenames>
```

* Typecheck:
```code
stack run -- -t <filenames>

stack run -- --typecheck <filenames>
```

* Compilación para BVM:
```code
stack run -- -c <filenames>

stack run -- --bytecompile <filenames>
```

* Imprimir resultado luego de Closure Conversion y hoisting:
```code
stack run -- --cc <filenames>
```

* Ejecutar bytecode en la BVM (archivos .byte):
```code
stack run -- -c <filenames>

stack run -- --bytecompile <filenames>
```

* Generar código LLVM:
```code
stack run -- --llvm <filenames>
```

* Generar y ejecutar código LLVM:
```code
stack run -- --runllvm <filenames>
```