Testing

Se implementó black box testing para LLVM y Bytecompile.
Se probó sobre una batería de casos exitosos (Test/Cases) y una de casos que deberían fallar (Test/Cases/should-fail).

Se tomaron ejemplos que se encontraban en el repositorio de la materia de los distintos años. Se excluyeron los que no eran compatibles con la implementación original (por ejemplo, los que tenían print como comando).

Comandos:

Compilacion Bytecode y ejecucion BVM implementada en Haskell:

`stack test :Bytecode`

Compilacion Bytecode y ejecucion BVM implementada en C:

`stack test :CBytecode`

Compilación y ejecución LLVM:

`stack test :LLVM`
