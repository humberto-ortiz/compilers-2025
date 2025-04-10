# Tarea: Volver a arreglar if.

Este directorio contiene un compilador para enteros, let, operadores
binarios. Le estamos tratando de añadir una expresion condicional `if`
que recibe tres expresiones, la condicion, el then, y el else.
Verifica si la condicion es cierto y corre el then, si es falso corre
el else.

**NOTE** Tenemos un bug!

```
(if 0 1 2)
```
Debe dar un error, porque la condicion `0` no es un Booleano.

## Tarea

Arreglen el compilador para que al correr if-roto.run de un error como
"If expected a Boolean, got 0"

```
$ make if-roto.run
dune exec ./compiler.exe if-roto.int > if-roto.s
nasm -f elf64 -o if-roto.o if-roto.s
gcc -g -m64 -o if-roto.run main.c if-roto.o
rm if-roto.o if-roto.s
$ ./if-roto.run
If expected a Boolean, got 0
```

**HINT** probablemente quieren hacer un `err_not_boolean` y brincar
  alli si detectan un error.
