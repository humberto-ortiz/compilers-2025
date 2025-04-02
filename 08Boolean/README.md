# If de verdad

Este directorio contiene un compilador para enteros, let, operadores
binarios. Le estamos tratando de añadir una expresion condicional `if` que recibe tres expresiones, la condicion, el then, y el else. Verifica si la condicion es cierto y corre el then, si es falso corre el else.

```
(if false 2 3)
```
Produciria
```
3
```

**NOTE** Tenemos un bug!

```
(if true 1 2)
```
Debe ser `1`, pero da `2`:
```
$ make if-true.run
dune exec ./compiler.exe if-true.int > if-true.s
nasm -f elf64 -o if-true.o if-true.s
gcc -g -m64 -o if-true.run main.c if-true.o
rm if-true.o if-true.s
$ ./if-true.run
2
```
