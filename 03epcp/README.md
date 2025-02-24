# Enteros, pero con parser

Este directorio contiene el mismo compilador para enteros, pero
modificado para utilizar menhir y ocamllex para construir un "parser"
y "lexer" que reconozca enteros y los convierta a `Int64`.

Pueden construir el compilador con `dune build`, y correrlo con `dune
exec ./compiler.exe` seguido por el nombre del archivo que quieren
compilar:

```{bash}
$ dune exec ./compiler.exe cero.int
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, 0
  ret

```

El Makefile modificado utiliza esta instruccion para compilar programas:

```{bash}
$ make cero.run
Saving digest db...
dune exec ./compiler.exe cero.int > cero.s
Done: 84% (28/33, 5 left) (jobs: 0)
nasm -f elf64 -o cero.o cero.s
clang -g -m64 -o cero.run main.c cero.o
rm cero.s cero.o
$ ./cero.run
0
```
