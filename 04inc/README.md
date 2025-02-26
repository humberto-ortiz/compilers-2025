# Incremento

Este directorio contiene el mismo compilador para enteros, con "parser"
y "lexer" que reconozca enteros y `++` y los convierta a `expr`.

*ESTA ROTO*, no le hemos dicho como usar `IAdd`.

```
$ dune build
File "compiler.ml", line 13, characters 2-21:
13 |   | IAdd of arg * arg
       ^^^^^^^^^^^^^^^^^^^
Error (warning 37 [unused-constructor]): constructor IAdd is never used to build values.
(However, this constructor appears in patterns.)
```
