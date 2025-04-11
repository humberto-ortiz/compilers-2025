Diagram of our compiler so far.

```mermaid
flowchart TD
in[/foo.int/]
 -->
lexer[[lexer]] 
 -->
tok[/list of tokens/]
 -->
parser[[parser]]
 -->
expr[/pos expr/]
 -->
tag[[tag]]
 -->
tagged[/tag expr/]
-->
rename[[rename]]
 -->
renamed[/tag expr/]
-->
anf[[anf]]
 -->
aexpr[/aexpr/]
 -->
comp[[compile_aexpr]]
 -->
inst[/instruction list/]
 -->
asm[[asm_to_string]]
 -->
s[/foo.s/]
 -->
nasm[[nasm]]
 -->
obj[/foo.o/]
 -->
gcc[[gcc]]
 -->
run[/foo.run/]

mll[lexer.mll] --> lexer
mly[parser.mly] --> parser
main.c --> gcc
```
