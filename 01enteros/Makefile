# Makefile from lecture 2.

%.run: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.int
	ocaml compiler.ml $< > $@
