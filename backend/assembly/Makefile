all : full.o
	gcc -lm -o full full.o

full.o : full.asm
	nasm -f elf64 -o full.o full.asm

full.asm : head.asm tmp.asm
	cat head.asm tmp.asm >full.asm

tmp.asm : assembly.rkt
	racket -t assembly.rkt -m ../../test.music >tmp.asm
