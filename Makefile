linux:
	nasm -f elf src/linux.asm -o linux.o 
	ld -m elf_i386 linux.o -o bin/linux/linux.out 
	rm linux.o
windows:
	nasm -f win32 src\windows.asm -o windows.obj 
	gcc windows.obj -lkernel32 -nostartfiles -o bin\windows\windows.exe
	del windows.obj