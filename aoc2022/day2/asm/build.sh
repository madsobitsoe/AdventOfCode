# Use nasm and ld
#nasm -f elf64 -o day2.o day2.asm && ld day2.o -o day2
nasm -f bin -o day2 day2.asm
chmod +x day2
