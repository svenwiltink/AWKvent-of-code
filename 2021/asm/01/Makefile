.PHONY: clean

compile: aoc.nasm strings.nasm
	@mkdir -p out
	@nasm -f elf64 aoc.nasm -o out/aoc.o
	@nasm -f elf64 strings.nasm -o out/strings.o
	@ld out/aoc.o out/strings.o -o out/aoc

clean:
	@rm -rf out
