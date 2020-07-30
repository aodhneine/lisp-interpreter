.PHONY: all
all: main

main:
	make -C src/
	mv src/main main

.PHONY: clean
clean: main
	rm -f main src/main.cmx src/main.cmi src/main.o
