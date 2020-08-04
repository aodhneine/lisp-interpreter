OCAMLC = ocamlopt.opt

.PHONY: all
all: main

main.cmx: main.ml
	$(OCAMLC) -c main.ml

main: main.cmx
	$(OCAMLC) main.cmx -o main
