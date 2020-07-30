OCAMLC = ocamlopt.opt

.PHONY: all
all: main

main.cmx: main.ml
	$(OCAMLC) -c $<

main: main.cmx
	$(OCAMLC) $^ -o $@
