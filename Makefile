.PHONY: all clean install uninstall reinstall check 

all: MLBDD.cmxa MLBDD.cma

# Build Native Library

MLBDD.cmxa : MLBDD.cmx
	ocamlopt -a -inline 100 MLBDD.cmx -o MLBDD.cmxa

# Build Bytecode Library

MLBDD.cma : MLBDD.cmo
	ocamlc -a MLBDD.cmo -o MLBDD.cma


# Build BDD Library

MLBDD.cmo : MLBDD.ml MLBDD.mli MLBDD.cmi
	ocamlc -c MLBDD.ml
MLBDD.cmx : MLBDD.ml MLBDD.mli MLBDD.cmi
	ocamlopt -c -inline 100 MLBDD.ml
MLBDD.cmi : MLBDD.mli
	ocamlopt -c -inline 100 MLBDD.mli

# Clean Up

clean:
	-rm MLBDD.cmxa MLBDD.cma MLBDD.a MLBDD.cmo MLBDD.cmx MLBDD.o MLBDD.cmi
	-ocamlbuild -clean

# Install uses ocamlfind

install:
	ocamlfind install mlbdd META MLBDD.cmxa MLBDD.cma MLBDD.cmi MLBDD.a MLBDD.o MLBDD.cmx MLBDD.cmo

uninstall:
	ocamlfind remove mlbdd

reinstall: uninstall install


# Test requires installation

check:
	ocamlbuild -use-ocamlfind -no-hygiene test.byte
	./test.byte
