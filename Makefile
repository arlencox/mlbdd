.PHONY: all clean install uninstall reinstall check 

all: MLBDD.cmxa MLBDD.cma

# Build Native Library

MLBDD.cmxa : WeakHash.cmx MLBDD.cmx
	ocamlopt -a -inline 100 WeakHash.cmx MLBDD.cmx -o MLBDD.cmxa

# Build Bytecode Library

MLBDD.cma : WeakHash.cmo MLBDD.cmo
	ocamlc -a WeakHash.cmo MLBDD.cmo -o MLBDD.cma


# Build BDD Library

MLBDD.cmo : MLBDD.ml MLBDD.mli WeakHash.cmo MLBDD.cmi
	ocamlc -c MLBDD.ml
MLBDD.cmx : MLBDD.ml MLBDD.mli WeakHash.cmx MLBDD.cmi
	ocamlopt -c -inline 100 MLBDD.ml
MLBDD.cmi : MLBDD.mli
	ocamlopt -c -inline 100 MLBDD.mli

# Build Weak Pointer Hash Table

WeakHash.cmo : WeakHash.ml
	ocamlc -c WeakHash.ml
WeakHash.cmx : WeakHash.ml
	ocamlopt -c WeakHash.ml

# Clean Up

clean:
	-rm MLBDD.cmxa MLBDD.cma MLBDD.a MLBDD.cmo MLBDD.cmx MLBDD.o MLBDD.cmi WeakHash.cmo WeakHash.cmx WeakHash.o WeakHash.cmi
	-ocamlbuild -clean

# Install uses ocamlfind

install:
	ocamlfind install mlbdd META MLBDD.cmxa MLBDD.cma MLBDD.cmi MLBDD.a MLBDD.o MLBDD.cmx MLBDD.cmo WeakHash.cmi WeakHash.cmo WeakHash.cmx WeakHash.o

uninstall:
	ocamlfind remove mlbdd

reinstall: uninstall install


# Test requires installation

check:
	ocamlbuild -use-ocamlfind -no-hygiene test.byte
	./test.byte
