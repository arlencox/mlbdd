.PHONY: all clean install uninstall reinstall check 

OCAMLJAVA := $(shell command -v ocamljava 2> /dev/null)

ifdef OCAMLJAVA
	JAVA_TARGET=MLBDD.cmja
	JAVA_INSTALL=MLBDD.cmja MLBDD.jo MLBDD.ja MLBDD.cmj
else
	JAVA_TARGET=
endif

all: MLBDD.cmxa MLBDD.cma $(JAVA_TARGET)

# Build Native Library

MLBDD.cmxa : MLBDD.cmx
	ocamlopt -a -inline 100 MLBDD.cmx -o MLBDD.cmxa

# Build Bytecode Library

MLBDD.cma : MLBDD.cmo
	ocamlc -a MLBDD.cmo -o MLBDD.cma

# Build JVM Library
MLBDD.cmja : MLBDD.cmj
	ocamljava -a MLBDD.cmj -o MLBDD.cmja

# Build BDD Library

MLBDD.cmo : MLBDD.ml MLBDD.mli MLBDD.cmi
	ocamlc -c -unsafe MLBDD.ml
MLBDD.cmx : MLBDD.ml MLBDD.mli MLBDD.cmi
	ocamlopt -c -inline 100 -unsafe MLBDD.ml
MLBDD.cmi : MLBDD.mli
	ocamlopt -c -inline 100 -unsafe MLBDD.mli
MLBDD.cmj : MLBDD.ml MLBDD.mli MLBDD.cmi
	ocamljava -c -inline 100 -unsafe MLBDD.ml

# Clean Up

clean:
	-rm MLBDD.cmxa MLBDD.cma MLBDD.a MLBDD.cmo MLBDD.cmx MLBDD.o MLBDD.cmi $(JAVA_INSTALL)
	-ocamlbuild -clean

# Install uses ocamlfind

install:
	ocamlfind install mlbdd META MLBDD.cmxa MLBDD.cma MLBDD.cmi MLBDD.a MLBDD.o MLBDD.cmx MLBDD.cmo$ $(JAVA_INSTALL)

uninstall:
	ocamlfind remove mlbdd

reinstall: uninstall install


# Test requires installation

check:
	ocamlbuild -use-ocamlfind -no-hygiene test.byte
	./test.byte
