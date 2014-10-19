SHELL=/bin/bash
.SHELLFLAGS="-O extglob -c"

FLAGS=-use-ocamlfind -cflags -annot

.PHONY: all test coverage clean
all:
	ocamlbuild $(FLAGS) MLBDD.cma MLBDD.cmxa

test:
	ocamlbuild $(FLAGS) test.native
	./test.native

coverage:
	-rm -f bisect*.out
	-rm -rf coverage
	ocamlbuild $(FLAGS) -package bisect -syntax camlp4o test.native
	./test.native
	(cd _build; bisect-report.opt -html ../coverage ../bisect*.out)


clean:
	ocamlbuild -clean

doc:
	ocamlbuild $(FLAGS) MLBDD.docdir/index.html
