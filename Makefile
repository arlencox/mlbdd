.PHONY: all clean check 

all:
	dune build

clean:
	dune clean

check:
	dune runtest
