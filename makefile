all: masses graphobj points

clean:
	ocamlbuild -clean

graphobj:
	ocamlbuild graphobj.byte

masses:
	ocamlbuild masses.byte

points:
	ocamlbuild points.byte

tests:
	ocamlbuild tests.byte
