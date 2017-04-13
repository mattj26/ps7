all: masses graphobj points

clean:
	ocamlbuild -clean

graphobj:
	ocamlbuild graphobj.byte

masses:
	ocamlbuild masses.byte

mlbteams:
	ocamlbuild mlb_team_graph.byte

points:
	ocamlbuild points.byte

testgraphs:
	ocamlbuild testgraphs.byte

tests:
	ocamlbuild tests.byte
