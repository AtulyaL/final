build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

cloc:
	cloc --by-file --include-lang=OCaml .