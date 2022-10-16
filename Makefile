build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe