.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

gui:
	OCAMLRUNPARAM=b dune exec test/gui/gui_test.exe

simulate:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f simulator.zip
	zip -r simulator.zip . 

clean:
	dune clean
	rm -f simulator.zip