MODULES=ball camel director main map
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(MAIN) && js_of_ocaml +graphics.js $(MAIN)

docs: docs-public docs-private

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip