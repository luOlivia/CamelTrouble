MODULES=ball camel cell main maze state test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,graphics


default: build
	utop

build:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
	$(OCAMLBUILD) $(OBJECTS) && js_of_ocaml +graphics.js $(MAIN)
	
test:
	$(OCAMLBUILD) $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

docs: docs-public docs-private

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip