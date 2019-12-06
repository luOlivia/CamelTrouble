MODULES=ball camel cell main maze position resources state test utils
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind


default: build
	utop

build:
	$(OCAMLBUILD) $(MAIN) && js_of_ocaml +graphics.js $(MAIN) 
	
test:
	$(OCAMLBUILD) $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

# zip:
# 	zip cameltrouble.zip *.ml* _tags *.txt *.js *.md *.byte Makefile

docs: docs-public docs-private

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip