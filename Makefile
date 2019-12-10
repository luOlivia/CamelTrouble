MODULES=ball resources camel cell main maze position state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,js_of_ocaml-lwt,js_of_ocaml-lwt.graphics,js_of_ocaml-ppx


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
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip